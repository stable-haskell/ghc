{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1994-1998

Core-syntax unfoldings

Unfoldings (which can travel across module boundaries) are in Core
syntax (namely @CoreExpr@s).

The type @Unfolding@ sits ``above'' simply-Core-expressions
unfoldings, capturing ``higher-level'' things we know about a binding,
usually things that the simplifier found out (e.g., ``it's a
literal'').  In the corner of a @CoreUnfolding@ unfolding, you will
find, unsurprisingly, a Core expression.
-}



module GHC.Core.Unfold (
        Unfolding, UnfoldingGuidance,   -- Abstract types

        ExprTree, exprTree, exprTreeSize, altTreesSize,
        exprTreeWillInline, couldBeSmallEnoughToInline,
        ArgSummary(..), hasArgInfo,

        Size, InlineContext(..),

        UnfoldingOpts (..), defaultUnfoldingOpts,
        updateCreationThreshold, updateUseThreshold,
        updateFunAppDiscount, updateDictDiscount,
        updateVeryAggressive, updateCaseScaling,
        updateCaseThreshold, updateReportPrefix,

        inlineBoringOk, calcUnfoldingGuidance
    ) where

import GHC.Prelude

import GHC.Core
import GHC.Core.Utils
import GHC.Core.DataCon
import GHC.Core.Type

import GHC.Types.Id
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Types.Literal
import GHC.Types.Id.Info
import GHC.Types.RepType ( isZeroBitTy )
import GHC.Types.Basic  ( Arity )
import GHC.Types.ForeignCall
import GHC.Types.Tickish

import GHC.Builtin.Names
import GHC.Builtin.PrimOps

import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic

import GHC.Data.Bag
import GHC.Data.Maybe

import qualified Data.ByteString as BS


{- Note [Overview of inlining heuristics]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Key examples
------------
Example 1:

   let f x = case x of
               A -> True
               B -> <big>
   in ...(f A)....(f B)...

Even though f's entire RHS is big, it collapses to something small when applied
to A.  We'd like to spot this.

Example 1:

   let f x = case x of
               (p,q) -> case p of
                           A -> True
                           B -> <big>
   in ...(f (A,3))....

This is similar to Example 1, but nested.

Example 3:

   let j x = case y of
               A -> True
               B -> <big>
   in case y of
         A -> ..(j 3)...(j 4)....
         B -> ...

Here we want to spot that although the free far `y` is unknown at j's definition
site, we know that y=A at the two calls in the A-alternative of the body. If `y`
had been an argument we'd have spotted this; we'd like to get the same goodness
when `y` is a free variable.

This kind of thing can occur a lot with join points.

Design overview
---------------
The question is whethe or not to inline f = rhs.
The key idea is to abstract `rhs` to an ExprTree, which gives a measure of
size, but records structure for case-expressions.


The moving parts
-----------------
* An unfolding is accompanied (in its UnfoldingGuidance) with its GHC.Core.ExprTree,
  computed by GHC.Core.Unfold.exprTree.

* At a call site, GHC.Core.Opt.Simplify.Inline.contArgs constructs an ArgSummary
  for each value argument. This reflects any nested data construtors.

* Then GHC.Core.Unfold.exprTreeSize takes information about the context of the
  call (particularly the ArgSummary for each argument) and computes a final size
  for the inlined body, taking account of case-of-known-consructor.

-}

{- *********************************************************************
*                                                                      *
                     UnfoldingOpts
*                                                                      *
********************************************************************* -}

-- | Unfolding options
data UnfoldingOpts = UnfoldingOpts
   { unfoldingCreationThreshold :: !Size
      -- ^ Threshold above which unfoldings are not *created*

   , unfoldingUseThreshold :: !Size
      -- ^ Threshold above which unfoldings are not *inlined*

   , unfoldingFunAppDiscount :: !Discount
      -- ^ Discount for lambdas that are used (applied)

   , unfoldingDictDiscount :: !Discount
      -- ^ Discount for dictionaries

   , unfoldingVeryAggressive :: !Bool
      -- ^ Force inlining in many more cases

   , unfoldingCaseThreshold :: !Size
      -- ^ Don't consider depth up to x

   , unfoldingCaseScaling :: !Int
      -- ^ Penalize depth with 1/x

   , exprTreeCaseWidth :: !Int
      -- ^ Bale out entirely with a case width greater than this

   , exprTreeCaseDepth :: !Int
      -- ^ Don't make ExprTrees with a case depth greater than this

   , unfoldingReportPrefix :: !(Maybe String)
      -- ^ Only report inlining decisions for names with this prefix
   }

defaultUnfoldingOpts :: UnfoldingOpts
defaultUnfoldingOpts = UnfoldingOpts
   { unfoldingCreationThreshold = 750
      -- The unfoldingCreationThreshold threshold must be reasonably high
      -- to take account of possible discounts.
      -- E.g. 450 is not enough in 'fulsom' for Interval.sqr to
      -- inline into Csg.calc (The unfolding for sqr never makes it
      -- into the interface file.)

   , unfoldingUseThreshold   = 90
      -- Last adjusted upwards in #18282, when I reduced
      -- the result discount for constructors.

   , unfoldingFunAppDiscount = 60
      -- Be fairly keen to inline a function if that means
      -- we'll be able to pick the right method from a dictionary

   , unfoldingDictDiscount   = 30
      -- Be fairly keen to inline a function if that means
      -- we'll be able to pick the right method from a dictionary

   , unfoldingVeryAggressive = False

      -- Only apply scaling once we are deeper than threshold cases
      -- in an RHS.
   , unfoldingCaseThreshold = 2

      -- Penalize depth with (size*depth)/scaling
   , unfoldingCaseScaling = 30

      -- Don't filter inlining decision reports
   , unfoldingReportPrefix = Nothing

     -- Bale out at exprTreeCaseWidth
     -- See Note [Bale out on very wide case expressions]
   , exprTreeCaseWidth = 20

     -- Don't record CaseOf beyond exprTreeCaseDepth
   , exprTreeCaseDepth = 4
   }

-- Helpers for "GHC.Driver.Session"

updateCreationThreshold :: Size -> UnfoldingOpts -> UnfoldingOpts
updateCreationThreshold n opts = opts { unfoldingCreationThreshold = n }

updateUseThreshold :: Size -> UnfoldingOpts -> UnfoldingOpts
updateUseThreshold n opts = opts { unfoldingUseThreshold = n }

updateFunAppDiscount :: Discount -> UnfoldingOpts -> UnfoldingOpts
updateFunAppDiscount n opts = opts { unfoldingFunAppDiscount = n }

updateDictDiscount :: Discount -> UnfoldingOpts -> UnfoldingOpts
updateDictDiscount n opts = opts { unfoldingDictDiscount = n }

updateVeryAggressive :: Bool -> UnfoldingOpts -> UnfoldingOpts
updateVeryAggressive n opts = opts { unfoldingVeryAggressive = n }


updateCaseThreshold :: Size -> UnfoldingOpts -> UnfoldingOpts
updateCaseThreshold n opts = opts { unfoldingCaseThreshold = n }

updateCaseScaling :: Int -> UnfoldingOpts -> UnfoldingOpts
updateCaseScaling n opts = opts { unfoldingCaseScaling = n }

updateReportPrefix :: Maybe String -> UnfoldingOpts -> UnfoldingOpts
updateReportPrefix n opts = opts { unfoldingReportPrefix = n }


{-
************************************************************************
*                                                                      *
\subsection{The UnfoldingGuidance type}
*                                                                      *
************************************************************************
-}

inlineBoringOk :: CoreExpr -> Bool
-- See Note [INLINE for small functions]
-- True => the result of inlining the expression is
--         no bigger than the expression itself
--     eg      (\x y -> f y x)
-- This is a quick and dirty version. It doesn't attempt
-- to deal with  (\x y z -> x (y z))
-- The really important one is (x `cast` c)
inlineBoringOk e
  = go 0 e
  where
    go :: Int -> CoreExpr -> Bool
    go credit (Lam x e) | isId x           = go (credit+1) e
                        | otherwise        = go credit e
        -- See Note [Count coercion arguments in boring contexts]
    go credit (App f (Type {}))            = go credit f
    go credit (App f a) | credit > 0
                        , exprIsTrivial a  = go (credit-1) f
    go credit (Tick _ e)                   = go credit e -- dubious
    go credit (Cast e _)                   = go credit e
    go credit (Case e b _ alts)
      | null alts
      = go credit e   -- EmptyCase is like e
      | Just rhs <- isUnsafeEqualityCase e b alts
      = go credit rhs -- See Note [Inline unsafeCoerce]
    go _      (Var {})                     = boringCxtOk
    go _      (Lit l)                      = litIsTrivial l && boringCxtOk
    go _      _                            = boringCxtNotOk

calcUnfoldingGuidance
        :: UnfoldingOpts
        -> Bool          -- Definitely a top-level, bottoming binding
        -> CoreExpr      -- Expression to look at
        -> UnfoldingGuidance
calcUnfoldingGuidance opts is_top_bottoming (Tick t expr)
  | not (tickishIsCode t)  -- non-code ticks don't matter for unfolding
  = calcUnfoldingGuidance opts is_top_bottoming expr
calcUnfoldingGuidance opts is_top_bottoming expr
  = case exprTree opts val_bndrs body of
      Nothing -> UnfNever
      Just et@(ExprTree { et_wc_tot = tot })
        | uncondInline expr n_val_bndrs tot
        -> UnfWhen { ug_unsat_ok  = unSaturatedOk
                   , ug_boring_ok =  boringCxtOk
                   , ug_arity     = n_val_bndrs }   -- Note [INLINE for small functions]

        | is_top_bottoming
        -> UnfNever   -- See Note [Do not inline top-level bottoming functions]

        | otherwise
        -> UnfIfGoodArgs { ug_args = val_bndrs, ug_tree = et }

  where
    (bndrs, body) = collectBinders expr
    val_bndrs   = filter isId bndrs
    n_val_bndrs = length val_bndrs

couldBeSmallEnoughToInline :: UnfoldingOpts -> Size -> CoreExpr -> Bool
-- We use 'couldBeSmallEnoughToInline' to avoid exporting inlinings that
-- we ``couldn't possibly use'' on the other side.  Can be overridden
-- w/flaggery.  Just the same as smallEnoughToInline, except that it has no
-- actual arguments.
couldBeSmallEnoughToInline opts threshold rhs
  = isJust (exprTree opts' [] body)
  where
    opts' = opts { unfoldingCreationThreshold = threshold }
            -- We use a different (and larger) theshold here for
            -- creating specialised copies of the function
    (_, body) = collectBinders rhs

{- Note [Inline unsafeCoerce]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We really want to inline unsafeCoerce, even when applied to boring
arguments.  It doesn't look as if its RHS is smaller than the call
   unsafeCoerce x = case unsafeEqualityProof @a @b of UnsafeRefl -> x
but that case is discarded in CoreToStg -- see Note [Implementing unsafeCoerce]
in base:Unsafe.Coerce.

Moreover, if we /don't/ inline it, we may be left with
          f (unsafeCoerce x)
which will build a thunk -- bad, bad, bad.

Conclusion: we really want inlineBoringOk to be True of the RHS of
unsafeCoerce. And it really is, because we regard
  case unsafeEqualityProof @a @b of UnsafeRefl -> rhs
as trivial iff rhs is. This is (U4) in Note [Implementing unsafeCoerce].

Note [Computing the size of an expression]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The basic idea of exprSizeTree is obvious enough: count nodes.  But getting the
heuristics right has taken a long time.  Here's the basic strategy:

    * Variables, literals: 0
      (Exception for string literals, see litSize.)

    * Function applications (f e1 .. en): 1 + #value args

    * Constructor applications: 1, regardless of #args

    * Let(rec): 1 + size of components

    * Note, cast: 0

Examples

  Size  Term
  --------------
    0     42#
    0     x
    0     True
    2     f x
    1     Just x
    4     f (g x)

Notice that 'x' counts 0, while (f x) counts 2.  That's deliberate: there's
a function call to account for.  Notice also that constructor applications
are very cheap, because exposing them to a caller is so valuable.

[25/5/11] All sizes are now multiplied by 10, except for primops
(which have sizes like 1 or 4.  This makes primops look fantastically
cheap, and seems to be almost universally beneficial.  Done partly as a
result of #4978.

Note [Do not inline top-level bottoming functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The FloatOut pass has gone to some trouble to float out calls to 'error'
and similar friends.  See Note [Bottoming floats] in GHC.Core.Opt.SetLevels.
Do not re-inline them!  But we *do* still inline if they are very small
(the uncondInline stuff).

Note [INLINE for small functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider        {-# INLINE f #-}
                f x = Just x
                g y = f y
Then f's RHS is no larger than its LHS, so we should inline it into
even the most boring context.  In general, f the function is
sufficiently small that its body is as small as the call itself, the
inline unconditionally, regardless of how boring the context is.

Things to note:

(1) We inline *unconditionally* if inlined thing is smaller (using exprSizeTree)
    than the thing it's replacing.  Notice that
      (f x) --> (g 3)             -- YES, unconditionally
      (f x) --> x : []            -- YES, *even though* there are two
                                  --      arguments to the cons
      x     --> g 3               -- NO
      x     --> Just v            -- NO

    It's very important not to unconditionally replace a variable by
    a non-atomic term.

(2) We do this even if the thing isn't saturated, else we end up with the
    silly situation that
       f x y = x
       ...map (f 3)...
    doesn't inline.  Even in a boring context, inlining without being
    saturated will give a lambda instead of a PAP, and will be more
    efficient at runtime.

(3) However, when the function's arity > 0, we do insist that it
    has at least one value argument at the call site.  (This check is
    made in the UnfWhen case of callSiteInline.) Otherwise we find this:
         f = /\a \x:a. x
         d = /\b. MkD (f b)
    If we inline f here we get
         d = /\b. MkD (\x:b. x)
    and then prepareRhs floats out the argument, abstracting the type
    variables, so we end up with the original again!

(4) We must be much more cautious about arity-zero things. Consider
       let x = y +# z in ...
    In *size* terms primops look very small, because the generate a
    single instruction, but we do not want to unconditionally replace
    every occurrence of x with (y +# z).  So we only do the
    unconditional-inline thing for *trivial* expressions.

    NB: you might think that PostInlineUnconditionally would do this
    but it doesn't fire for top-level things; see GHC.Core.Opt.Simplify.Utils
    Note [Top level and postInlineUnconditionally]

Note [Count coercion arguments in boring contexts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In inlineBoringOK, we ignore type arguments when deciding whether an
expression is okay to inline into boring contexts. This is good, since
if we have a definition like

  let y = x @Int in f y y

there’s no reason not to inline y at both use sites — no work is
actually duplicated. It may seem like the same reasoning applies to
coercion arguments, and indeed, in #17182 we changed inlineBoringOK to
treat coercions the same way.

However, this isn’t a good idea: unlike type arguments, which have
no runtime representation, coercion arguments *do* have a runtime
representation (albeit the zero-width VoidRep, see Note [Coercion tokens]
in "GHC.CoreToStg"). This caused trouble in #17787 for DataCon wrappers for
nullary GADT constructors: the wrappers would be inlined and each use of
the constructor would lead to a separate allocation instead of just
sharing the wrapper closure.

The solution: don’t ignore coercion arguments after all.
-}

uncondInline :: CoreExpr -> Arity -> Size -> Bool
-- Inline unconditionally if there no size increase
-- Size of call is arity (+1 for the function)
-- See Note [INLINE for small functions]
uncondInline rhs arity size
  | arity > 0 = size <= 10 * (arity + 1) -- See Note [INLINE for small functions] (1)
  | otherwise = exprIsTrivial rhs        -- See Note [INLINE for small functions] (4)



{- *********************************************************************
*                                                                      *
            From CoreExpr to ExprTree
     This is used when we make the unfolding for a function
*                                                                      *
********************************************************************* -}

{- Note [Constructing an ExprTree]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We maintain:
* avs: argument variables, or variables bound by a case on an
       argument variable.

  We record a CaseOf or ScrutOf for the `avs`

* lvs: variables bound by lambda and lets in the body; and by
       case expressions that scrutinise one of the `lvs`, or
       a non-variable.

  We never record a CaseOf or ScrutOf for one of the `lvs`.

* We record a CaseOf, but not ScrutOf, for other variables; that is,
  variables free in the entire function definition.  For example:
        let  f x = case y of
                     A -> True
                     B -> <big>
        in
        case y of
          A -> ....f 3....f 4....
          B -> blah
  At the calls site of `f` we know that the free var `y` is equal to A, so
  f should definitely inline.

  But consider instead this example
        let f x = y x 3 <big>
        in  ...(f 3)...
  There nothing we will learn about the free `y` that will make the inining of
  `f` more attractive.  Hence we don't record ScrutOf for y.

  This is IMPORTANT, because even a call like (reverse xs) would otherwise record
  a ScrutOf for `reverse` which is very silly.

Wrinkles:

* We must be careful about recording enormous functions, with very wide or very
  deep case trees. (This can happen with Generics; e.g. test T5642.)  We limit
  both with UnfoldingOpts.
-}

type ETVars = (VarSet,VarSet)  -- (avs, lvs)
              -- See Note [Constructing an ExprTree]

exprTree :: UnfoldingOpts -> [Var] -> CoreExpr -> Maybe ExprTree
-- Nothing => too big
-- Note [Computing the size of an expression]

exprTree opts args expr
  = go (exprTreeCaseDepth opts) (mkVarSet args, emptyVarSet) expr
  where
    !max_width     = exprTreeCaseWidth opts
    !bOMB_OUT_SIZE = unfoldingCreationThreshold opts
       -- Bomb out if size gets bigger than this
       -- Forcing bOMB_OUT_SIZE early prevents repeated
       -- unboxing of the Int argument.

    et_add     = metAdd bOMB_OUT_SIZE
    et_add_alt = metAddAlt bOMB_OUT_SIZE

    go :: Int -> ETVars -> CoreExpr -> Maybe ExprTree
          -- rcd is the /unused/ case depth; decreases toward zero
          -- (avs,lvs): see Note [Constructing an ExprTree]
    go rcd vs (Cast e _)      = go rcd vs e
    go rcd vs (Tick _ e)      = go rcd vs e
    go _   _  (Type _)        = Just (exprTreeN 0)
    go _   _  (Coercion _)    = Just (exprTreeN 0)
    go _   _  (Lit lit)       = Just (exprTreeN (litSize lit))
    go rcd vs (Case e b _ as) = go_case rcd vs e b as
    go rcd vs (Let bind body) = go_let rcd vs bind body
    go rcd vs (Lam b e)       = go_lam rcd vs b e
    go rcd vs e@(App {})      = go_app rcd vs e
    go _   vs (Var f)         = Just (callTree opts vs f [] 0)
                                -- Use callTree to ensure we get constructor
                                -- discounts even on nullary constructors

    ----------- Lambdas ------------------
    go_lam rcd vs bndr body
      | isId bndr, not (isZeroBitId bndr) = go rcd vs' body `et_add` Just (lamSize opts)
      | otherwise                         = go rcd vs' body
      where
        vs' = vs `add_lv` bndr

    ----------- Applications ------------------
    go_app rcd vs e = lgo e [] 0
      where
         lgo :: CoreExpr -> [CoreExpr] -> Int -> Maybe ExprTree
             -- args:  all the value args
             -- voids: counts the zero-bit arguments; don't charge for these
             --        This makes a difference in ST-heavy code which does a lot
             --        of state passing, and which can be in an inner loop.
         lgo (App fun arg) args voids
                    | isTypeArg arg    = lgo fun args voids
                    | isZeroBitArg arg = lgo fun (arg:args) (voids+1)
                    | otherwise        = go rcd vs arg `et_add`
                                         lgo fun (arg:args) voids
         lgo (Var fun)     args voids  = Just (callTree opts vs fun args voids)
         lgo (Tick _ expr) args voids  = lgo expr args voids
         lgo (Cast expr _) args voids  = lgo expr args voids
         lgo other         args voids  = vanillaCallSize (length args) voids
                                         `metAddN` go rcd vs other
         -- if the lhs is not an App or a Var, or an invisible thing like a
         -- Tick or Cast, then we should charge for a complete call plus the
         -- size of the lhs itself.

    ----------- Let-expressions ------------------
    go_let rcd vs (NonRec binder rhs) body
      = go_bind rcd vs (binder, rhs)  `et_add`
        go rcd (vs `add_lv` binder) body

    go_let rcd vs (Rec pairs) body
      = foldr (et_add . go_bind rcd vs') (go rcd vs' body) pairs
      where
        vs' = vs `add_lvs` map fst pairs

    go_bind rcd vs (bndr, rhs)
      | JoinPoint join_arity <- idJoinPointHood bndr
      , (bndrs, body) <- collectNBinders join_arity rhs
                          -- Skip arguments to join point
      = go rcd (vs `add_lvs` bndrs) body
      | otherwise
      = size_up_alloc bndr `metAddN` go rcd vs rhs

    -- Cost to allocate binding with given binder
    size_up_alloc bndr
      |  isTyVar bndr                    -- Doesn't exist at runtime
      || isJoinId bndr                   -- Not allocated at all
      || not (isBoxedType (idType bndr)) -- Doesn't live in heap
      = 0
      | otherwise
      = 10

    -----------Case expressions ------------------
    go_case :: Int -> ETVars -> CoreExpr -> Id -> [CoreAlt] -> Maybe ExprTree
    -- Empty case
    go_case rcd vs scrut _ [] = go rcd vs scrut
         -- case e of {} never returns, so take size of scrutinee

    -- Record a CaseOf
    go_case remaining_case_depth vs@(avs,lvs) scrut b alts
      | alts `lengthExceeds` max_width
      = Nothing   -- See Note [Bale out on very wide case expressions]

      | Just v <- interestingVarScrut vs scrut
      = go remaining_case_depth vs scrut `et_add`
        (if   remaining_case_depth > 0
         then do { alts' <- mapM (alt_alt_tree v) alts
                 ; etCaseOf bOMB_OUT_SIZE v b alts' }
         else Just (etScrutOf v caseElimDiscount) `et_add`
              -- When this scrutinee has structure, we expect to eliminate the case
              go_alts remaining_case_depth vs b alts)
      where
        rcd1 = remaining_case_depth - 1

        alt_alt_tree :: Id -> Alt Var -> Maybe AltTree
        alt_alt_tree v (Alt con bs rhs)
          = do { rhs <- go rcd1 (add_alt_bndrs v val_bs) rhs
               ; return (AltTree con val_bs rhs) }
          where
            val_bs = filter isId bs

        add_alt_bndrs v bs
          | v `elemVarSet` avs = (avs `extendVarSetList` (b:bs), lvs)
                                 -- Don't forget to add the case binder, b
          | otherwise = vs

    -- Don't record a CaseOf
    go_case rcd vs scrut b alts    -- alts is non-empty
      = caseSize scrut alts     `metAddN`   -- A bit odd that this is only in one branch
        (altSize * length alts) `metAddN`
        go rcd vs scrut `et_add` go_alts (rcd-1) vs b alts

    go_alts :: Int -> ETVars -> Id -> [CoreAlt] -> Maybe ExprTree
    -- Add up the sizes of all RHSs.
    -- IMPORTANT: charge `altSize` for each alternative, else we
    -- find that giant case nests are treated as practically free
    -- A good example is Foreign.C.Error.errnoToIOError
    go_alts rcd vs b alts = foldr1 et_add_alt (map alt_expr_tree alts)
      where
        alt_expr_tree :: Alt Var -> Maybe ExprTree
        alt_expr_tree (Alt _con bs rhs) = altSize `metAddN`
                                          go rcd (vs `add_lvs` (b:bs)) rhs
            -- Don't charge for bndrs, so that wrappers look cheap
            -- (See comments about wrappers with Case)
            -- Don't forget to add the case binder, b, to lvs.

caseSize :: CoreExpr -> [CoreAlt] -> Size
caseSize scrut alts
  | is_inline_scrut scrut, lengthAtMost alts 1 = -10
  | otherwise                                  = 0
              -- Normally we don't charge for the case itself, but
              -- we charge one per alternative (see size_up_alt,
              -- below) to account for the cost of the info table
              -- and comparisons.
              --
              -- However, in certain cases (see is_inline_scrut
              -- below), no code is generated for the case unless
              -- there are multiple alts.  In these cases we
              -- subtract one, making the first alt free.
              -- e.g. case x# +# y# of _ -> ...   should cost 1
              --      case touch# x# of _ -> ...  should cost 0
              -- (see #4978)
              --
              -- I would like to not have the "lengthAtMost alts 1"
              -- condition above, but without that some programs got worse
              -- (spectral/hartel/event and spectral/para).  I don't fully
              -- understand why. (SDM 24/5/11)

              -- Unboxed variables, inline primops and unsafe foreign calls
              -- are all "inline" things:
  where
    is_inline_scrut (Var v) =
      isUnliftedType (idType v)
        -- isUnliftedType is OK here: scrutinees have a fixed RuntimeRep (search for FRRCase)
    is_inline_scrut scrut
        | (Var f, _) <- collectArgs scrut
          = case idDetails f of
              FCallId fc    -> not (isSafeForeignCall fc)
              PrimOpId op _ -> not (primOpOutOfLine op)
              _other        -> False
        | otherwise
          = False

add_lv :: ETVars -> Var -> ETVars
add_lv (avs,lvs) b = (avs, lvs `extendVarSet` b)

add_lvs :: ETVars -> [Var] -> ETVars
add_lvs (avs,lvs) bs = (avs, lvs `extendVarSetList` bs)

interestingVarScrut :: ETVars -> CoreExpr -> Maybe Id
-- The scrutinee of a case is worth recording
interestingVarScrut (_,lvs) (Var v)
     | v `elemVarSet` lvs  = Nothing
     | otherwise           = Just v
interestingVarScrut vs (Tick _ e) = interestingVarScrut vs e
interestingVarScrut vs (Cast e _) = interestingVarScrut vs e
interestingVarScrut _     _       = Nothing

isZeroBitArg :: CoreExpr -> Bool
-- We could take ticks and casts into account, but it makes little
-- difference, and avoiding a recursive function here is good.
isZeroBitArg (Var id) = isZeroBitId id
isZeroBitArg _        = False

isZeroBitId :: Id -> Bool
-- Don't count expressions such as State# RealWorld
isZeroBitId id = assertPpr (not (isJoinId id)) (ppr id) $
                   -- Exclude join points, because they can be rep-polymorphic
                   -- and typePrimRep will crash
                 isZeroBitTy (idType id)


-- | Finds a nominal size of a string literal.
litSize :: Literal -> Size
-- Used by GHC.Core.Unfold.exprTree
litSize (LitNumber LitNumBigNat _)  = 100
litSize (LitString str) = 10 + 10 * ((BS.length str + 3) `div` 4)
        -- If size could be 0 then @f "x"@ might be too small
        -- [Sept03: make literal strings a bit bigger to avoid fruitless
        --  duplication of little strings]
litSize _other = 0    -- Must match size of nullary constructors
                      -- Key point: if  x |-> 4, then x must inline unconditionally
                      --            (eg via case binding)

----------------------------
callTree :: UnfoldingOpts -> ETVars -> Id -> [CoreExpr] -> Int -> ExprTree
callTree opts vs fun val_args voids
  = case idDetails fun of
      FCallId _        -> exprTreeN (vanillaCallSize n_val_args voids)
      JoinId {}        -> exprTreeN (jumpSize        n_val_args voids)
      PrimOpId op _    -> exprTreeN (primOpSize op   n_val_args)
      DataConWorkId dc -> conSize dc n_val_args
      ClassOpId {}     -> classOpSize opts vs fun val_args voids
      _                -> funSize opts vs fun n_val_args voids
  where
    n_val_args = length val_args

-- | The size of a function call
vanillaCallSize :: Int -> Int -> Size
vanillaCallSize n_val_args voids = 10 * (1 + n_val_args - voids)
        -- The 1+ is for the function itself
        -- Add 1 for each non-trivial value arg

-- | The size of a jump to a join point
jumpSize :: Int -> Int -> Size
jumpSize n_val_args voids = 2 * (1 + n_val_args - voids)
  -- A jump is 20% the size of a function call. Making jumps free reopens
  -- bug #6048, but making them any more expensive loses a 21% improvement in
  -- spectral/puzzle. TODO Perhaps adjusting the default threshold would be a
  -- better solution?

classOpSize :: UnfoldingOpts -> ETVars -> Id -> [CoreExpr] -> Int -> ExprTree
-- See Note [Conlike is interesting]
classOpSize _ _ _ [] _
  = etZero
classOpSize opts vs fn val_args voids
  | arg1 : _ <- val_args
  , Just dict <- interestingVarScrut vs arg1
  = warnPprTrace (not (isId dict)) "classOpSize" (ppr fn <+> ppr val_args) $
    vanillaCallSize (length val_args) voids `etAddN`
    etScrutOf dict (unfoldingDictDiscount opts)
           -- If the class op is scrutinising a lambda bound dictionary then
           -- give it a discount, to encourage the inlining of this function
           -- The actual discount is rather arbitrarily chosen
n  | otherwise
  = exprTreeN (vanillaCallSize (length val_args) voids)

funSize :: UnfoldingOpts -> ETVars -> Id -> Int -> Int -> ExprTree
-- Size for function calls that are not constructors or primops
-- Note [Function applications]
funSize opts (avs,_) fun n_val_args voids
  | fun `hasKey` buildIdKey   = etZero  -- Wwant to inline applications of build/augment
  | fun `hasKey` augmentIdKey = etZero  -- so we give size zero to the whole call
  | otherwise = ExprTree { et_wc_tot = size, et_size  = size
                         , et_cases = cases
                         , et_ret   = res_discount }
  where
    size | n_val_args == 0 = 0    -- Naked variable counts zero
         | otherwise       = vanillaCallSize n_val_args voids

    -- Discount if this is an interesting variable, and is applied
    -- If the function is an argument and is applied to some values,
    -- give it a discount -- maybe we can apply that lambda.
    --  See Note [Function and non-function discounts]
    cases | n_val_args > 0, fun `elemVarSet` avs
          = unitBag (ScrutOf fun (unfoldingFunAppDiscount opts))
          | otherwise
          = emptyBag

    res_discount | idArity fun > n_val_args = unfoldingFunAppDiscount opts
                 | otherwise                = 0
        -- If the function is partially applied, show a result discount

lamSize :: UnfoldingOpts -> ExprTree
-- Does not include the size of the body, just the lambda itself
lamSize opts = ExprTree { et_size = 10, et_wc_tot = 10
                        , et_cases = emptyBag
                        , et_ret = unfoldingFunAppDiscount opts }

conSize :: DataCon -> Int -> ExprTree
-- Does not need to include the size of the arguments themselves
conSize dc n_val_args
  | isUnboxedTupleDataCon dc
  = etZero     -- See Note [Unboxed tuple size and result discount]
  | otherwise  -- See Note [Constructor size and result discount]
  = ExprTree { et_size = size, et_wc_tot = size
             , et_cases = emptyBag, et_ret = 10 }
  where
    size | n_val_args == 0 = 0  -- Like variables
         | otherwise       = 10

primOpSize :: PrimOp -> Int -> Size
primOpSize op n_val_args
  | primOpOutOfLine op = op_size + n_val_args
  | otherwise          = op_size
 where
   op_size = primOpCodeSize op

altSize :: Size
-- We charge `altSize` for each alternative in a case
altSize = 10

caseElimDiscount :: Discount
-- Bonus for eliminating a case
caseElimDiscount = 10

{- Note [Bale out on very wide case expressions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
With very wide case trees (say N) we get a size N*altSize, which usually
prevents inlining (e.g. 20*altSize = 200 currently, which is way above the
inlining thresold of 90-ish).  Plus, we risk getting big CaseOf trees in the
ExprTree.

If we aren't going to inline it anyway, then retaining the unfolding in an
interface file is plain silly; T5642 (involving Generics) is a good example.
We had a very wide case whose branches mentioned dozens of data structures,
each of which had very large types.

Of course, if we apply such a function to a data constructor, we could in
principle get a huge discount (because all but one branches fall away).
So perhaps we could use a different setting
* when generating an unfolding /within a module/
* when generating an unfoldign /for an interface file/

Currently we aren't doing this, but we could consider it.

Note [Constructor size and result discount]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Treat a constructors application as size 10, regardless of how many
arguments it has; we are keen to expose them (and we charge separately
for their args).  We can't treat them as size zero, else we find that
(Just x) has size 0, which is the same as a lone variable; and hence
'v' will always be replaced by (Just x), where v is bound to Just x.

The "result discount" is applied if the result of the call is
scrutinised (say by a case).  For a constructor application that will
mean the constructor application will disappear, so we don't need to
charge it to the function.  So the discount should at least match the
cost of the constructor application, namely 10.

Historical note 1: Until Jun 2020 we gave it a "bit of extra
incentive" via a discount of 10*(1 + n_val_args), but that was FAR too
much (#18282).  In particular, consider a huge case tree like

   let r = case y1 of
          Nothing -> B1 a b c
          Just v1 -> case y2 of
                      Nothing -> B1 c b a
                      Just v2 -> ...

If conSize gives a cost of 10 (regardless of n_val_args) and a
discount of 10, that'll make each alternative RHS cost zero.  We
charge 10 for each case alternative (see size_up_alt).  If we give a
bigger discount (say 20) in conSize, we'll make the case expression
cost *nothing*, and that can make a huge case tree cost nothing. This
leads to massive, sometimes exponential inlinings (#18282).  In short,
don't give a discount that give a negative size to a sub-expression!

Historical note 2: Much longer ago, Simon M tried a MUCH bigger
discount: (10 * (10 + n_val_args)), and said it was an "unambiguous
win", but its terribly dangerous because a function with many many
case branches, each finishing with a constructor, can have an
arbitrarily large discount.  This led to terrible code bloat: see #6099.

Note [Unboxed tuple size and result discount]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
However, unboxed tuples count as size zero. I found occasions where we had
        f x y z = case op# x y z of { s -> (# s, () #) }
and f wasn't getting inlined.

I tried giving unboxed tuples a *result discount* of zero (see the
commented-out line).  Why?  When returned as a result they do not
allocate, so maybe we don't want to charge so much for them. If you
have a non-zero discount here, we find that workers often get inlined
back into wrappers, because it look like
    f x = case $wf x of (# a,b #) -> (a,b)
and we are keener because of the case.  However while this change
shrank binary sizes by 0.5% it also made spectral/boyer allocate 5%
more. All other changes were very small. So it's not a big deal but I
didn't adopt the idea.

When fixing #18282 (see Note [Constructor size and result discount])
I changed the result discount to be just 10, not 10*(1+n_val_args).

Note [Function and non-function discounts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want a discount if the function is applied. A good example is
monadic combinators with continuation arguments, where inlining is
quite important.

But we don't want a big discount when a function is called many times
(see the detailed comments with #6048) because if the function is
big it won't be inlined at its many call sites and no benefit results.
Indeed, we can get exponentially big inlinings this way; that is what
#6048 is about.

On the other hand, for data-valued arguments, if there are lots of
case expressions in the body, each one will get smaller if we apply
the function to a constructor application, so we *want* a big discount
if the argument is scrutinised by many case expressions.

Conclusion:
  - For functions, take the max of the discounts
  - For data values, take the sum of the discounts


Note [Literal integer size]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Literal integers *can* be big (mkInteger [...coefficients...]), but
need not be (IS n).  We just use an arbitrary big-ish constant here
so that, in particular, we don't inline top-level defns like
   n = IS 5
There's no point in doing so -- any optimisations will see the IS
through n's unfolding.  Nor will a big size inhibit unfoldings functions
that mention a literal Integer, because the float-out pass will float
all those constants to top level.

Note [etAddAlt result discounts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When adding the size of alternatives, we *add* the result discounts
too, rather than take the *maximum*.  For a multi-branch case, this
gives a discount for each branch that returns a constructor, making us
keener to inline.  I did try using 'max' instead, but it makes nofib
'rewrite' and 'puzzle' allocate significantly more, and didn't make
binary sizes shrink significantly either.

Note [Discounts and thresholds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Constants for discounts and thresholds are defined in 'UnfoldingOpts'. They are:

unfoldingCreationThreshold
     At a definition site, if the unfolding is bigger than this, we
     may discard it altogether

unfoldingUseThreshold
     At a call site, if the unfolding, less discounts, is smaller than
     this, then it's small enough inline

unfoldingDictDiscount
     The discount for each occurrence of a dictionary argument
     as an argument of a class method.  Should be pretty small
     else big functions may get inlined

unfoldingFunAppDiscount
     Discount for a function argument that is applied.  Quite
     large, because if we inline we avoid the higher-order call.

unfoldingVeryAggressive
     If True, the compiler ignores all the thresholds and inlines very
     aggressively. It still adheres to arity, simplifier phase control and
     loop breakers.


Historical Note: Before April 2020 we had another factor,
ufKeenessFactor, which would scale the discounts before they were subtracted
from the size. This was justified with the following comment:

  -- We multiply the raw discounts (args_discount and result_discount)
  -- ty opt_UnfoldingKeenessFactor because the former have to do with
  --  *size* whereas the discounts imply that there's some extra
  --  *efficiency* to be gained (e.g. beta reductions, case reductions)
  -- by inlining.

However, this is highly suspect since it means that we subtract a *scaled* size
from an absolute size, resulting in crazy (e.g. negative) scores in some cases
(#15304). We consequently killed off ufKeenessFactor and bumped up the
ufUseThreshold to compensate.


Note [Function applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a function application (f a b)

  - If 'f' is an argument to the function being analysed,
    and there's at least one value arg, record a FunAppDiscount for f

  - If the application if a PAP (arity > 2 in this example)
    record a *result* discount (because inlining
    with "extra" args in the call may mean that we now
    get a saturated application)

Code for manipulating sizes
-}

---------------------------------------
-- Right associative; predence level unimportant
infixr 5 `metAddN`, `etAddN`, `metAdd`, `metAddAlt`

metAddN :: Size -> Maybe ExprTree -> Maybe ExprTree
metAddN _ Nothing = Nothing
metAddN n (Just et) = Just (n `etAddN` et)

etAddN :: Size -> ExprTree -> ExprTree
-- Does not account for et_wc_tot geting too big, but that doesn't
-- matter; the extra increment is always small, and we never get
-- a long cascade of etAddNs
etAddN n1 (ExprTree { et_wc_tot = t2, et_size = n2, et_cases = c2, et_ret = ret2 })
  = ExprTree { et_wc_tot = n1+t2, et_size = n1+n2, et_cases = c2, et_ret = ret2 }

metAdd :: Size -> Maybe ExprTree -> Maybe ExprTree -> Maybe ExprTree
-- Takes return value from the right hand argument
metAdd _ Nothing _ = Nothing
metAdd _ _ Nothing = Nothing
metAdd bOMB_OUT_SIZE (Just et1) (Just et2)
  | ExprTree { et_wc_tot = t1, et_size = n1, et_cases = c1, et_ret = _ret1 } <- et1
  , ExprTree { et_wc_tot = t2, et_size = n2, et_cases = c2, et_ret =  ret2 } <- et2
  , let t12 = t1 + t2
  = if   t12 >= bOMB_OUT_SIZE
    then Nothing
    else Just (ExprTree { et_wc_tot = t12
                        , et_size   = n1 + n2
                        , et_cases  = c1 `unionBags` c2
                        , et_ret    = ret2 })

metAddAlt :: Size -> Maybe ExprTree -> Maybe ExprTree -> Maybe ExprTree
-- Adds return discounts from both args
metAddAlt _ Nothing _ = Nothing
metAddAlt _ _ Nothing = Nothing
metAddAlt bOMB_OUT_SIZE (Just et1) (Just et2)
  | ExprTree { et_wc_tot = t1, et_size = n1, et_cases = c1, et_ret = ret1 } <- et1
  , ExprTree { et_wc_tot = t2, et_size = n2, et_cases = c2, et_ret = ret2 } <- et2
  , let t12 = t1 + t2
  = if   t12 >= bOMB_OUT_SIZE
    then Nothing
    else Just (ExprTree { et_wc_tot = t12
                        , et_size   = n1 + n2
                        , et_cases  = c1 `unionBags` c2
                        , et_ret    = ret1 + ret2 })


-- | The "expression tree"; an abstraction of the RHS of the function
exprTreeN :: Size -> ExprTree
exprTreeN n = ExprTree { et_size = n, et_wc_tot = n, et_cases = emptyBag, et_ret = 0 }

etZero :: ExprTree
etZero = ExprTree { et_wc_tot = 0, et_size = 0, et_cases = emptyBag, et_ret = 0 }

etCaseOf :: Size -> Id -> Id -> [AltTree] -> Maybe ExprTree
-- We make the case itself free (remember that in this case the scrutinee
-- is a variable) but charge for each alternative (included in `altTreesSize`)
etCaseOf bOMB_OUT_SIZE scrut case_bndr alts
  | tot >= bOMB_OUT_SIZE = Nothing
  | otherwise            = Just (ExprTree { et_wc_tot = tot, et_size = 0, et_ret = 0
                                          , et_cases = unitBag case_tree })
  where
    case_tree = CaseOf scrut case_bndr alts
    tot       = altTreesSize alts

altTreesSize :: [AltTree] -> Size
-- Total worst-case size of a [AltTree], including the per-alternative cost of altSize
altTreesSize alts = foldl' add_alt 0 alts
  where
    add_alt n (AltTree _ _ (ExprTree { et_wc_tot = alt_tot }))
       = n + alt_tot + altSize

etScrutOf :: Id -> Discount -> ExprTree
etScrutOf v d = etZero { et_cases = unitBag (ScrutOf v d) }

{- *********************************************************************
*                                                                      *
            From ExprTree to Size
     This is used when we have an acutal call site
*                                                                      *
********************************************************************* -}

data InlineContext
   = IC { ic_free  :: Id -> ArgSummary  -- Current unfoldings for free variables
        , ic_bound :: IdEnv ArgSummary  -- Summaries for local variables
        , ic_want_res :: Bool           -- True <=> result is scrutinised/demanded
                                        --          so apply result discount
     }

data ArgSummary = ArgNoInfo
                | ArgIsCon AltCon [ArgSummary]  -- Value args only
                | ArgIsNot [AltCon]
                | ArgIsLam

hasArgInfo :: ArgSummary -> Bool
hasArgInfo ArgNoInfo = False
hasArgInfo _         = True

instance Outputable ArgSummary where
  ppr ArgNoInfo       = text "ArgNoInfo"
  ppr ArgIsLam        = text "ArgIsLam"
  ppr (ArgIsCon c as) = ppr c <> ppr as
  ppr (ArgIsNot cs)   = text "ArgIsNot" <> ppr cs


-------------------------
exprTreeWillInline :: Size -> ExprTree -> Bool
-- (cheapExprTreeSize limit et) takes an upper bound `n` on the
-- size of et; i.e. without discounts etc.
-- Return True if (s <= limit), False otherwise
exprTreeWillInline limit (ExprTree { et_wc_tot = tot }) = tot <= limit

-------------------------
exprTreeSize :: InlineContext -> ExprTree -> Size
exprTreeSize !ic (ExprTree { et_size  = size
                           , et_cases = cases
                           , et_ret   = ret_discount })
  = foldr ((+) . caseTreeSize (ic { ic_want_res = False }))
          discounted_size cases
  where
    discounted_size | ic_want_res ic = size - ret_discount
                    | otherwise      = size

caseTreeSize :: InlineContext -> CaseTree -> Size
caseTreeSize ic (ScrutOf bndr disc)
  = case lookupBndr ic bndr of
      ArgNoInfo   -> 0
      ArgIsNot {} -> -disc  -- E.g. bndr is a DFun application
                            --      T8732 need to inline mapM_
      ArgIsLam    -> -disc  -- Apply discount
      ArgIsCon {} -> -disc  -- Apply discount

caseTreeSize ic (CaseOf scrut_var case_bndr alts)
  = case lookupBndr ic scrut_var of
      ArgNoInfo     -> altsSize ic case_bndr alts + case_size

      ArgIsNot cons -> altsSize ic case_bndr (trim_alts cons alts)
         -- The case-expression may not disappear, but it scrutinises
         -- a variable bound to something with structure; may lead to
         -- avoiding a thunk, or other benefits.  So we give a discount
         -- compared to ArgNoInfo.  How much?  Rather a guess, but simply
         -- not adding case_size is convenient.
         --
         -- The function 'radiance' in nofib/real/smallpt benefits a lot from this

      ArgIsLam -> altsSize ic case_bndr alts  -- Case will disappear altogether

      arg_summ@(ArgIsCon con args)
         | Just at@(AltTree alt_con bndrs rhs) <- find_alt con alts
         , let new_summaries :: [(Id,ArgSummary)]
               new_summaries = (case_bndr,arg_summ) : bndrs `zip` args
                  -- Don't forget to add a summary for the case binder!
               ic' = ic { ic_bound = ic_bound ic `extendVarEnvList` new_summaries }
                     -- In DEFAULT case, bs is empty, so extending is a no-op
         -> assertPpr ((alt_con == DEFAULT) || (bndrs `equalLength` args))
                      (ppr arg_summ $$ ppr at) $
            exprTreeSize ic' rhs - caseElimDiscount
              -- Take off an extra discount for eliminating the case expression itself

         | otherwise  -- Happens for empty alternatives
         -> altsSize ic case_bndr alts
  where
    case_size = altSize * length alts
      -- We make the case itself free, but charge for each alternatives
      -- (the latter is already included in the AltTrees)
      -- If there are no alternatives (case e of {}), we get zero


find_alt :: AltCon -> [AltTree] -> Maybe AltTree
find_alt _   []                     = Nothing
find_alt con (alt:alts)
   | AltTree DEFAULT _ _ <- alt = go alts       (Just alt)
   | otherwise                  = go (alt:alts) Nothing
   where
     go []         deflt              = deflt
     go (alt:alts) deflt
       | AltTree con' _ _ <- alt, con==con' = Just alt
       | otherwise                          = go alts deflt

trim_alts :: [AltCon] -> [AltTree] -> [AltTree]
trim_alts _   []                      = []
trim_alts acs (alt:alts)
  | AltTree con _ _ <- alt, con `elem` acs = trim_alts acs alts
  | otherwise                              = alt : trim_alts acs alts

altsSize :: InlineContext -> Id -> [AltTree] -> Size
-- Size of a (retained) case expression
altsSize ic case_bndr alts = foldr ((+) . size_alt) 0 alts
  -- Just add up the  sizes of the alternatives
  -- We recurse in case we have
  --    args = [a,b], expr_tree = [CaseOf a [ X -> CaseOf b [...]
  --                                        , Y -> CaseOf b [...] ] ]
  -- Then for a call with ArgInfo for `b`, but not `a`, we want to get
  -- the trimmed trees in the X and Y branches
  where
    size_alt :: AltTree -> Size
    size_alt (AltTree _ bndrs rhs) = exprTreeSize ic' rhs
        -- Cost for the alternative is already in `rhs`
      where
        -- Must extend ic_bound, lest a captured variable
        -- is looked up in ic_free by lookupBndr
        new_summaries :: [(Id,ArgSummary)]
        new_summaries = [(b,ArgNoInfo) | b <- case_bndr:bndrs]
        ic' = ic { ic_bound = ic_bound ic `extendVarEnvList` new_summaries }

lookupBndr :: HasDebugCallStack => InlineContext -> Id -> ArgSummary
lookupBndr (IC { ic_bound = bound_env, ic_free = lookup_free }) var
  | Just info <- assertPpr (isId var) (ppr var) $
                 lookupVarEnv bound_env var = info
  | otherwise                               = lookup_free var
