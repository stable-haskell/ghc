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

        ExprTree, exprTree, exprTreeSize, keptCaseSize,
        exprTreeWillInline, couldBeSmallEnoughToInline,
        ArgSummary(..), hasArgInfo,

        Size(..), leqSize, addSizeN, addSize, adjustSize, sizeZero,
        InlineContext(..),

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
   { unfoldingCreationThreshold :: !Int
      -- ^ Threshold above which unfoldings are not *created*

   , unfoldingUseThreshold :: !Int
      -- ^ Threshold above which unfoldings are not *inlined*

   , unfoldingFunAppDiscount :: !Int
      -- ^ Discount for lambdas that are used (applied)

   , unfoldingDictDiscount :: !Int
      -- ^ Discount for dictionaries

   , unfoldingVeryAggressive :: !Bool
      -- ^ Force inlining in many more cases

   , unfoldingCaseThreshold :: !Int
      -- ^ Don't consider depth up to x

   , unfoldingCaseScaling :: !Int
      -- ^ Penalize depth with 1/x

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
   }

-- Helpers for "GHC.Driver.Session"

updateCreationThreshold :: Int -> UnfoldingOpts -> UnfoldingOpts
updateCreationThreshold n opts = opts { unfoldingCreationThreshold = n }

updateUseThreshold :: Int -> UnfoldingOpts -> UnfoldingOpts
updateUseThreshold n opts = opts { unfoldingUseThreshold = n }

updateFunAppDiscount :: Int -> UnfoldingOpts -> UnfoldingOpts
updateFunAppDiscount n opts = opts { unfoldingFunAppDiscount = n }

updateDictDiscount :: Int -> UnfoldingOpts -> UnfoldingOpts
updateDictDiscount n opts = opts { unfoldingDictDiscount = n }

updateVeryAggressive :: Bool -> UnfoldingOpts -> UnfoldingOpts
updateVeryAggressive n opts = opts { unfoldingVeryAggressive = n }


updateCaseThreshold :: Int -> UnfoldingOpts -> UnfoldingOpts
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
      TooBig -> UnfNever
      et@(SizeIs { et_size = size, et_cases = cases })
        | not (any is_case cases)
        , uncondInline expr n_val_bndrs size
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

    is_case (CaseOf {})  = True
    is_case (ScrutOf {}) = False


couldBeSmallEnoughToInline :: UnfoldingOpts -> Int -> CoreExpr -> Bool
-- We use 'couldBeSmallEnoughToInline' to avoid exporting inlinings that
-- we ``couldn't possibly use'' on the other side.  Can be overridden
-- w/flaggery.  Just the same as smallEnoughToInline, except that it has no
-- actual arguments.
couldBeSmallEnoughToInline opts threshold rhs
  = exprTreeWillInline threshold $
    exprTree opts [] body
  where
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
The basic idea of sizeExpr is obvious enough: count nodes.  But getting the
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

(1) We inline *unconditionally* if inlined thing is smaller (using sizeExpr)
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

uncondInline :: CoreExpr -> Arity -> Int -> Bool
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
-}

type ETVars = (VarSet,VarSet)  -- (avs, lvs)
              -- See Note [Constructing an ExprTree]

exprTree :: UnfoldingOpts -> [Var] -> CoreExpr -> ExprTree
-- Note [Computing the size of an expression]

exprTree opts args expr
  = go (mkVarSet args, emptyVarSet) expr
  where
    !bOMB_OUT_SIZE = unfoldingCreationThreshold opts
       -- Bomb out if size gets bigger than this
       -- Forcing bOMB_OUT_SIZE early prevents repeated
       -- unboxing of the Int argument.

    et_add     = etAdd bOMB_OUT_SIZE
    et_add_alt = etAddAlt bOMB_OUT_SIZE

    go :: ETVars -> CoreExpr -> ExprTree
          -- (avs,lvs): see Note [Constructing an ExprTree]
    go vs (Cast e _)   = go vs e
    go vs (Tick _ e)   = go vs e
    go _  (Type _)     = exprTreeN 0
    go _  (Coercion _) = exprTreeN 0
    go _  (Lit lit)    = exprTreeN (litSize lit)

    go vs (Lam b e)
      | isId b, not (id_is_free b) = go vs' e `et_add` lamSize opts
      | otherwise                  = go vs' e
      where
        vs' = vs `add_lv` b

    go vs (Let (NonRec binder rhs) body)
      = go_bind vs (binder, rhs)  `et_add`
        go (vs `add_lv` binder) body

    go vs (Let (Rec pairs) body)
      = foldr (et_add . go_bind vs') (go vs' body) pairs
      where
        vs' = vs `add_lvs` map fst pairs

    go vs e@(App {}) = go_app vs e []

    go vs (Var f) | id_is_free f = exprTreeN 0
                    -- Use calLSize to ensure we get constructor
                    -- discounts even on nullary constructors
                    | otherwise  = callTree opts vs f []

    go vs (Case e b _ alts) = go_case vs e b alts

    -----------------------------
    go_bind vs (bndr, rhs)
      | JoinPoint join_arity <- idJoinPointHood bndr
      , (bndrs, body) <- collectNBinders join_arity rhs
                          -- Skip arguments to join point
      = go (vs `add_lvs` bndrs) body
      | otherwise
      = size_up_alloc bndr `etAddN` go vs rhs

    -- Cost to allocate binding with given binder
    size_up_alloc bndr
      |  isTyVar bndr                    -- Doesn't exist at runtime
      || isJoinId bndr                   -- Not allocated at all
      || not (isBoxedType (idType bndr)) -- Doesn't live in heap
      = 0
      | otherwise
      = 10

    -----------------------------
    -- size_up_app is used when there's ONE OR MORE value args
    go_app :: ETVars -> CoreExpr -> [CoreExpr] -> ExprTree
                   -- args are the non-void value args
    go_app vs (App fun arg) args
               | arg_is_free arg = go_app vs fun args
               | otherwise       = go vs arg `et_add`
                                   go_app vs fun (arg:args)
    go_app vs (Var fun)     args = callTree opts vs fun args
    go_app vs (Tick _ expr) args = go_app vs expr args
    go_app vs (Cast expr _) args = go_app vs expr args
    go_app vs other         args = vanillaCallSize (length args) `etAddN`
                                   go vs other
       -- if the lhs is not an App or a Var, or an invisible thing like a
       -- Tick or Cast, then we should charge for a complete call plus the
       -- size of the lhs itself.

    -----------------------------
    -- Empty case
    go_case vs scrut _ [] = go vs scrut
         -- case e of {} never returns, so take size of scrutinee

    -- Record a CaseOf
    go_case vs@(avs,lvs) scrut b alts                 -- Now alts is non-empty
      | Just v <- recordCaseOf vs scrut
      = -- pprTrace "recordCaseOf" (ppr v $$ ppr lvs $$ ppr scrut $$ ppr alts) $
        go vs scrut `et_add`
        etOneCase (CaseOf v b (map (alt_alt_tree v) alts))
      where
        alt_alt_tree :: Id -> Alt Var -> AltTree
        alt_alt_tree v (Alt con bs rhs)
          = AltTree con val_bs (10 `etAddN` go (add_alt_bndrs v val_bs) rhs)
          where
            val_bs = filter isId bs

        add_alt_bndrs v bs
          | v `elemVarSet` avs = (avs `extendVarSetList` (b:bs), lvs)
                                 -- Don't forget to add the case binder, b
          | otherwise = vs

    -- Don't record a CaseOf
    go_case vs scrut b alts    -- alts is non-empty
      = caseSize scrut alts `etAddN`   -- A bit odd that this is only in one branch
        go vs scrut         `et_add`
        foldr1 et_add_alt (map alt_expr_tree alts)
      where
        alt_expr_tree :: Alt Var -> ExprTree
        alt_expr_tree (Alt _con bs rhs)
          = 10 `etAddN` go (vs `add_lvs` (b:bs)) rhs
            -- Don't charge for bndrs, so that wrappers look cheap
            -- (See comments about wrappers with Case)
            -- Don't forget to add the case binder, b, to lvs.
            --
            -- IMPORTANT: *do* charge 10 for the alternative, else we
            -- find that giant case nests are treated as practically free
            -- A good example is Foreign.C.Error.errnoToIOError

caseSize :: CoreExpr -> [CoreAlt] -> Int
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

recordCaseOf :: ETVars -> CoreExpr -> Maybe Id
recordCaseOf (_,lvs) (Var v)
     | v `elemVarSet` lvs  = Nothing
     | otherwise           = Just v
recordCaseOf vs (Tick _ e) = recordCaseOf vs e
recordCaseOf vs (Cast e _) = recordCaseOf vs e
recordCaseOf _     _       = Nothing

arg_is_free :: CoreExpr -> Bool
-- "free" means we don't charge for this
-- occurrence in a function application
arg_is_free (Var id)      = id_is_free id
arg_is_free (Tick _ e)    = arg_is_free e
arg_is_free (Cast e _)    = arg_is_free e
arg_is_free (Type {})     = True
arg_is_free (Coercion {}) = True
arg_is_free _             = False

id_is_free :: Id -> Bool
id_is_free id = not (isJoinId id) && isZeroBitTy (idType id)
   -- Don't count expressions such as State# RealWorld
   -- exclude join points, because they can be rep-polymorphic
   -- and typePrimRep will crash


-- | Finds a nominal size of a string literal.
litSize :: Literal -> Int
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
callTree :: UnfoldingOpts -> ETVars -> Id -> [CoreExpr] -> ExprTree
callTree opts vs fun val_args
  = case idDetails fun of
      FCallId _        -> exprTreeN (vanillaCallSize n_val_args)
      JoinId {}        -> exprTreeN (jumpSize        n_val_args)
      PrimOpId op _    -> exprTreeN (primOpSize op   n_val_args)
      DataConWorkId dc -> conSize dc n_val_args
      ClassOpId {}     -> classOpSize opts vs fun val_args
      _                -> funSize opts vs fun n_val_args
  where
    n_val_args = length val_args

-- | The size of a function call
vanillaCallSize :: Int -> Int
vanillaCallSize n_val_args = 10 * (1 + n_val_args)
        -- The 1+ is for the function itself
        -- Add 1 for each non-trivial value arg

-- | The size of a jump to a join point
jumpSize :: Int -> Int
jumpSize n_val_args = 2 * (1 + n_val_args)
  -- A jump is 20% the size of a function call. Making jumps free reopens
  -- bug #6048, but making them any more expensive loses a 21% improvement in
  -- spectral/puzzle. TODO Perhaps adjusting the default threshold would be a
  -- better solution?

classOpSize :: UnfoldingOpts -> ETVars -> Id -> [CoreExpr] -> ExprTree
-- See Note [Conlike is interesting]
classOpSize _ _ _ []
  = etZero
classOpSize opts vs fn val_args
  | arg1 : _ <- val_args
  , Just dict <- recordCaseOf vs arg1
  = warnPprTrace (not (isId dict)) "classOpSize" (ppr fn <+> ppr val_args) $
    vanillaCallSize (length val_args) `etAddN`
    etOneCase (ScrutOf dict (unfoldingDictDiscount opts))
           -- If the class op is scrutinising a lambda bound dictionary then
           -- give it a discount, to encourage the inlining of this function
           -- The actual discount is rather arbitrarily chosen
  | otherwise
  = exprTreeN (vanillaCallSize (length val_args))

funSize :: UnfoldingOpts -> ETVars -> Id -> Int -> ExprTree
-- Size for function calls that are not constructors or primops
-- Note [Function applications]
funSize opts (avs,_) fun n_val_args
  | fun `hasKey` buildIdKey   = etZero  -- Wwant to inline applications of build/augment
  | fun `hasKey` augmentIdKey = etZero  -- so we give size zero to the whole call
  | otherwise = SizeIs { et_size  = size
                       , et_cases = cases
                       , et_ret   = res_discount }
  where
    size | n_val_args == 0 = 0
         | otherwise       = vanillaCallSize n_val_args

    -- Discount if this is an interesting variable, and is applied
    -- Discount is enough to make the application free (but not negative!)
    --  See Note [Function and non-function discounts]
    cases | n_val_args > 0, fun `elemVarSet` avs
          = unitBag (ScrutOf fun size)
          | otherwise
          = emptyBag

    res_discount | idArity fun > n_val_args = unfoldingFunAppDiscount opts
                 | otherwise                = 0
        -- If the function is partially applied, show a result discount

lamSize :: UnfoldingOpts -> ExprTree
-- Does not include the size of the body, just the lambda itself
lamSize opts = SizeIs { et_size = 10, et_cases = emptyBag
                      , et_ret = unfoldingFunAppDiscount opts }

conSize :: DataCon -> Int -> ExprTree
-- Does not need to include the size of the arguments themselves
conSize dc n_val_args
  = SizeIs { et_size = n, et_cases = emptyBag, et_ret = n }
  where
    n | n_val_args == 0 = 0   -- Like variables
      | unboxed_tuple   = 0   -- See Note [Unboxed tuple size and result discount]
      | otherwise       = 10  -- See Note [Constructor size and result discount]

    unboxed_tuple = isUnboxedTupleDataCon dc

primOpSize :: PrimOp -> Int -> Int
primOpSize op n_val_args
  | primOpOutOfLine op = op_size + n_val_args
  | otherwise          = op_size
 where
   op_size = primOpCodeSize op



{- Note [Constructor size and result discount]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
-- | The "expression tree"; an abstraction of the RHS of the function
exprTreeN :: Int -> ExprTree
exprTreeN n = SizeIs { et_size = n, et_cases = emptyBag, et_ret = 0 }

etAddN :: Int -> ExprTree -> ExprTree
etAddN _ TooBig = TooBig
etAddN n1 (SizeIs { et_size = n2, et_cases = c2, et_ret = ret2 })
  = SizeIs { et_size = n1+n2, et_cases = c2, et_ret = ret2 }

etAdd :: Int -> ExprTree -> ExprTree -> ExprTree
-- Takes return value from the right hand argument
etAdd _ TooBig _ = TooBig
etAdd _ _ TooBig = TooBig
etAdd bOMB_OUT_SIZE (SizeIs { et_size = n1, et_cases = c1, et_ret = _ret1 })
                    (SizeIs { et_size = n2, et_cases = c2, et_ret = ret2 })
  | n12 >= bOMB_OUT_SIZE = TooBig
  | otherwise = SizeIs { et_size  = n12
                       , et_cases = c1 `unionBags` c2
                       , et_ret   = ret2 }
  where
    n12 = n1 + n2

etAddAlt :: Int -> ExprTree -> ExprTree -> ExprTree
-- etAddalt is used to add the sizes of case alternatives
etAddAlt _  TooBig _ = TooBig
etAddAlt _  _ TooBig = TooBig
etAddAlt bOMB_OUT_SIZE (SizeIs { et_size = n1, et_cases = c1, et_ret = ret1 })
                       (SizeIs { et_size = n2, et_cases = c2, et_ret = ret2 })
  | n12 >= bOMB_OUT_SIZE = TooBig
  | otherwise = SizeIs { et_size  = n12
                       , et_cases = c1 `unionBags` c2
                       , et_ret   = ret1 + ret2 }
                           -- et_ret: see Note [etAddAlt result discounts]
  where
    n12 = n1 + n2

etZero :: ExprTree
etZero = SizeIs { et_size = 0, et_cases = emptyBag, et_ret = 0 }

etOneCase :: CaseTree -> ExprTree
etOneCase ct = SizeIs { et_size = 0, et_cases = unitBag ct, et_ret = 0 }

{- *********************************************************************
*                                                                      *
            From ExprTree to Size
     This is used when we have an acutal call site
*                                                                      *
********************************************************************* -}

data Size = STooBig | SSize {-# UNPACK #-} !Int

instance Outputable Size where
  ppr STooBig = text "STooBig"
  ppr (SSize n) = int n

sizeZero :: Size
sizeZero = SSize 0

sizeN :: Int -> Size
sizeN n = SSize n

addSize :: Size -> Size -> Size
addSize (SSize n1) (SSize n2) = SSize (n1+n2)
addSize _          _          = STooBig

addSizeN :: Int -> Size -> Size
addSizeN n1 (SSize n2) = SSize (n1+n2)
addSizeN _  STooBig    = STooBig

adjustSize :: (Int -> Int) -> Size -> Size
adjustSize f (SSize n) = SSize (f n)
adjustSize _ STooBig   = STooBig

leqSize :: Size -> Int -> Bool
leqSize STooBig   _ = False
leqSize (SSize n) m = n <= m

-------------------------
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
exprTreeWillInline :: Int -> ExprTree -> Bool
-- (cheapExprTreeSize limit et) takes an upper bound `n` on the
-- size of et; i.e. without discounts etc.
-- Return True if (s <- limit), False otherwise
-- Bales out early in the False case
exprTreeWillInline limit et
  = go et (\n -> n <= limit) 0
  where
    go :: ExprTree -> (Int -> Bool) -> Int -> Bool
    go _      _ n | n > limit = False
    go TooBig _ _             = False
    go (SizeIs { et_size = size, et_cases = cases }) k n
      = foldr go_ct k cases (n+size)

    go_ct :: CaseTree -> (Int -> Bool) -> Int -> Bool
    go_ct (ScrutOf {})      k n = k n
    go_ct (CaseOf _ _ alts) k n = foldr go_alt k alts n

    go_alt :: AltTree -> (Int -> Bool) -> Int -> Bool
    go_alt (AltTree _ _ et) k n = go et k (n+10)


-------------------------
exprTreeSize :: InlineContext -> ExprTree -> Size
exprTreeSize _    TooBig = STooBig
exprTreeSize !ic (SizeIs { et_size  = size
                         , et_cases = cases
                         , et_ret   = ret_discount })
  = foldr (addSize . caseTreeSize (ic { ic_want_res = False }))
          (sizeN discounted_size) cases
  where
    discounted_size | ic_want_res ic = size - ret_discount
                    | otherwise      = size

caseTreeSize :: InlineContext -> CaseTree -> Size
caseTreeSize ic (ScrutOf bndr disc)
  = case lookupBndr ic bndr of
      ArgNoInfo   -> sizeN 0
      ArgIsNot {} -> sizeN 0
      ArgIsLam    -> sizeN (-disc)  -- Apply discount
      ArgIsCon {} -> sizeN (-disc)  -- Apply discount

caseTreeSize ic (CaseOf scrut_var case_bndr alts)
  = case lookupBndr ic scrut_var of
      ArgNoInfo     -> keptCaseSize ic case_bndr alts
      ArgIsLam      -> keptCaseSize ic case_bndr alts
      ArgIsNot cons -> keptCaseSize ic case_bndr (trim_alts cons alts)

      arg_summ@(ArgIsCon con args)
         | Just at@(AltTree alt_con bndrs rhs) <- find_alt con alts
         , let new_summaries :: [(Id,ArgSummary)]
               new_summaries = (case_bndr,arg_summ) : bndrs `zip` args
                  -- Don't forget to add a summary for the case binder!
               ic' = ic { ic_bound = ic_bound ic `extendVarEnvList` new_summaries }
                     -- In DEFAULT case, bs is empty, so extending is a no-op
         -> assertPpr ((alt_con == DEFAULT) || (bndrs `equalLength` args)) (ppr arg_summ $$ ppr at) $
            exprTreeSize ic' rhs

         | otherwise  -- Happens for empty alternatives
         -> keptCaseSize ic case_bndr alts

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

keptCaseSize :: InlineContext -> Id -> [AltTree] -> Size
-- Size of a (retained) case expression
keptCaseSize ic case_bndr alts
  = foldr (addSize . size_alt) (sizeN 0) alts
    -- We make the case itself free, but charge for each alternative
    -- If there are no alternatives (case e of {}), we get just the size of the scrutinee
  where
    size_alt :: AltTree -> Size
    size_alt (AltTree _ bndrs rhs)
       = exprTreeSize ic' rhs
        -- Cost for the alternative is already in `rhs`
      where
        -- Must extend ic_bound, lest a captured variable is
        -- looked up in ic_free by lookupBndr
        new_summaries :: [(Id,ArgSummary)]
        new_summaries = [(b,ArgNoInfo) | b <- case_bndr:bndrs]
        ic' = ic { ic_bound = ic_bound ic `extendVarEnvList` new_summaries }

lookupBndr :: HasDebugCallStack => InlineContext -> Id -> ArgSummary
lookupBndr (IC { ic_bound = bound_env, ic_free = lookup_free }) var
  | Just info <- assertPpr (isId var) (ppr var) $
                 lookupVarEnv bound_env var = info
  | otherwise                               = lookup_free var


