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

        ExprDigest, mkExprDigest, exprDigestSize, altDigestsSize,
        exprDigestMaxSize, exprDigestWillInline, exprIsTooLarge,
        ArgDigest(..), digestHasInfo, digestIsEvald,
        ContDigest(..),

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
import GHC.Types.Basic  ( Arity, RecFlag )
-- import GHC.Types.ForeignCall
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
These inlining heurstics all concern callSiteInline; that is, the
decision about whether or not to inline a let-binding.  It does not
concern inlining used-once things, or things with a trivial RHS, which
kills the let-binding altogether.

Key examples
------------
Example 1:

   let f x = case x of
               A -> True
               B -> <big>
   in ...(f A)....(f B)...

Even though f's entire RHS is big, it collapses to something small when applied
to A.  We'd like to spot this.

Example 2a:

   let f x = case x of
               (p,q) -> case p of
                           A -> True
                           B -> <big>
   in ...(f (A,3))....

This is similar to Example 1, but nested.

Example 2b:

   let f x y = case x of
                 A -> case y of
                        A -> <big>
                        B -> True
                 B -> <big>
   in ...(f A B)...

Again nested.  Note that (f A A), (f B _) generate a big RHS. Only
(f A B) generates a small RHS.  The ExprDigest defined here deals nicely
with this case.

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

Example 4: result discounts:

   let f x = case x of
               A -> (e1,e2)
               B -> (e3,e4)
               C -> e5
    in \y -> ...case f y of { (a,b) -> blah }

Here there is nothing interesting about f's /argument/, but:
  * Many of f's cases return a data constructur (True or False)
  * The call of `f` scrutinises its result

If we inline `f`, the 'case' will cancel with pair constrution, we should be keener
to inline `f` than if it was called in a boring context. We say that `f`  has a
/result discount/ meaning that we should apply a discount if `f` is called in
a case context.

Example 5: totally boring

   let f x = not (g x x)
   in ....(\y. f y)...

Here,there is /nothing/ interesting about either the arguments or the result
coninuation of the call (f y).  There is no point in inlining, even if f's RHS
is small, as it is here.

Design overview
---------------
The question is whether or not to inline f = \xyz -> rhs.
The key idea is to abstract `rhs` to an ExprDigest, which gives a measure of
size, but records structure for case-expressions.

The moving parts
-----------------
* An unfolding is accompanied (in its UnfoldingGuidance) with its GHC.Core.ExprDigest,
  computed by GHC.Core.Unfold.mkExprDigest.

* At a call site, GHC.Core.Opt.Simplify.Inline.contArgDigests constructs an ArgDigest
  for each value argument. This reflects any nested data construtors.

* Then GHC.Core.Unfold.exprDigestSize takes information about the context of the
  call (particularly the ArgDigest for each argument) and computes a final size
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

   , unfoldingCaseThreshold :: !Int
      -- ^ Don't consider depth up to x

   , unfoldingCaseScaling :: !Int
      -- ^ Penalize depth with 1/x

   , exprDigestCaseWidth :: !Int
      -- ^ Bale out entirely with a case width greater than this
      -- See Note [Bale out on very wide case expressions]

   , exprDigestCaseDepth :: !Int
      -- ^ Don't make ExprDigests with a case depth greater than this

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

   , unfoldingUseThreshold   = 80
      -- Adjusted 90 -> 75 when adding discounts for free variables which
      -- generally make things more likely to inline.  Reducing the threshold
      -- eliminates some undesirable compile-time regressions (e.g. T10412a)
      --
      -- Previously: adjusted upwards in #18282, when I reduced
      -- the result discount for constructors.

   , unfoldingFunAppDiscount = 45
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

     -- Bale out at exprDigestCaseWidth
     -- See Note [Bale out on very wide case expressions]
   , exprDigestCaseWidth = 20

     -- Don't record CaseOf beyond exprDigestCaseDepth
   , exprDigestCaseDepth = 4
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
  = case mkExprDigest opts val_bndrs body of
      Nothing -> UnfNever
      Just ed@(ExprDigest { ed_wc_tot = tot })
        | uncondInline expr n_val_bndrs tot
        -> UnfWhen { ug_unsat_ok  = unSaturatedOk
                   , ug_boring_ok =  boringCxtOk
                   , ug_arity     = n_val_bndrs }   -- Note [INLINE for small functions]

        | is_top_bottoming
        -> UnfNever   -- See Note [Do not inline top-level bottoming functions]

        | otherwise
        -> UnfIfGoodArgs { ug_args = val_bndrs, ug_tree = ed }

  where
    (bndrs, body) = collectBinders expr
    val_bndrs   = filter isId bndrs
    n_val_bndrs = length val_bndrs

exprIsTooLarge :: UnfoldingOpts -> Size -> CoreExpr -> Bool
-- 'exprIsTooLarge' says when an expression is bigger than some threshold;
-- used to avoid creating too-large specialisations of teh function
exprIsTooLarge opts threshold rhs
  = isNothing (mkExprDigest opts' [] body)
  where
    opts' = opts { unfoldingCreationThreshold = threshold }
            -- We use a different (and larger) theshold here for
            -- creating specialised copies of the function
    (_, body) = collectBinders rhs

uncondInline :: CoreExpr -> Arity -> Size -> Bool
-- Inline unconditionally if there no size increase
-- Size of call is arity (+1 for the function)
-- See Note [INLINE for small functions]
uncondInline rhs arity size
  | arity > 0 = size <= 10 * (arity + 1) -- See Note [INLINE for small functions] (1)
  | otherwise = exprIsTrivial rhs        -- See Note [INLINE for small functions] (4)


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

(1) We inline *unconditionally* if inlined thing is smaller (using exprSizeDigest)
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


{- *********************************************************************
*                                                                      *
            From CoreExpr to ExprDigest
     This is used when we make the unfolding for a function
*                                                                      *
********************************************************************* -}

{- Note [Constructing an ExprDigest]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given a function      f = \x1...xn. body
in the typical case we will record an UnfoldingGuidance of

   UnfoldIfGoodArgs { ug_args = filter isValArg [x1,..,xn]
                    , ug_tree = mkExprDigest body }

The ug_tree :: ExprDigest is an abstaction or "digest" of the body
of the function.  An ExprDigest has

  * A CaseOf for each case-expression that scrutinises an argument or
    free variable, with a branch for each alternative.

  * A DiscVal for each other interesting use a variable, giving a discount
    to apply if that argument has structure. e.g. a function that is applied.

How mkExprDigest works
------------------
EDVars maintains a partition of in-scope variables into avs, lvs and fvs,
defined as follows:

* avs: argument variables, or variables bound by a case on an
       argument variable

  We record a CaseOf or DiscVal for the `avs`

* lvs: variables bound by lambda and lets in the body; and by
       case expressions that scrutinise one of the `lvs`, or
       a non-variable.

  We never record a CaseOf or DiscVal for one of the `lvs`.

* fvs: we record a CaseOf /but not DiscVal (see (ED2)/ for other variables that
  are neither `lvs` nor `avs`; that is, variables free in the entire definition.
  See Example 3 of Note [Overview of inlining heuristics].

Wrinkles:

(ED1) We must be careful about recording enormous functions, with very wide or very
  deep case trees. (This can happen with Generics; e.g. test T5642.)  We limit
  both with UnfoldingOpts: see the use of `exprDigestCaseWidth` and `exprDigestCaseDepth`
  in `mkExprDigest`.
  * When we exceed the maximum case-depth we just record DiscVal info (instead of
    CaseOf) for the scrutinee.

  * When we exceed the maximum case width we just bale out entirely and say that
    the function is too big. See Note [Bale out on very wide case expressions].

(ED2) It is IMPORTANT that we do not record DiscVal for free variables:
        let f x = y x 3 <big>
        in  ...(f 3)...
  There nothing we will learn about the free `y` that will make the inlining of
  `f` more attractive.  Hence we don't record DiscVal for `y`.  This is imporant,
  because even a call like (reverse xs) would otherwise record a DiscVal for
  `reverse` which is very silly.

Note [Computing the size of an expression]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The basic idea of exprSizeDigest is obvious enough: count nodes.  But getting the
heuristics right has taken a long time.  Here's the basic strategy:

    * Variables, literals: 0
      (Exception for string literals, see litSize.)

    * Function applications (f e1 .. en): 10 + #value args

    * Constructor applications: 10, regardless of #args
      See #22317

    * Let(rec): 10 + size of components

    * Primops: size 1.  They look fantastically cheap. Done partly
      as a result of #4978.

    * Tick, Cast: 0

    * Lam: we don't charge anything for the binders, just the body

    * Case: see `caseSize`

Examples

  Size  Term
  --------------
     0     42#
     0     x
     0     True
    20     f x
    10     Just x
    40     f (g x)

Notice that 'x' counts 0, while (f x) counts 20.  That's deliberate: there's
a function call to account for.  Notice also that constructor applications
are very cheap, because exposing them to a caller is so valuable.
-}

data EDVars = ED { ed_avs, ed_lvs :: VarSet }
              -- See Note [Constructing an ExprDigest]
              -- These sets include type variables, which do no harm,
              -- and are a bit tiresome to filter out.

mkExprDigest :: UnfoldingOpts -> [Var] -> CoreExpr -> Maybe ExprDigest
-- Nothing => too big
-- See Note [Overview of inlining heuristics]
-- See Note [Computing the size of an expression]

mkExprDigest opts args expr
  = go (exprDigestCaseDepth opts) (ED { ed_avs = mkVarSet args, ed_lvs = emptyVarSet }) expr
  where
    !max_width     = exprDigestCaseWidth opts
    !bOMB_OUT_SIZE = unfoldingCreationThreshold opts
       -- Bomb out if size gets bigger than this
       -- Forcing bOMB_OUT_SIZE early prevents repeated
       -- unboxing of the Int argument.

    med_add     = medAdd bOMB_OUT_SIZE
    med_add_alt = medAddAlt bOMB_OUT_SIZE

    go :: Int -> EDVars -> CoreExpr -> Maybe ExprDigest
          -- rcd is the /remaining/ case depth; decreases toward zero
          -- (avs,lvs): see Note [Constructing an ExprDigest]
    go rcd vs (Cast e _)      = go rcd vs e
    go rcd vs (Tick _ e)      = go rcd vs e
    go _   _  (Type _)        = Just (edSized 0)
    go _   _  (Coercion _)    = Just (edSized 0)
    go _   _  (Lit lit)       = Just (edSized (litSize lit))
    go rcd vs (Case e b _ as) = go_case rcd vs e b as
    go rcd vs (Let bind body) = go_let rcd vs bind body
    go rcd vs (Lam b e)       = go_lam rcd vs b e
    go rcd vs e@(App {})      = go_app rcd vs e []
    go _   vs (Var f)         = Just (callDigest opts vs f [])
                                -- Same as (go_app rcd vs (Var f) [] 0)
                                -- Use callDigest to ensure we get constructor
                                -- discounts even on nullary constructors

    ----------- Lambdas ------------------
    go_lam rcd vs bndr body
      | isId bndr, not (isZeroBitId bndr) = lamSize opts `medAddS` go rcd vs' body
      | otherwise                         = go rcd vs' body
      where
        vs' = vs `add_lv` bndr

    ----------- Applications ------------------
    go_app :: Int -> EDVars -> CoreExpr -> [CoreExpr] -> Maybe ExprDigest
      -- args:  all the value args
      -- voids: counts the zero-bit arguments; don't charge for these
      --        This makes a difference in ST-heavy code which does a lot
      --        of state passing, and which can be in an inner loop.
    go_app rcd vs (Tick _ expr) args = go_app rcd vs expr args
    go_app rcd vs (Cast expr _) args = go_app rcd vs expr args
    go_app rcd vs (App fun arg) args
      | isTypeArg arg     = go_app rcd vs fun args
      | otherwise         = go rcd vs arg `med_add`
                            go_app rcd vs fun (arg:args)
    go_app _   vs (Var fun) args = Just (callDigest opts vs fun args)
    go_app rcd vs other     args = vanillaCallSize args `medAddS` go rcd vs other
    -- If the lhs is not an App or a Var, or an invisible thing like a
    -- Tick or Cast, then we should charge for a complete call plus the
    -- size of the lhs itself.

    ----------- Let-expressions ------------------
    go_let rcd vs (NonRec binder rhs) body
      = go_bind rcd vs (binder, rhs)  `med_add`
        go rcd (vs `add_lv` binder) body

    go_let rcd vs (Rec pairs) body
      = foldr (med_add . go_bind rcd vs') (go rcd vs' body) pairs
      where
        vs' = vs `add_lvs` map fst pairs

    go_bind rcd vs (bndr, rhs)
      | isTyVar bndr
      = Just (edSized 0)

      | JoinPoint join_arity <- idJoinPointHood bndr
      , (bndrs, body) <- collectNBinders join_arity rhs
                          -- Skip arguments to join point
      = go rcd (vs `add_lvs` bndrs) body

      | isUnliftedType (idType bndr) -- Doesn't live in heap
      = go rcd vs rhs

      | otherwise
      = closureSize `medAddS` go rcd vs rhs

    -----------Case expressions ------------------
    go_case :: Int -> EDVars -> CoreExpr -> Id -> [CoreAlt] -> Maybe ExprDigest
    -- Empty case
    go_case rcd vs scrut _ [] = go rcd vs scrut
         -- case e of {} never returns, so take size of scrutinee

    -- Record a CaseOf
    go_case remaining_case_depth vs scrut b alts
      | alts `lengthExceeds` max_width
      = Nothing   -- See Note [Bale out on very wide case expressions]

      | Just scrut_id <- interestingVarScrut vs scrut
      = if   remaining_case_depth > 0
        then do { alts' <- mapM (alt_alt_tree scrut_id) alts
                ; edCaseOf bOMB_OUT_SIZE scrut_id b alts' }
        else Just (edDiscVal scrut_id caseElimDiscount) `med_add`
              -- When this scrutinee has structure, we expect to eliminate the case
             go_alts remaining_case_depth vs b alts
      where
        -- Decremement remaining case depth when going inside
        -- a case with more than one alternative.
        -- Don't do so for single-alt cases, becuase they don't give rise
        -- to exponential blow-up, and it's very common to have deep nests
        -- of case x of (a,b) -> case a of I# a' -> ...
        rcd1 | isSingleton alts = remaining_case_depth
             | otherwise        = remaining_case_depth - 1

        alt_alt_tree :: Id -> Alt Var -> Maybe AltDigest
        alt_alt_tree scrut_id (Alt con bs rhs)
          = do { let val_bs = filter isId bs  -- The AltDigest only has value binders
               ; rhs <- go rcd1 (add_alt_lvs vs scrut_id (b:val_bs)) rhs
                        -- (b:bs) don't forget to include the case binder
               ; return (AltDigest con val_bs rhs) }

    -- Don't record a CaseOf
    go_case rcd vs scrut b alts    -- alts is non-empty
      = -- caseDiscount scrut alts  `medAddS`   -- It is a bit odd that this `caseDiscount` business is only
        --                                  -- applied in this equation, not in the previous ones
        go rcd vs scrut      `med_add`
        go_alts (rcd-1) vs b alts

    go_alts :: Int -> EDVars -> Id -> [CoreAlt] -> Maybe ExprDigest
    -- Add up the sizes of all RHSs.  Only used for DiscVal.
    -- IMPORTANT: include a charge for the case itself, else we
    -- find that giant case nests are treated as practically free
    -- A good example is Foreign.C.Error.errnoToIOError
    go_alts rcd vs case_bndr alts
      = caseSize case_bndr alts `medAddS`
        foldr1 med_add_alt (map alt_expr_tree alts)
      where
        alt_expr_tree :: Alt Var -> Maybe ExprDigest
        alt_expr_tree (Alt _con bs rhs) = go rcd (vs `add_lvs` (case_bndr : bs)) rhs
            -- Don't charge for bndrs, so that wrappers look cheap
            -- (See comments about wrappers with Case)
            -- Don't forget to add the case binder, b, to lvs.

add_lv :: EDVars -> Var -> EDVars
add_lv edvs@(ED { ed_lvs = lvs }) b = edvs { ed_lvs = lvs `extendVarSet` b }

add_lvs :: EDVars -> [Var] -> EDVars
add_lvs edvs@(ED { ed_lvs = lvs }) bs = edvs { ed_lvs = lvs `extendVarSetList` bs }

add_alt_lvs :: EDVars
            -> Var       -- Scrutinised variable
            -> [Var]     -- Binders of the alternative (including the case binder)
            -> EDVars
add_alt_lvs edvs@(ED { ed_avs = avs }) scrut_id alt_bndrs
  | scrut_id `elemVarSet` avs = edvs { ed_avs = avs `extendVarSetList` alt_bndrs }
  | otherwise                 = edvs

interestingVarScrut :: EDVars -> CoreExpr -> Maybe Id
-- The scrutinee of a case is worth recording
-- That is: an argument variable or free variable, but /not/
--          one of the `lvs`, which are bound by local lets etc
interestingVarScrut (ED { ed_lvs = lvs }) (Var v)
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
-- Used by GHC.Core.Unfold.mkExprDigest
litSize (LitNumber LitNumBigNat _)  = 100
litSize (LitString str) = 10 + 10 * ((BS.length str + 3) `div` 4)
        -- If size could be 0 then @f "x"@ might be too small
        -- [Sept03: make literal strings a bit bigger to avoid fruitless
        --  duplication of little strings]
litSize _other = 0    -- Must match size of nullary constructors
                      -- Key point: if  x |-> 4, then x must inline unconditionally
                      --            (eg via case binding)

----------------------------
callDigest :: UnfoldingOpts -> EDVars -> Id -> [CoreExpr] -> ExprDigest
-- Caller accounts for the size of the arguments,
-- but not for the cost of building a closure
callDigest opts vs fun val_args
  = case idDetails fun of
      FCallId _        -> edSized (vanillaCallSize val_args)
      JoinId {}        -> edSized (jumpSize        val_args)
      PrimOpId op _    -> edSized (primOpSize op   val_args)
      DataConWorkId dc -> edSized (conAppSize dc val_args)
      ClassOpId {}     -> classOpAppET opts vs fun val_args
      _                -> genAppET opts vs fun val_args

-- | The size of a function call
-- Caller accounts for the size of the arguments,
-- but not for the cost of building a closure
vanillaCallSize :: [CoreExpr] -> Size
vanillaCallSize val_args = foldl' arg_sz 2 val_args
  where
    arg_sz n arg
      | isZeroBitArg arg  = n
      | exprIsTrivial arg = n+2
      | otherwise         = n+closureSize
-- 10 * (1 + n_val_args - voids)
        -- The 1+ is for the function itself
        -- Add 1 for each non-trivial value arg

conAppSize :: DataCon -> [CoreExpr] -> Size
-- Smaller than vanillaCallSize; don't charge for the call
-- itself, just for the closures it builds
conAppSize _dc val_args = foldl' arg_sz 0 val_args
  where
    arg_sz n arg
      | exprIsTrivial arg = n
      | otherwise         = n+closureSize
{-
  | isUnboxedTupleDataCon dc
  = edZero     -- See Note [Unboxed tuple size and result discount]
  | n_val_args == 0    -- Like variables
  = edZero
  | otherwise  -- See Note [Constructor size and result discount]
  = ExprDigest { ed_size = 10, ed_wc_tot = 10
             , ed_cases = emptyBag, ed_ret = 10 }
-}

primOpSize :: PrimOp -> [CoreExpr] -> Size
-- Args are almost always strict, so we don't charge for arg
-- closures, unlike vanillaCallSize, conAppSize
primOpSize op val_args
  | primOpOutOfLine op = op_size + length val_args
  | otherwise          = op_size
 where
   op_size = primOpCodeSize op

-- | The size of a jump to a join point
jumpSize :: [CoreExpr] -> Size
jumpSize val_args = vanillaCallSize val_args
  --  2 * (1 + n_val_args - voids)
  -- A jump isn't so much smaller than a function call, but it's definitely
  --   a known, exactly saturated call, so we make it very cheap
  -- A jump is 20% the size of a function call. Making jumps free reopens
  -- bug #6048, but making them any more expensive loses a 21% improvement in
  -- spectral/puzzle. TODO Perhaps adjusting the default threshold would be a
  -- better solution?

classOpAppET :: UnfoldingOpts -> EDVars -> Id -> [CoreExpr] -> ExprDigest
classOpAppET _ _ _ []
  = edZero
classOpAppET opts vs fn val_args
  | arg1 : _ <- val_args
  , Just dict <- interestingVarScrut vs arg1
  = warnPprTrace (not (isId dict)) "classOpAppET" (ppr fn <+> ppr val_args) $
    vanillaCallSize val_args `edAddS`
    edDiscVal dict (unfoldingDictDiscount opts)
           -- If the class op is scrutinising a lambda bound dictionary then
           -- give it a discount, to encourage the inlining of this function
           -- The actual discount is rather arbitrarily chosen
  | otherwise
  = edSized (vanillaCallSize val_args)

genAppET :: UnfoldingOpts -> EDVars -> Id -> [CoreExpr] -> ExprDigest
-- Size for function calls that are not constructors or primops
-- Note [Function applications]
-- Caller accounts for the size of the arguments,
-- but not for the cost of building a closure
genAppET opts (ED { ed_avs = avs, ed_lvs = lvs }) fun val_args
  | fun `hasKey` buildIdKey   = edZero  -- We want to inline applications of build/augment
  | fun `hasKey` augmentIdKey = edZero  -- so we give size zero to the whole call
  | otherwise = ExprDigest { ed_wc_tot = size, ed_size  = size
                           , ed_cases = cases, ed_fvs = fvs
                           , ed_ret   = res_discount }
  where
    size | null val_args = 0    -- Naked variable counts zero
         | otherwise     = vanillaCallSize val_args

    in_avs = fun `elemVarSet` avs
    in_lvs = fun `elemVarSet` lvs

    -- Discount if this is an interesting variable, and is applied
    -- If the function is an argument and is applied to some values,
    -- give it a discount -- maybe we can apply that lambda.
    --  See Note [Function and non-function discounts]
    cases | (_:_) <- val_args
          , fun `elemVarSet` avs  -- Arguments only: see Note (ED2)
          = unitBag (DiscVal fun (unfoldingFunAppDiscount opts))
          | otherwise
          = emptyBag

    unf = idUnfolding fun
    fvs | isLocalId fun
        , not (in_avs || in_lvs)
        , hasCoreUnfolding unf
        , not (isEvaldUnfolding unf)
        = unitVarSet fun
        | otherwise = emptyVarSet

    res_discount | val_args `lengthLessThan` idArity fun = unfoldingFunAppDiscount opts
                 | otherwise                             = 0
        -- If the function is partially applied, show a result discount

lamSize :: UnfoldingOpts -> Size
-- Does not include the size of the body, just the lambda itself
lamSize _ = 0  -- Lambdas themselves cost nothing


closureSize :: Size  -- Size for a heap-allocated closure
closureSize = 15

caseSize :: Id -> [alt] -> Size
-- For a case expression we charge for charge for each alternative.
-- (This does /not/ include the cost of the alternatives themselves)
-- If there are no alternatives (case e of {}), we get zero
--
-- Unlifted cases are much, much cheaper becuase they don't need to
-- save live variables, push a return address create an info table
-- An unlifted case is just a conditional; and if there is only one
-- alternative, it's not even a conditional, hence size zero
caseSize scrut_id alts
  | isUnliftedType (idType scrut_id)
  = if isSingleton alts then 0
                        else 5 * length alts
  | otherwise
  = 10 * length alts

caseElimDiscount :: Discount
-- Bonus for eliminating a case
caseElimDiscount = 10

{- Note [Bale out on very wide case expressions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
With very wide case trees (say N) we get a size N*altSize, which usually
prevents inlining (e.g. 20*altSize = 200 currently, which is way above the
inlining thresold of 90-ish).  Plus, we risk getting big CaseOf trees in the
ExprDigest.

If we aren't going to inline it anyway, then retaining the unfolding in an
interface file is plain silly; T5642 (involving Generics) is a good example.
We had a very wide case whose branches mentioned dozens of data structures,
each of which had very large types.

Sebastian wonders about the effect of this choice on #11068.

For example, if we apply such a function to a data constructor, we could in
principle get a huge discount (because all but one branches fall away).
Something a bit like this happens in nofib/real/cacheprof, in module Main:
    instance PP Reg where
       pp ppm ST_0 = "%st"
       ... other special cases ...
       pp ppm r    = "%" ++ map toLower (show r)
If we inline that call (show r), itself a 32-wide case,  we get a lot of CAFs
which can be floated out.  Results in a 4% allocation change.

So perhaps we could use a different setting
* when generating an unfolding /within a module/
* when generating an unfolding /for an interface file/

Currently we aren't doing this, but we could consider it

See (ED1) in Note [Constructing an ExprDigest].

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

If conAppET gives a cost of 10 (regardless of n_val_args) and a
discount of 10, that'll make each alternative RHS cost zero.  We
charge 10 for each case alternative (see size_up_alt).  If we give a
bigger discount (say 20) in conAppET, we'll make the case expression
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

Note [edAddAlt result discounts]
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
infixr 5 `medAddS`, `edAddS`, `medAdd`, `medAddAlt`

-- Nomenclature:
-- * The "med" is for "Maybe ExprDigest"
-- * The "S" is for Size

medAddS :: Size -> Maybe ExprDigest -> Maybe ExprDigest
medAddS n med = fmap (edAddS n) med

edAddS :: Size -> ExprDigest -> ExprDigest
-- Does not account for ed_wc_tot geting too big, but that doesn't
-- matter; the extra increment is always small, and we never get
-- a long cascade of edAddSs
edAddS n1 ed@(ExprDigest { ed_wc_tot = t2, ed_size = n2 })
  = ed { ed_wc_tot = n1+t2, ed_size = n1+n2 }

---------------------------------------------------
-- medAdd vs medAddAlt are identical except
-- * medAdd    takesthe return discount from the second argument
-- * medAddAlt adds the return discounts

medAdd :: Size -> Maybe ExprDigest -> Maybe ExprDigest -> Maybe ExprDigest
medAdd _ Nothing _ = Nothing
medAdd _ _ Nothing = Nothing
medAdd bOMB_OUT_SIZE (Just ed1) (Just ed2)
  | ExprDigest { ed_wc_tot = t1, ed_size = n1, ed_cases = c1, ed_ret = _ret1, ed_fvs = fvs1 } <- ed1
  , ExprDigest { ed_wc_tot = t2, ed_size = n2, ed_cases = c2, ed_ret =  ret2, ed_fvs = fvs2 } <- ed2
  , let t12 = t1 + t2
  = if   t12 >= bOMB_OUT_SIZE
    then Nothing
    else Just (ExprDigest { ed_wc_tot = t12
                          , ed_size   = n1 + n2
                          , ed_fvs    = fvs1 `unionVarSet` fvs2
                          , ed_cases  = c1 `unionBags` c2
                          , ed_ret    = ret2 })

medAddAlt :: Size -> Maybe ExprDigest -> Maybe ExprDigest -> Maybe ExprDigest
-- Adds return discounts from both args
medAddAlt _ Nothing _ = Nothing
medAddAlt _ _ Nothing = Nothing
medAddAlt bOMB_OUT_SIZE (Just ed1) (Just ed2)
  | ExprDigest { ed_wc_tot = t1, ed_size = n1, ed_cases = c1, ed_ret = ret1, ed_fvs = fvs1 } <- ed1
  , ExprDigest { ed_wc_tot = t2, ed_size = n2, ed_cases = c2, ed_ret = ret2, ed_fvs = fvs2 } <- ed2
  , let t12 = t1 + t2
  = if   t12 >= bOMB_OUT_SIZE
    then Nothing
    else Just (ExprDigest { ed_wc_tot = t12
                          , ed_size   = n1 + n2
                          , ed_fvs    = fvs1 `unionVarSet` fvs2
                          , ed_cases  = c1 `unionBags` c2
                          , ed_ret    = ret1 + ret2 -- See Note [Result discount for case alternatives]
          })


---------------------------------------------------
-- | The "expression tree"; an abstraction of the RHS of the function
--   The "S" signals the Size argument
edSized :: Size -> ExprDigest
edSized n = ExprDigest { ed_size = n, ed_wc_tot = n, ed_fvs = emptyVarSet
                       , ed_cases = emptyBag, ed_ret = 0 }

edZero :: ExprDigest
edZero = edSized 0

edCaseOf :: Size -> Id -> Id -> [AltDigest] -> Maybe ExprDigest
-- We make the case itself free (remember that in this case the scrutinee
-- is a variable) but charge for each alternative (included in `altDigestsSize`)
edCaseOf bOMB_OUT_SIZE scrut case_bndr alts
  | tot >= bOMB_OUT_SIZE = Nothing
  | otherwise            = Just (ExprDigest { ed_wc_tot = tot, ed_ret = ret
                                            , ed_size = 0, ed_fvs = emptyVarSet
                                            , ed_cases = unitBag case_tree })
  where
    case_tree = CaseOf scrut case_bndr alts
    tot       = altDigestsSize scrut alts
    ret       = altDigestsDiscount alts

altDigestsSize :: Id -> [AltDigest] -> Size
-- Total worst-case size of a [AltDigest], including the per-alternative cost of altSize
altDigestsSize scrut_id alts
  = foldl' add_alt (caseSize scrut_id alts) alts
  where
    add_alt :: Size -> AltDigest -> Size
    add_alt sz (AltDigest _ _ (ExprDigest { ed_wc_tot = alt_tot })) = sz + alt_tot

altDigestsDiscount :: [AltDigest] -> Discount
-- See Note [Result discount for case alternatives]
altDigestsDiscount alts = foldl' add_alt 0 alts
  where
    add_alt n (AltDigest _ _ (ExprDigest { ed_ret = ret })) = n + ret

edDiscVal :: Id -> Discount -> ExprDigest
edDiscVal v d = edZero { ed_cases = unitBag (DiscVal v d) }

{- Note [Result discount for case alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When adding the size of alternatives, we *add* the result discounts
too, rather than take the *maximum*.  For a multi-branch case, this
gives a discount for each branch that returns a constructor, making us
keener to inline.  I did try using 'max' instead, but it makes nofib
'rewrite' and 'puzzle' allocate significantly more, and didn't make
binary sizes shrink significantly either.
-}

{- *********************************************************************
*                                                                      *
            From ExprDigest to Size
     This is used when we have an acutal call site
*                                                                      *
********************************************************************* -}

data InlineContext
   = IC { ic_free  :: Id -> ArgDigest  -- Current unfoldings for free variables
        , ic_bound :: IdEnv ArgDigest  -- Digests for local variables
        , ic_cont  :: ContDigest       -- Describes the continuation
     }

data ArgDigest
  = ArgNoInfo
  | ArgIsCon AltCon [ArgDigest]   -- Arg is a data-con application;
                                  --   the [ArgDigest] is for value args only
  | ArgIsNot [AltCon]             -- Arg is a value, not headed by these construtors
  | ArgIsDFun                     -- Arg is a DFun application
  | ArgIsLam                      -- Arg is a lambda
  | ArgNonTriv                    -- The arg has some struture, but is not a value
                                  --   e.g. it might be a call (f x)

data ContDigest   -- Describes how the result of the call is consumed
  = BoringCtxt
  | RhsCtxt RecFlag     -- Rhs of a let-binding; see Note [RHS of lets]
  | DiscArgCtxt         -- Argument of a function with non-zero arg discount
  | RuleArgCtxt         -- We are somewhere in the argument of a function with rules

  | ValAppCtxt          -- We're applied to at least one value arg
                        -- This arises when we have ((f x |> co) y)
                        -- Then the (f x) has argument 'x' but in a ValAppCtxt

  | CaseCtxt            -- We're the scrutinee of a case
                        -- that decomposes its scrutinee

instance Outputable ContDigest where
  ppr CaseCtxt    = text "CaseCtxt"
  ppr ValAppCtxt  = text "ValAppCtxt"
  ppr BoringCtxt  = text "BoringCtxt"
  ppr (RhsCtxt ir)= text "RhsCtxt" <> parens (ppr ir)
  ppr DiscArgCtxt = text "DiscArgCtxt"
  ppr RuleArgCtxt = text "RuleArgCtxt"


digestHasInfo :: ArgDigest -> Bool
digestHasInfo ArgNoInfo = False
digestHasInfo _         = True

digestIsEvald :: ArgDigest -> Bool
digestIsEvald ArgNoInfo     = False
digestIsEvald ArgNonTriv    = False
digestIsEvald (ArgIsCon {}) = True
digestIsEvald (ArgIsNot {}) = True
digestIsEvald ArgIsDFun     = True
digestIsEvald ArgIsLam      = True

instance Outputable ArgDigest where
  ppr ArgNoInfo       = text "ArgNoInfo"
  ppr ArgIsLam        = text "ArgIsLam"
  ppr ArgNonTriv      = text "ArgNonTriv"
  ppr ArgIsDFun       = text "ArgIsDFun"
  ppr (ArgIsCon c as) = ppr c <> ppr as
  ppr (ArgIsNot cs)   = text "ArgIsNot" <> ppr cs

-------------------------
exprDigestWillInline :: Size -> ExprDigest -> Bool
-- (exprDigestWillInline limit ed) takes an upper bound `limit` on the
-- size of ed; i.e. without discounts etc.
-- Return True if (s <= limit), False otherwise
exprDigestWillInline limit (ExprDigest { ed_wc_tot = tot }) = tot <= limit

-------------------------
exprDigestMaxSize :: ExprDigest -> Size
exprDigestMaxSize = ed_wc_tot

exprDigestSize :: InlineContext -> ExprDigest -> Size
-- See Note [Overview of inlining heuristics]
exprDigestSize !ic (ExprDigest { ed_size = size, ed_cases = cases
                               , ed_ret = ret_discount})
  = foldr ((+) . caseDigestSize ic) size cases
    - returnDiscount ic ret_discount

returnDiscount :: InlineContext -> Discount -> Discount
returnDiscount (IC { ic_cont = cont_info }) ret_discount
  = case cont_info of
      BoringCtxt  -> 0
      DiscArgCtxt -> 0
      RuleArgCtxt -> ret_discount
      CaseCtxt    -> ret_discount
      ValAppCtxt  -> ret_discount
      RhsCtxt {}  -> 40 `min` ret_discount
         -- For RhsCtxt I suppose that exposing a data con is good in general;
         -- although 40 seems very arbitrary
         --
         -- `min` thresholding: res_discount can be very large when a
         -- function returns constructors; but we only want to invoke
         -- that large discount when there's a case continuation.

caseDigestSize :: InlineContext -> CaseDigest -> Size
caseDigestSize ic (DiscVal bndr disc)
  = case lookupBndr ic bndr of
      ArgNoInfo   -> 0
      ArgNonTriv  -> -10    -- E.g. bndr is a DFun application
                            --      T8732 need to inline mapM_

      -- Apply full discount for values
      ArgIsLam     -> -disc  -- Apply discount
      ArgIsNot {}  -> -disc
      ArgIsDFun {} -> -disc
      ArgIsCon {}  -> -disc  -- Apply discount

caseDigestSize ic (CaseOf scrut_var case_bndr alts)
  = case lookupBndr ic scrut_var of
      ArgNoInfo  -> caseAltsSize ic case_bndr alts + case_size
      ArgNonTriv -> caseAltsSize ic case_bndr alts + case_size

      ArgIsDFun     -> caseAltsSize ic case_bndr alts
      ArgIsNot cons -> caseAltsSize ic case_bndr (trim_alts cons alts)
         -- The case-expression may not disappear, but it scrutinises
         -- a variable bound to something with structure; may lead to
         -- avoiding a thunk, or other benefits.  So we give a discount
         -- compared to ArgNoInfo.  How much?  Rather a guess, but simply
         -- not adding case_size is convenient.
         --
         -- The function 'radiance' in nofib/real/smallpt benefits a lot from this

      ArgIsLam -> caseAltsSize ic case_bndr alts  -- Case will disappear altogether

      arg_digest@(ArgIsCon con args)
         | Just at@(AltDigest alt_con bndrs rhs) <- find_alt con alts
         , let new_digests :: [(Id,ArgDigest)]
               new_digests = (case_bndr,arg_digest) : bndrs `zip` args
                  -- Don't forget to add a digest for the case binder!
               ic' = ic { ic_bound = ic_bound ic `extendVarEnvList` new_digests }
                     -- In DEFAULT case, bs is empty, so extending is a no-op
         -> assertPpr ((alt_con == DEFAULT) || (bndrs `equalLength` args))
                      (ppr arg_digest $$ ppr at) $
            exprDigestSize ic' rhs - caseElimDiscount
              -- Take off an extra discount for eliminating the case expression itself

         | otherwise  -- Happens for empty alternatives
         -> caseAltsSize ic case_bndr alts
  where
    case_size = caseSize scrut_var alts

find_alt :: AltCon -> [AltDigest] -> Maybe AltDigest
find_alt _   []                     = Nothing
find_alt con (alt:alts)
   | AltDigest DEFAULT _ _ <- alt = go alts       (Just alt)
   | otherwise                  = go (alt:alts) Nothing
   where
     go []         deflt              = deflt
     go (alt:alts) deflt
       | AltDigest con' _ _ <- alt, con==con' = Just alt
       | otherwise                          = go alts deflt

trim_alts :: [AltCon] -> [AltDigest] -> [AltDigest]
trim_alts cons alts
  | null cons = alts
  | otherwise = go alts
  where
    go [] = []
    go (alt:alts) | AltDigest con _ _ <- alt, con `elem` cons = go alts
                  | otherwise                               = alt : go alts

caseAltsSize :: InlineContext -> Id -> [AltDigest] -> Size
-- Size of a (retained) case expression
-- Do /not/ include the per-alternative cost, just the alternatives themselves
caseAltsSize ic case_bndr alts = foldr ((+) . size_alt) 0 alts
  -- Just add up the  sizes of the alternatives
  -- We recurse in case we have
  --    args = [a,b], expr_tree = [CaseOf a [ X -> CaseOf b [...]
  --                                        , Y -> CaseOf b [...] ] ]
  -- Then for a call with ArgInfo for `b`, but not `a`, we want to get
  -- the trimmed trees in the X and Y branches
  where
    size_alt :: AltDigest -> Size
    size_alt (AltDigest _ bndrs rhs) = exprDigestSize ic' rhs
        -- Cost for the alternative is already in `rhs`
      where
        -- Must extend ic_bound, lest a captured variable
        -- is looked up in ic_free by lookupBndr
        new_digests :: [(Id,ArgDigest)]
        new_digests = [(b,ArgNoInfo) | b <- case_bndr:bndrs]
        ic' = ic { ic_bound = ic_bound ic `extendVarEnvList` new_digests }

lookupBndr :: HasDebugCallStack => InlineContext -> Id -> ArgDigest
lookupBndr (IC { ic_bound = bound_env, ic_free = lookup_free }) var
  | Just info <- assertPpr (isId var) (ppr var) $
                 lookupVarEnv bound_env var = info
  | otherwise                               = lookup_free var
