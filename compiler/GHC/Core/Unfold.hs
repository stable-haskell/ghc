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

        ExprTree, exprTree, exprTreeSize,

        ArgSummary(..), nonTriv,
        CallCtxt(..),

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
import GHC.Types.ForeignCall
import GHC.Types.Tickish

import GHC.Builtin.Names
import GHC.Builtin.PrimOps

import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Utils.Outputable

import GHC.Data.Bag

import qualified Data.ByteString as BS


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


{- *********************************************************************
*                                                                      *
                    Argument summary
*                                                                      *
********************************************************************* -}

data ArgSummary = ArgNoInfo
                | ArgIsCon AltCon [ArgInfo]
                | ArgIsNot [AltCon]
                | ArgIsLam

data ArgSummary = TrivArg       -- Nothing interesting
                | NonTrivArg    -- Arg has structure
                | ValueArg      -- Arg is a con-app or PAP
                                -- ..or con-like. Note [Conlike is interesting]

instance Outputable ArgSummary where
  ppr TrivArg    = text "TrivArg"
  ppr NonTrivArg = text "NonTrivArg"
  ppr ValueArg   = text "ValueArg"

nonTriv ::  ArgSummary -> Bool
nonTriv TrivArg = False
nonTriv _       = True

data CallCtxt
  = BoringCtxt
  | RhsCtxt RecFlag     -- Rhs of a let-binding; see Note [RHS of lets]
  | DiscArgCtxt         -- Argument of a function with non-zero arg discount
  | RuleArgCtxt         -- We are somewhere in the argument of a function with rules

  | ValAppCtxt          -- We're applied to at least one value arg
                        -- This arises when we have ((f x |> co) y)
                        -- Then the (f x) has argument 'x' but in a ValAppCtxt

  | CaseCtxt            -- We're the scrutinee of a case
                        -- that decomposes its scrutinee

instance Outputable CallCtxt where
  ppr CaseCtxt    = text "CaseCtxt"
  ppr ValAppCtxt  = text "ValAppCtxt"
  ppr BoringCtxt  = text "BoringCtxt"
  ppr (RhsCtxt ir)= text "RhsCtxt" <> parens (ppr ir)
  ppr DiscArgCtxt = text "DiscArgCtxt"
  ppr RuleArgCtxt = text "RuleArgCtxt"

{-
Note [Calculate unfolding guidance on the non-occ-anal'd expression]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Notice that we give the non-occur-analysed expression to
calcUnfoldingGuidance.  In some ways it'd be better to occur-analyse
first; for example, sometimes during simplification, there's a large
let-bound thing which has been substituted, and so is now dead; so
'expr' contains two copies of the thing while the occurrence-analysed
expression doesn't.

Nevertheless, we *don't* and *must not* occ-analyse before computing
the size because

a) The size computation bales out after a while, whereas occurrence
   analysis does not.

b) Residency increases sharply if you occ-anal first.  I'm not
   100% sure why, but it's a large effect.  Compiling Cabal went
   from residency of 534M to over 800M with this one change.

This can occasionally mean that the guidance is very pessimistic;
it gets fixed up next round.  And it should be rare, because large
let-bound things that are dead are usually caught by preInlineUnconditionally


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
  = case exprTree opts bOMB_OUT_SIZE (mkVarSet val_bndrs) body of
      TooBig -> UnfNever
      et@(SizeIs { et_size = size })
        | uncondInline expr n_val_bndrs size
        -> UnfWhen { ug_unsat_ok  = unSaturatedOk
                   , ug_boring_ok =  boringCxtOk
                   , ug_arity     = n_val_bndrs }   -- Note [INLINE for small functions]

        | is_top_bottoming
        -> UnfNever   -- See Note [Do not inline top-level bottoming functions]

        | otherwise
        -> UnfIfGoodArgs { ug_args = val_bndrs, ug_tree = et }

  where
    (bndrs, body) = collectBinders expr
    bOMB_OUT_SIZE = unfoldingCreationThreshold opts
           -- Bomb out if size gets bigger than this
    val_bndrs   = filter isId bndrs
    n_val_bndrs = length val_bndrs

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

exprTree :: UnfoldingOpts
         -> Int             -- Bomb out if it gets bigger than this
         -> VarSet          -- Record scrutiny of these variables
         -> CoreExpr
         -> ExprTree

-- Note [Computing the size of an expression]

-- Forcing bOMB_OUT_SIZE early prevents repeated
-- unboxing of the Int argument.
exprTree opts !bOMB_OUT_SIZE svars expr
  = size_up expr
  where
    et_add     = etAdd bOMB_OUT_SIZE
    et_add_alt = etAddAlt bOMB_OUT_SIZE

    size_up :: CoreExpr -> ExprTree
    size_up (Cast e _)   = size_up e
    size_up (Tick _ e)   = size_up e
    size_up (Type _)     = exprTreeN 0
    size_up (Coercion _) = exprTreeN 0
    size_up (Lit lit)    = exprTreeN (litSize lit)

    size_up (Lam b e)
      | isId b, not (id_is_free b) = size_up e `et_add` lamSize opts
      | otherwise                  = size_up e

    size_up (Let (NonRec binder rhs) body)
      = size_up_bind (binder, rhs)  `et_add` size_up body

    size_up (Let (Rec pairs) body)
      = foldr (et_add . size_up_bind) (size_up body) pairs

    size_up e@(App {}) = size_up_app e []

    size_up (Var f) | id_is_free f = exprTreeN 0
                    -- Use calLSize to ensure we get constructor
                    -- discounts even on nullary constructors
                    | otherwise    = callTree opts svars f []

    size_up (Case e _ _ alts) = size_up_case e alts

    -----------------------------
    size_up_bind (bndr, rhs)
      | JoinPoint join_arity <- idJoinPointHood bndr
      , (_bndrs, body) <- collectNBinders join_arity rhs
                          -- Skip arguments to join point
      = size_up body
      | otherwise
      = size_up_alloc bndr `etAddN` size_up rhs

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
    size_up_app :: CoreExpr -> [CoreExpr] -> ExprTree
                   -- args are the non-void value args
    size_up_app (App fun arg) args
                 | arg_is_free arg = size_up_app fun args
                 | otherwise       = size_up arg  `et_add`
                                     size_up_app fun (arg:args)
    size_up_app (Var fun)     args = callTree opts svars fun args
    size_up_app (Tick _ expr) args = size_up_app expr args
    size_up_app (Cast expr _) args = size_up_app expr args
    size_up_app other         args = vanillaCallSize (length args) `etAddN`
                                     size_up other
       -- if the lhs is not an App or a Var, or an invisible thing like a
       -- Tick or Cast, then we should charge for a complete call plus the
       -- size of the lhs itself.

    -----------------------------
    size_up_case scrut [] = size_up scrut
         -- case e of {} never returns, so take size of scrutinee

    size_up_case scrut alts                    -- Now alts is non-empty
        | Just v <- interesting_id svars scrut -- We are scrutinising an argument variable
        = size_up scrut `et_add`
          etZero { et_cases = unitBag (CaseOf v (map alt_alt_tree alts)) }

        | otherwise
        = case_size     `etAddN`   -- A bit odd that this is only in one branch
          size_up scrut `et_add`
          foldr1 et_add_alt (map alt_expr_tree alts)

        where
          alt_alt_tree :: Alt Var -> AltTree
          alt_alt_tree (Alt con bs rhs)
            = AT con bs (exprTree opts bOMB_OUT_SIZE svars' rhs)
            where
              svars' = svars `extendVarSetList` bs

          alt_expr_tree :: Alt Var -> ExprTree
          alt_expr_tree (Alt _con _bndrs rhs) = 10 `etAddN` size_up rhs
              -- Don't charge for bndrs, so that wrappers look cheap
              -- (See comments about wrappers with Case)
              --
              -- IMPORTANT: *do* charge 10 for the alternative, else we
              -- find that giant case nests are treated as practically free
              -- A good example is Foreign.C.Error.errnoToIOError

          case_size
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
    ------------

interesting_id :: VarSet -> CoreExpr -> Maybe Id
interesting_id svars (Var v)
  | v `elemVarSet` svars  = Just v
interesting_id svars (Tick _ e) = interesting_id svars e
interesting_id svars (Cast e _) = interesting_id svars e
interesting_id _     _          = Nothing

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
callTree :: UnfoldingOpts -> VarSet -> Id -> [CoreExpr] -> ExprTree
callTree opts svars fun val_args
  = case idDetails fun of
      FCallId _        -> exprTreeN (vanillaCallSize n_val_args)
      JoinId {}        -> exprTreeN (jumpSize        n_val_args)
      PrimOpId op _    -> exprTreeN (primOpSize op   n_val_args)
      DataConWorkId dc -> conSize dc n_val_args
      ClassOpId {}     -> classOpSize opts svars val_args
      _                -> funSize opts svars fun n_val_args
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

classOpSize :: UnfoldingOpts -> VarSet -> [CoreExpr] -> ExprTree
-- See Note [Conlike is interesting]
classOpSize _ _ []
  = etZero
classOpSize opts svars val_args
  | arg1 : _ <- val_args
  , Just dict <- interesting_id svars arg1
  = vanillaCallSize (length val_args) `etAddN`
    etZero { et_cases = unitBag (ScrutOf dict (unfoldingDictDiscount opts)) }
           -- If the class op is scrutinising a lambda bound dictionary then
           -- give it a discount, to encourage the inlining of this function
           -- The actual discount is rather arbitrarily chosen
  | otherwise
  = exprTreeN (vanillaCallSize (length val_args))

funSize :: UnfoldingOpts -> VarSet -> Id -> Int -> ExprTree
-- Size for function calls that are not constructors or primops
-- Note [Function applications]
funSize opts svars fun n_val_args
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
    cases | n_val_args > 0, fun `elemVarSet` svars
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


{- *********************************************************************
*                                                                      *
            From ExprTree to Size
     This is used when we have an acutal call site
*                                                                      *
********************************************************************* -}

data Size = STooBig | SSize {-# UNPACK #-} !Int

sizeN :: Int -> Size
sizeN n = SSize n

addSize :: Size -> Size -> Size
addSize (SSize n1) (SSize n2) = SSize (n1+n2)
addSize _          _          = STooBig

leqSize :: Size -> Int -> Bool
leqSize STooBig   _ = False
leqSize (SSize n) m = n <= m

-------------------------
type UnfoldingInfo = IdEnv ArgInfo
   -- Domain is the bound vars of the function RHS

-------------------------
exprTreeSize :: UnfoldingInfo       -- Unfolding
             -> Bool                -- Apply result discount please
             -> ExprTree -> Size
exprTreeSize _ _ TooBig = STooBig
exprTreeSize unf want_res (SizeIs { et_size  = size
                                  , et_cases = cases
                                  , et_ret   = ret_discount })
  = foldr (addSize . caseTreeSize unf False)
          (sizeN discounted_size) cases
  where
    discounted_size | want_res  = size - ret_discount
                    | otherwise = size

caseTreeSize :: UnfoldingInfo -> Bool -> CaseTree -> Size
caseTreeSize unf _ (ScrutOf bndr disc)
  = case lookupBndr unf bndr of
      ArgNoInfo   -> sizeN 0
      ArgIsNot {} -> sizeN 0
      ArgIsLam    -> sizeN (-disc)  -- Apply discount
      ArgIsCon {} -> sizeN (-disc)  -- Apply discount

caseTreeSize unf want_res (CaseOf bndr alts)
  = case lookupBndr unf bndr of
      ArgNoInfo     -> keptCaseSize unf want_res alts
      ArgIsLam      -> keptCaseSize unf want_res alts
      ArgIsNot cons -> keptCaseSize unf want_res (trim_alts cons alts)
      ArgIsCon con args
         | Just (AT _ bs rhs) <- find_alt con alts
         , let unf' = extendVarEnvList unf (bs `zip` args)   -- In DEFAULT case, bs is empty
         -> exprTreeSize unf' want_res rhs
         | otherwise  -- Happens for empty alternatives
         -> keptCaseSize unf want_res alts

find_alt :: AltCon -> [AltTree] -> Maybe AltTree
find_alt _   []                     = Nothing
find_alt con (alt:alts)
   | AT DEFAULT _ _ <- alt = go alts       (Just alt)
   | otherwise             = go (alt:alts) Nothing
   where
     go []         deflt              = deflt
     go (alt:alts) deflt
       | AT con' _ _ <- alt, con==con' = Just alt
       | otherwise                     = go alts deflt

trim_alts :: [AltCon] -> [AltTree] -> [AltTree]
trim_alts _   []                      = []
trim_alts acs (alt:alts)
  | AT con _ _ <- alt, con `elem` acs = trim_alts acs alts
  | otherwise                         = alt : trim_alts acs alts

keptCaseSize :: UnfoldingInfo -> Bool -> [AltTree] -> Size
-- Size of a (retained) case expression
keptCaseSize unf want_res alts
  = foldr (addSize . size_alt) (sizeN 0) alts
    -- We make the case itself free, but charge for each alternative
    -- If there are no alternatives (case e of {}), we get just the size of the scrutinee
  where
    size_alt :: AltTree -> Size
    size_alt (AT _ _ rhs) = sizeN 10 `addSize` exprTreeSize unf want_res rhs
        -- Add 10 for each alternative
        -- Don't charge for args, so that wrappers look cheap
        -- (See comments about wrappers with Case)
        --
        -- IMPORTANT: *do* charge 1 for the alternative, else we
        -- find that giant case nests are treated as practically free
        -- A good example is Foreign.C.Error.errnoToIOError

lookupBndr :: UnfoldingInfo -> Id -> ArgInfo
lookupBndr unf bndr
  | Just info <- lookupVarEnv unf bndr = info
  | otherwise                          = idArgInfo bndr

