{-# OPTIONS_GHC -Wno-orphans      #-} -- Outputable and IEWrappedName
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module Language.Haskell.Syntax.Extension
{-# LANGUAGE MultiWayIf           #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


GHC.Hs.ImpExp: Abstract syntax: imports, exports, interfaces
-}

module GHC.Hs.ImpExp
    ( module Language.Haskell.Syntax.ImpExp
    , module GHC.Hs.ImpExp
    ) where

import Language.Haskell.Syntax.Extension
import Language.Haskell.Syntax.Module.Name
import Language.Haskell.Syntax.ImpExp

import GHC.Prelude

import GHC.Types.SourceText   ( SourceText(..) )
import GHC.Types.SrcLoc
import GHC.Types.Name
import GHC.Types.PkgQual

import GHC.Parser.Annotation
import GHC.Hs.Extension

import GHC.Utils.Outputable
import GHC.Utils.Panic

import GHC.Unit.Module.Warnings

import Data.Data
import Data.Maybe
import GHC.Hs.Doc (LHsDoc)


{-
************************************************************************
*                                                                      *
    Import and export declaration lists
*                                                                      *
************************************************************************

One per import declaration in a module.
-}

type instance Anno (ImportDecl (GhcPass p)) = SrcSpanAnnA



-- | Convenience function to answer the question if an import decl. is
-- qualified.
isImportDeclQualified :: ImportDeclQualifiedStyle -> Bool
isImportDeclQualified NotQualified = False
isImportDeclQualified _ = True



type instance ImportDeclPkgQual GhcPs = RawPkgQual
type instance ImportDeclPkgQual GhcRn = PkgQual
type instance ImportDeclPkgQual GhcTc = PkgQual

type instance XCImportDecl  GhcPs = XImportDeclPass
type instance XCImportDecl  GhcRn = XImportDeclPass
type instance XCImportDecl  GhcTc = DataConCantHappen

data XImportDeclPass = XImportDeclPass
    { ideclAnn        :: EpAnn EpAnnImportDecl
    , ideclSourceText :: SourceText -- Note [Pragma source text] in "GHC.Types.SourceText"
    , ideclImplicit   :: Bool
        -- ^ GHC generates an `ImportDecl` to represent the invisible `import Prelude`
        -- that appears in any file that omits `import Prelude`, setting
        -- this field to indicate that the import doesn't appear in the
        -- original source. True => implicit import (of Prelude)
    }
    deriving (Data)

type instance XXImportDecl  (GhcPass _) = DataConCantHappen

type instance Anno ModuleName = SrcSpanAnnA
type instance Anno [LocatedA (IE (GhcPass p))] = SrcSpanAnnLI

deriving instance Data (IEWrappedName GhcPs)
deriving instance Data (IEWrappedName GhcRn)
deriving instance Data (IEWrappedName GhcTc)

deriving instance Eq (IEWrappedName GhcPs)
deriving instance Eq (IEWrappedName GhcRn)
deriving instance Eq (IEWrappedName GhcTc)

-- ---------------------------------------------------------------------

-- API Annotations types

data EpAnnImportDecl = EpAnnImportDecl
  { importDeclAnnImport    :: EpToken "import" -- ^ The location of the @import@ keyword
  , importDeclAnnPragma    :: Maybe (EpaLocation, EpToken "#-}") -- ^ The locations of @{-# SOURCE@ and @#-}@ respectively
  , importDeclAnnSafe      :: Maybe (EpToken "safe") -- ^ The location of the @safe@ keyword
  , importDeclAnnLevel     :: Maybe EpAnnLevel -- ^ The location of the @splice@ or @quote@ keyword
  , importDeclAnnQualified :: Maybe (EpToken "qualified") -- ^ The location of the @qualified@ keyword
  , importDeclAnnPackage   :: Maybe EpaLocation -- ^ The location of the package name (when using @-XPackageImports@)
  , importDeclAnnAs        :: Maybe (EpToken "as") -- ^ The location of the @as@ keyword
  } deriving (Data)


instance NoAnn EpAnnImportDecl where
  noAnn = EpAnnImportDecl noAnn  Nothing Nothing  noAnn  Nothing  Nothing  Nothing

data EpAnnLevel = EpAnnLevelSplice (EpToken "splice")
                | EpAnnLevelQuote (EpToken "quote")
                deriving Data

instance HasLoc EpAnnLevel where
  getHasLoc (EpAnnLevelSplice tok) = getEpTokenSrcSpan tok
  getHasLoc (EpAnnLevelQuote tok) = getEpTokenSrcSpan tok

-- ---------------------------------------------------------------------

simpleImportDecl :: ModuleName -> ImportDecl GhcPs
simpleImportDecl mn = ImportDecl {
      ideclExt        = XImportDeclPass noAnn NoSourceText False,
      ideclName       = noLocA mn,
      ideclPkgQual    = NoRawPkgQual,
      ideclSource     = NotBoot,
      ideclSafe       = False,
      ideclLevelSpec  = NotLevelled,
      ideclQualified  = NotQualified,
      ideclAs         = Nothing,
      ideclImportList = Nothing
    }

instance (OutputableBndrId p
         , Outputable (Anno (IE (GhcPass p)))
         , Outputable (ImportDeclPkgQual (GhcPass p)))
       => Outputable (ImportDecl (GhcPass p)) where
    ppr (ImportDecl { ideclExt = impExt, ideclName = mod'
                    , ideclPkgQual = pkg
                    , ideclSource = from, ideclSafe = safe
                    , ideclLevelSpec = level
                    , ideclQualified = qual
                    , ideclAs = as, ideclImportList = spec })
      = hang (hsep [text "import", ppr_imp impExt from, pp_implicit impExt,
                    pp_level level False, pp_safe safe, pp_qual qual False,
                    ppr pkg, ppr mod',
                    pp_level level True, pp_qual qual True,
                    pp_as as])
             4 (pp_spec spec)
      where
        pp_implicit ext =
            let implicit = case ghcPass @p of
                            GhcPs | XImportDeclPass { ideclImplicit = implicit } <- ext -> implicit
                            GhcRn | XImportDeclPass { ideclImplicit = implicit } <- ext -> implicit
                            GhcTc -> dataConCantHappen ext
            in if implicit then text "(implicit)"
                           else empty

        pp_qual QualifiedPre False = text "qualified" -- Prepositive qualifier/prepositive position.
        pp_qual QualifiedPost True = text "qualified" -- Postpositive qualifier/postpositive position.
        pp_qual QualifiedPre True = empty -- Prepositive qualifier/postpositive position.
        pp_qual QualifiedPost False = empty -- Postpositive qualifier/prepositive position.
        pp_qual NotQualified _ = empty

        pp_level (LevelStylePre  sty) False = pp_level_style sty
        pp_level (LevelStylePost   _) False = empty
        pp_level (LevelStylePre    _) True = empty
        pp_level (LevelStylePost sty) True = pp_level_style sty
        pp_level NotLevelled _ = empty

        pp_level_style ImportDeclQuote = text "quote"
        pp_level_style ImportDeclSplice = text "splice"

        pp_safe False   = empty
        pp_safe True    = text "safe"

        pp_as Nothing   = empty
        pp_as (Just a)  = text "as" <+> ppr a

        ppr_imp ext IsBoot =
            let mSrcText = case ghcPass @p of
                                GhcPs | XImportDeclPass { ideclSourceText = mst } <- ext -> mst
                                GhcRn | XImportDeclPass { ideclSourceText = mst } <- ext -> mst
                                GhcTc -> dataConCantHappen ext
            in case mSrcText of
                  NoSourceText   -> text "{-# SOURCE #-}"
                  SourceText src -> ftext src <+> text "#-}"
        ppr_imp _ NotBoot = empty

        pp_spec Nothing             = empty
        pp_spec (Just (Exactly, (L _ ies))) = ppr_ies ies
        pp_spec (Just (EverythingBut, (L _ ies))) = text "hiding" <+> ppr_ies ies

        ppr_ies []  = text "()"
        ppr_ies ies = char '(' <+> interpp'SP ies <+> char ')'

{-
************************************************************************
*                                                                      *
\subsection{Imported and exported entities}
*                                                                      *
************************************************************************
-}

type instance XIEName    (GhcPass _) = NoExtField
type instance XIEDefault (GhcPass _) = EpToken "default"
type instance XIEPattern (GhcPass _) = EpToken "pattern"
type instance XIEType    (GhcPass _) = EpToken "type"
type instance XIEData    (GhcPass _) = EpToken "data"
type instance XXIEWrappedName (GhcPass _) = DataConCantHappen

type instance Anno (IEWrappedName (GhcPass _)) = SrcSpanAnnA

type instance Anno (IE (GhcPass p)) = SrcSpanAnnA

-- The additional field of type 'Maybe (WarningTxt pass)' holds information
-- about export deprecation annotations and is thus set to Nothing when `IE`
-- is used in an import list (since export deprecation can only be used in exports)
type instance XIEVar       GhcPs = Maybe (LWarningTxt GhcPs)
type instance XIEVar       GhcRn = Maybe (LWarningTxt GhcRn)
type instance XIEVar       GhcTc = NoExtField

-- The additional field of type 'Maybe (WarningTxt pass)' holds information
-- about export deprecation annotations and is thus set to Nothing when `IE`
-- is used in an import list (since export deprecation can only be used in exports)
type instance XIEThingAbs  GhcPs = Maybe (LWarningTxt GhcPs)
type instance XIEThingAbs  GhcRn = Maybe (LWarningTxt GhcRn)
type instance XIEThingAbs  GhcTc = ()

-- The additional field of type 'Maybe (WarningTxt pass)' holds information
-- about export deprecation annotations and is thus set to Nothing when `IE`
-- is used in an import list (since export deprecation can only be used in exports)
type instance XIEThingAll  GhcPs = (Maybe (LWarningTxt GhcPs), (EpToken "(", EpToken "..", EpToken ")"))
type instance XIEThingAll  GhcRn = (Maybe (LWarningTxt GhcRn), (EpToken "(", EpToken "..", EpToken ")"))
type instance XIEThingAll  GhcTc = (EpToken "(", EpToken "..", EpToken ")")

-- The additional field of type 'Maybe (WarningTxt pass)' holds information
-- about export deprecation annotations and is thus set to Nothing when `IE`
-- is used in an import list (since export deprecation can only be used in exports)
type instance XIEThingWith GhcPs = (Maybe (LWarningTxt GhcPs), IEThingWithAnns)
type instance XIEThingWith GhcRn = (Maybe (LWarningTxt GhcRn), IEThingWithAnns)
type instance XIEThingWith GhcTc = IEThingWithAnns

type IEThingWithAnns = (EpToken "(", EpToken "..", EpToken ",", EpToken ")")

-- The additional field of type 'Maybe (WarningTxt pass)' holds information
-- about export deprecation annotations and is thus set to Nothing when `IE`
-- is used in an import list (since export deprecation can only be used in exports)
type instance XIEModuleContents  GhcPs = (Maybe (LWarningTxt GhcPs), EpToken "module")
type instance XIEModuleContents  GhcRn = Maybe (LWarningTxt GhcRn)
type instance XIEModuleContents  GhcTc = NoExtField

type instance XIEGroup           (GhcPass _) = NoExtField
type instance XIEDoc             (GhcPass _) = NoExtField
type instance XIEDocNamed        (GhcPass _) = NoExtField
type instance XXIE               (GhcPass _) = DataConCantHappen

type instance Anno (LocatedA (IE (GhcPass p))) = SrcSpanAnnA

ieLIEWrappedName :: IE (GhcPass p) -> LIEWrappedName (GhcPass p)
ieLIEWrappedName (IEVar _ n _)           = n
ieLIEWrappedName (IEThingAbs  _ n _)     = n
ieLIEWrappedName (IEThingWith _ n _ _ _) = n
ieLIEWrappedName (IEThingAll  _ n _)     = n
ieLIEWrappedName _ = panic "ieLIEWrappedName failed pattern match!"

ieName :: IE (GhcPass p) -> IdP (GhcPass p)
ieName = lieWrappedName . ieLIEWrappedName

ieNames :: IE (GhcPass p) -> [IdP (GhcPass p)]
ieNames (IEVar       _ (L _ n) _)      = [ieWrappedName n]
ieNames (IEThingAbs  _ (L _ n) _)      = [ieWrappedName n]
ieNames (IEThingAll  _ (L _ n) _)      = [ieWrappedName n]
ieNames (IEThingWith _ (L _ n) _ ns _) = ieWrappedName n : map (ieWrappedName . unLoc) ns
ieNames (IEModuleContents {})     = []
ieNames (IEGroup          {})     = []
ieNames (IEDoc            {})     = []
ieNames (IEDocNamed       {})     = []

ieDeprecation :: forall p. IsPass p => IE (GhcPass p) -> Maybe (WarningTxt (GhcPass p))
ieDeprecation = fmap unLoc . ie_deprecation (ghcPass @p)
  where
    ie_deprecation :: GhcPass p -> IE (GhcPass p) -> Maybe (LWarningTxt (GhcPass p))
    ie_deprecation GhcPs (IEVar xie _ _) = xie
    ie_deprecation GhcPs (IEThingAbs xie _ _) = xie
    ie_deprecation GhcPs (IEThingAll (xie, _) _ _) = xie
    ie_deprecation GhcPs (IEThingWith (xie, _) _ _ _ _) = xie
    ie_deprecation GhcPs (IEModuleContents (xie, _) _) = xie
    ie_deprecation GhcRn (IEVar xie _ _) = xie
    ie_deprecation GhcRn (IEThingAbs xie _ _) = xie
    ie_deprecation GhcRn (IEThingAll (xie, _) _ _) = xie
    ie_deprecation GhcRn (IEThingWith (xie, _) _ _ _ _) = xie
    ie_deprecation GhcRn (IEModuleContents xie _) = xie
    ie_deprecation _ _ = Nothing

ieWrappedLName :: IEWrappedName (GhcPass p) -> LIdP (GhcPass p)
ieWrappedLName (IEDefault _ (L l n)) = L l n
ieWrappedLName (IEName    _ (L l n)) = L l n
ieWrappedLName (IEPattern _ (L l n)) = L l n
ieWrappedLName (IEType    _ (L l n)) = L l n
ieWrappedLName (IEData    _ (L l n)) = L l n

ieWrappedName :: IEWrappedName (GhcPass p) -> IdP (GhcPass p)
ieWrappedName = unLoc . ieWrappedLName


lieWrappedName :: LIEWrappedName (GhcPass p) -> IdP (GhcPass p)
lieWrappedName (L _ n) = ieWrappedName n

ieLWrappedName :: LIEWrappedName (GhcPass p) -> LIdP (GhcPass p)
ieLWrappedName (L _ n) = ieWrappedLName n

replaceWrappedName :: IEWrappedName GhcPs -> IdP GhcRn -> IEWrappedName GhcRn
replaceWrappedName (IEDefault r (L l _)) n = IEDefault r (L l n)
replaceWrappedName (IEName    x (L l _)) n = IEName    x (L l n)
replaceWrappedName (IEPattern r (L l _)) n = IEPattern r (L l n)
replaceWrappedName (IEType    r (L l _)) n = IEType    r (L l n)
replaceWrappedName (IEData    r (L l _)) n = IEData    r (L l n)

replaceLWrappedName :: LIEWrappedName GhcPs -> IdP GhcRn -> LIEWrappedName GhcRn
replaceLWrappedName (L l n) n' = L l (replaceWrappedName n n')

exportDocstring :: LHsDoc pass -> SDoc
exportDocstring doc = braces (text "docstring: " <> ppr doc)

instance OutputableBndrId p => Outputable (IE (GhcPass p)) where
    ppr ie@(IEVar       _     var doc) =
      sep $ catMaybes [ ppr <$> ieDeprecation ie
                      , Just $ ppr (unLoc var)
                      , exportDocstring <$> doc
                      ]
    ppr ie@(IEThingAbs  _   thing doc) =
      sep $ catMaybes [ ppr <$> ieDeprecation ie
                      , Just $ ppr (unLoc thing)
                      , exportDocstring <$> doc
                      ]
    ppr ie@(IEThingAll  _   thing doc) =
      sep $ catMaybes [ ppr <$> ieDeprecation ie
                      , Just $ hcat [ppr (unLoc thing)
                      , text "(..)"]
                      , exportDocstring <$> doc
                      ]
    ppr ie@(IEThingWith _ thing wc withs doc) =
      sep $ catMaybes [ ppr <$> ieDeprecation ie
                      , Just $ ppr (unLoc thing) <> parens (fsep (punctuate comma ppWiths))
                      , exportDocstring <$> doc
                      ]
      where
        ppWiths =
          case wc of
              NoIEWildcard ->
                map (ppr . unLoc) withs
              IEWildcard pos ->
                let (bs, as) = splitAt pos (map (ppr . unLoc) withs)
                in bs ++ [text ".."] ++ as
    ppr ie@(IEModuleContents _ mod')
        = sep $ catMaybes [ppr <$> ieDeprecation ie, Just $ text "module" <+> ppr mod']
    ppr (IEGroup _ n _)           = text ("<IEGroup: " ++ show n ++ ">")
    ppr (IEDoc _ doc)             = ppr doc
    ppr (IEDocNamed _ string)     = text ("<IEDocNamed: " ++ string ++ ">")

instance (HasOccName (IdP (GhcPass p)), OutputableBndrId p) => HasOccName (IEWrappedName (GhcPass p)) where
  occName w = occName (ieWrappedName w)

instance OutputableBndrId p => OutputableBndr (IEWrappedName (GhcPass p)) where
  pprBndr bs   w = pprBndr bs   (ieWrappedName w)
  pprPrefixOcc w = pprPrefixOcc (ieWrappedName w)
  pprInfixOcc  w = pprInfixOcc  (ieWrappedName w)

instance OutputableBndrId p => Outputable (IEWrappedName (GhcPass p)) where
  ppr (IEDefault _ (L _ n)) = text "default" <+> pprPrefixOcc n
  ppr (IEName    _ (L _ n)) = pprPrefixOcc n
  ppr (IEPattern _ (L _ n)) = text "pattern" <+> pprPrefixOcc n
  ppr (IEType    _ (L _ n)) = text "type"    <+> pprPrefixOcc n
  ppr (IEData    _ (L _ n)) = text "data"    <+> pprPrefixOcc n

pprImpExp :: (HasOccName name, OutputableBndr name) => name -> SDoc
pprImpExp name = type_pref <+> pprPrefixOcc name
    where
    occ = occName name
    type_pref | isTcOcc occ && isSymOcc occ = text "type"
              | otherwise                   = empty
