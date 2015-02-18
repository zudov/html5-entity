{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative ((<$>))
import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM

import Language.Haskell.Syntax
import Language.Haskell.Pretty


newtype EntityVal = EntityVal { codepoints :: [Integer] } deriving (Show, Eq, Ord)
instance FromJSON EntityVal where
    parseJSON (Object o) = EntityVal <$> o .: "codepoints"

main = do
    entFile <- BS.readFile "generation/entities.json"
    let Just entMap = (HM.toList . HM.map codepoints) <$> decode entFile
    writeFile "src/Text/Html5/Entity/Data.hs" $ prettyPrint $ mkModule entMap

-- | AST generation
mkModule :: [(String, [Integer])] -> HsModule
mkModule ents = HsModule noloc (Module "Text.Html5.Entity.Data") Nothing imports (decls ents)

imports :: [HsImportDecl]
imports = [ HsImportDecl noloc (Module "Data.Map") True (Just $ Module "M")
                         (Just (False, [ HsIVar $ HsIdent "fromList"
                                       , HsIAbs $ HsIdent "Map"]))
          , HsImportDecl noloc (Module "Data.Set") True (Just $ Module "S")
                         (Just (False, [ HsIVar $ HsIdent "fromList"
                                       , HsIAbs $ HsIdent "Set"]))]

decls :: [(String, [Integer])] -> [HsDecl]
decls ents = 
    [ HsTypeSig noloc [HsIdent "entityMap"] (HsQualType [] (HsTyVar (HsIdent "M.Map String [Int]"))) -- This TyVar is invalid but who cares
    , HsFunBind [HsMatch noloc (HsIdent "entityMap") [] (HsUnGuardedRhs (mkEntityMap ents)) []]

    , HsTypeSig noloc [HsIdent "entitySet"] (HsQualType [] (HsTyVar (HsIdent "S.Set String"))) -- This TyVar is invalid but who cares
    , HsFunBind [HsMatch noloc (HsIdent "entitySet") [] (HsUnGuardedRhs (mkEntitySet ents)) []]
    ]

mkEntityMap :: [(String, [Integer])] -> HsExp
mkEntityMap ents = HsApp (HsVar $ Qual (Module "M") $ HsIdent "fromList")
                         (HsList $ map mkMapElemTup ents)

mkMapElemTup :: (String, [Integer]) -> HsExp
mkMapElemTup (name, codes) =
    HsTuple [ HsLit $ HsString name
            , HsList $ map (HsLit . HsInt) codes
            ] 

mkEntitySet :: [(String, [Integer])] -> HsExp
mkEntitySet ents = HsApp (HsVar $ Qual (Module "S") $ HsIdent "fromList")
                         (HsList $ map (HsLit . HsString . fst) ents)


noloc :: SrcLoc
noloc = SrcLoc "unknown" 0 0
