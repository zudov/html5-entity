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
imports = [HsImportDecl noloc (Module "Data.Map") False Nothing 
                        (Just (False, [ HsIVar $ HsIdent "fromList"
                                      , HsIAbs $ HsIdent "Map"]))]

decls :: [(String, [Integer])] -> [HsDecl]
decls ents = 
    [ HsTypeSig noloc [HsIdent "entities"] (HsQualType [] (HsTyVar (HsIdent "Map String [Int]"))) -- This TyVar is invalid but who cares
    , HsFunBind [HsMatch noloc (HsIdent "entities") [] (HsUnGuardedRhs (mkEntityMap ents)) []]]

mkEntityMap :: [(String, [Integer])] -> HsExp
mkEntityMap ents =
    HsApp (HsVar $ UnQual $ HsIdent "fromList") (HsList $ map mkMapElemTup ents)

mkMapElemTup :: (String, [Integer]) -> HsExp
mkMapElemTup (name, codes) =
    HsTuple [ HsLit $ HsString name
            , HsList $ map (HsLit . HsInt) codes
            ] 

noloc :: SrcLoc
noloc = SrcLoc "unknown" 0 0
