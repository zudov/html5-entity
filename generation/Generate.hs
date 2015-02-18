{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative ((<$>))
import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Lazy as HM

import Language.Haskell.Syntax
import Language.Haskell.Pretty
import Language.Haskell.Parser


newtype EntityVal = EntityVal { codepoints :: [Integer] } deriving (Show, Eq, Ord)
instance FromJSON EntityVal where
    parseJSON (Object o) = EntityVal <$> o .: "codepoints"
    parseJSON _ = error "Not an object"

main :: IO ()
main = do
    entFile <- BS.readFile "generation/entities.json"
    let Just entMap = HM.toList . HM.map codepoints <$> decode entFile
    ParseOk template <- parseModule <$> readFile "generation/Template.hs"
    writeFile "src/Text/Html5/Entity/Data.hs" $ prettyPrint 
                                              $ appendTemplate template entMap

-- | AST generation
appendTemplate :: HsModule -> [(String, [Integer])] -> HsModule
appendTemplate (HsModule srcLoc modName exports imports decls') ents =
    HsModule srcLoc modName exports imports (decls' ++ decls ents)


decls :: [(String, [Integer])] -> [HsDecl]
decls ents =
    [ HsFunBind [HsMatch noloc (HsIdent "entityMap") []
                               (HsUnGuardedRhs (mkEntityMap ents)) []]
    , HsFunBind [HsMatch noloc (HsIdent "entitySet") []
                               (HsUnGuardedRhs (mkEntitySet ents)) []]
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
noloc = SrcLoc "" 0 0
