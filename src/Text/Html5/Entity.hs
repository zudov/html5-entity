module Text.Html5.Entity
    ( -- * Entity lookup
      entityCodePoints
    , entityChars
      -- ** Entity validation
    , isValidEntity
      -- * TODO: Entity named lookup
    )where

import           Control.Applicative ((<$>))
import           Data.Char (chr)
import qualified Data.Map as M
import qualified Data.Set as S

import Text.Html5.Entity.Data

-- | Given an entity looks up unicode code points that represent it.
--
-- >>> lookupEntityCodePoints "&mu;"
-- Just [966]
--
-- >>> lookupEntityCodePoints "&nano;"
-- Nothing
entityCodePoints :: String -> Maybe [Int]
entityCodePoints ent = M.lookup ent entityMap

-- | Given an entity returns it's textual representation (if any).
--
-- >>> lookupEntityChars "&mu;"
-- Just "μ"
-- 
-- >>> lookupEntityChars "&nano;"
-- Nothing
entityChars :: String -> Maybe String
entityChars ent = map chr <$> M.lookup ent entityMap

-- | Check if entity is valid (if such entity exists).
--
-- >>> isValidEntity "&mu;"
-- True
--
-- >>> isValidEntity "&nano;"
-- False
isValidEntity :: String -> Bool
isValidEntity ent = S.member ent entitySet


nameToEntity :: String -> String
nameToEntity name = '&' : name ++ ";"
-- | Given a name of entity looks up code points that represent it.
entityNameCodePoints :: String -> Maybe [Int]
entityNameCodePoints = entityCodePoints . nameToEntity

entityNameChars :: String -> Maybe String
entityNameChars = entityChars . nameToEntity

isValidEntityName :: String -> Bool
isValidEntityName name = S.member (nameToEntity name) entitySet
