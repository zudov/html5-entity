{-# LANGUAGE OverloadedStrings #-}
module Text.Html.Entity
    ( -- * Entity lookup
      entityCodePoints
    , entityChars
      -- ** Entity validation
    , isValidEntity
      -- * Entity lookup by name
    , entityNameCodePoints
    , entityNameChars
      -- ** Entity name validation
    , isValidEntityName
    )where

import           Control.Applicative ((<$>))
import           Data.Char (chr)
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Text (Text, pack)
import           Data.Monoid ((<>))

import Text.Html.Entity.Data

-- | Given an entity looks up unicode code points that represent it.
--
-- >>> entityCodePoints "&mu;"
-- Just [966]
--
-- >>> entityCodePoints "&nano;"
-- Nothing
entityCodePoints :: Text -> Maybe [Int]
entityCodePoints ent = M.lookup ent entityMap

-- | Given an entity returns it's textual representation (if any).
--
-- >>> entityChars "&mu;"
-- Just "μ"
-- 
-- >>> entityChars "&nano;"
-- Nothing
entityChars :: Text -> Maybe Text
entityChars ent = pack <$> map chr <$> M.lookup ent entityMap

-- | Check if entity is valid (if such entity exists).
--
-- >>> isValidEntity "&mu;"
-- True
--
-- >>> isValidEntity "&nano;"
-- False
isValidEntity :: Text -> Bool
isValidEntity ent = S.member ent entitySet


nameToEntity :: Text -> Text
nameToEntity name = "&" <> name <> ";"

-- | Given a name of entity looks up code points that represent it.
--
-- >>> entityNameCodePoints "mu"
-- Just [966]
--
-- >>> entityNameCodePoints "nano"
-- Nothing
entityNameCodePoints :: Text -> Maybe [Int]
entityNameCodePoints = entityCodePoints . nameToEntity

-- | Given a name of entity returns it's textual representation (if any).
--
-- >>> entityNameChars "mu"
-- Just "μ"
-- 
-- >>> entityNameChars "nano"
-- Nothing
entityNameChars :: Text -> Maybe Text
entityNameChars = entityChars . nameToEntity

-- | Check if entity name is valid (if corresponding entity exists).
--
-- >>> isValidEntityName "mu"
-- True
--
-- >>> isValidEntityName "nano"
-- False
isValidEntityName :: Text -> Bool
isValidEntityName = isValidEntity . nameToEntity
