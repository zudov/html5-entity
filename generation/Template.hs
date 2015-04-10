module Text.Html.Entity.Data where

import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.HashSet (HashSet)
import qualified Data.HashSet as S

import           Data.Text (Text, pack)

entityMap :: HashMap Text [Int]
entitySet :: HashSet Text
