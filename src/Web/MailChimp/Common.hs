{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
----------------------------------------------------------------------
-- |
-- Module: Web.MailChimp.Common
-- Description:
--
--
--
----------------------------------------------------------------------

module Web.MailChimp.Common
  ( Id
  , ListResponse (..)
  , listResponseFromJSON
  , prefixToJSON
  )
  where

-- aeson
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Aeson as Aeson

-- base
import GHC.Generics

-- text
import Data.Text (Text)


-- |
--
-- An ID.

type Id =
  Text

-- |
--
--

data ListResponse a =
  ListResponse
    { listResponseItems      :: [a]
    , listResponseTotalItems :: Int
    } deriving (Show)

-- |
--
--

listResponseFromJSON :: FromJSON a => Text -> Value -> Parser (ListResponse a)
listResponseFromJSON itemsLabel =
  Aeson.withObject "" $
    \o ->
      ListResponse
        <$> o .: itemsLabel
        <*> o .: "total_items"


-- |
--
--
prefixToJSON :: (Generic a, GToJSON Zero (Rep a)) => String -> a -> Value
prefixToJSON prefix =
  genericToJSON defaultOptions
    { fieldLabelModifier     = camelTo2 '_' . drop (length prefix)
    , constructorTagModifier = camelTo2 '_'
    }
