{-# LANGUAGE OverloadedStrings #-}
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
  )
  where

-- aeson
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Aeson as Aeson

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

