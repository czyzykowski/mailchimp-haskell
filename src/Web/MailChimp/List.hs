{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

----------------------------------------------------------------------
-- |
-- Module: Web.MailChimp.List
-- Description:
--
--
--
----------------------------------------------------------------------

module Web.MailChimp.List
  ( ListApi
  , ListClient (..)
  , ListId
  , List (..)
  )
  where

-- aeson
import Data.Aeson.Types

-- base
import GHC.Generics

-- generics-sop
import Generics.SOP

-- mailchimp
import Web.MailChimp.Common

-- servant
import Servant.API

-- servant-client
import Servant.Client
import Servant.Client.Generic

-- text
import Data.Text (Text)


-- |
--
-- A list ID.

type ListId =
  Id

type Contact = Object
type CampaignDefaults = Object
type Statistics = Object


data List =
  List
    { listId                  :: ListId
    , listWebId               :: Int
    , listName                :: Text
    , listContact             :: Contact
    , listPermissionReminder  :: Text
    , listUseArchiveBar       :: Bool
    , listCampaignDefaults    :: CampaignDefaults
    , listNotifyOnSubscribe   :: Text
    , listNotifyOnUnsubscribe :: Text
    , listDateCreated         :: Text
    , listListRating          :: Int
    , listEmailTypeOption     :: Bool
    , listSubscribeUrlShort   :: Text
    , listSubscribeUrlLong    :: Text
    , listBeamerAddress       :: Text
    , listVisibility          :: Text
    , listModules             :: [Text]
    , listStats               :: Statistics
    } deriving (Show, GHC.Generics.Generic)

-- |
--
--

instance FromJSON List where
  parseJSON =
    genericParseJSON defaultOptions
      { fieldLabelModifier     = camelTo2 '_' . drop (length ("list" :: String))
      , constructorTagModifier = camelTo2 '_'
      }

-- |
--
--

instance FromJSON (ListResponse List) where
  parseJSON =
    listResponseFromJSON "lists"

-- |
--
--

instance ToJSON List where
  toJSON =
    prefixToJSON "list"

-- |
--
--

type ListApi =
     QueryParam "count" Int
  :> QueryParam "offset" Int
  :> Get '[JSON] (ListResponse List)


-- |
--
-- A client for a list.

data ListClient =
  ListClient
    { -- |
      --
      --

      getLists
        :: Maybe Int
        -> Maybe Int
        -> ClientM (ListResponse List)

    }
  deriving (GHC.Generics.Generic)


-- |
--
--
instance Generics.SOP.Generic ListClient


-- |
--
--

instance (Client ListApi ~ client) => ClientLike client ListClient
