{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.MailChimp.Campaign
  ( CampaignApi
  , CampaignClient (..)
  , CampaignId
  , Campaign (..)
  , CampaignDeliveryStatus
  ) where


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
--

type CampaignId = Id


type CampaignSettings        = Object
type List                    = Object
type ABTestOptions           = Object
type CampaignTrackingOptions = Object
type RSSOptions              = Object
type CampaignSocialCard      = Object
type CampaignReportSummary   = Object
type CampaignDeliveryStatus  = Object


-- |
--
--

data Campaign =
  Campaign
    { campaignId              :: CampaignId
    , campaignWebId           :: Int
    , campaignType            :: Text
    , campaignCreateTime      :: Text
    , campaignArchiveURL      :: Text
    , campaignLongArchiveURL  :: Text
    , campaignStatus          :: Text
    , campaignEmailsSent      :: Int
    , campaignSendTime        :: Text
    , campaignContentType     :: Text
    , campaignRecipients      :: List
    , campaignSettings        :: CampaignSettings
    , campaignVariateSettings :: Maybe ABTestOptions
    , campaignTracking        :: CampaignTrackingOptions
    , campaignRSSOpts         :: Maybe RSSOptions
    , campaignSocialCard      :: Maybe CampaignSocialCard
    , campaignReportSummary   :: Maybe CampaignReportSummary
    , campaignDeliveryStatus  :: CampaignDeliveryStatus
    }
  deriving (Show, GHC.Generics.Generic)


-- |
--
--

instance FromJSON Campaign where
  parseJSON =
    genericParseJSON defaultOptions
      { fieldLabelModifier     = camelTo2 '_' . drop 8
      , constructorTagModifier = camelTo2 '_'
      }


-- |
--
--

instance FromJSON (ListResponse Campaign) where
  parseJSON =
    listResponseFromJSON "campaigns"


-- |
--
--

type CampaignApi =
     QueryParam "count" Int
  :> QueryParam "offset" Int
  :> Get '[JSON] (ListResponse Campaign)


-- |
--
--

data CampaignClient =
  CampaignClient
    {
      getCampaigns
        :: Maybe Int -- count
        -> Maybe Int -- offset
        -> ClientM (ListResponse Campaign)
    }
  deriving (GHC.Generics.Generic)


-- |
--
--

instance Generics.SOP.Generic CampaignClient


-- |
--
--

instance (Client CampaignApi ~ client) => ClientLike client CampaignClient
