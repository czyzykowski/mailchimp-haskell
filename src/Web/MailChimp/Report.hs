{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.MailChimp.Report
  ( ReportApi
  , ReportClient (..)
  , Report (..)
  , ClickDetail (..)
  ) where

-- aeson
import Data.Aeson
import Data.Aeson.Types

-- base
import GHC.Generics

-- generics-sop
import Generics.SOP

-- mailchimp
import Web.MailChimp.Common
import Web.MailChimp.Campaign
import Web.MailChimp.List

-- servant
import Servant.API

-- servant-client
import Servant.Client
import Servant.Client.Generic

-- text
import Data.Text (Text)


type Bounces       = Object
type Forwards      = Object
type Opens         = Object
type Clicks        = Object
type FacebookLikes = Object
type IndustryStats = Object
type ABSplitStats  = Object
type TimewarpStats = Object
type Timeseries    = Object
type ShareReport   = Object
type Ecommerce     = Object

-- |
--
--

data Report =
  Report
    {
      reportId             :: CampaignId
    , reportCampaignTitle  :: Text
    , reportType           :: Text
    , reportListId         :: ListId
    , reportListName       :: Text
    , reportSubjectLine    :: Text
    , reportEmailsSent     :: Int
    , reportAbuseReports   :: Int
    , reportUnsubscribed   :: Int
    , reportSendTime       :: Text
    , reportRSSLastSend    :: Maybe Text
    , reportBounces        :: Bounces
    , reportForwards       :: Forwards
    , reportOpens          :: Opens
    , reportClicks         :: Clicks
    , reportFacebookLikes  :: FacebookLikes
    , reportIndustryStats  :: IndustryStats
    , reportListStats      :: IndustryStats
    , reportABSplit        :: Maybe ABSplitStats
    , reportTimewarp       :: Maybe TimewarpStats
    , reportTimeseries     :: [Timeseries]
    , reportShareReport    :: ShareReport
    , reportEcommerce      :: Ecommerce
    , reportDeliveryStatus :: CampaignDeliveryStatus
    }
  deriving (Show, GHC.Generics.Generic)

-- |
--
--

instance FromJSON Report where
  parseJSON =
    genericParseJSON defaultOptions
      { fieldLabelModifier     = camelTo2 '_' . drop 6
      , constructorTagModifier = camelTo2 '_'
      }


-- |
--
--

instance FromJSON (ListResponse Report) where
  parseJSON =
    listResponseFromJSON "reports"


-- |
--
--

instance ToJSON Report where
  toJSON = prefixToJSON "report"



type ABSplit = Object
type LinkId = Id


-- |
--
--

data ClickDetail =
  ClickDetail
    { clickDetailId                    :: LinkId
    , clickDetailURL                   :: Text
    , clickDetailTotalClicks           :: Int
    , clickDetailClickPercentage       :: Double
    , clickDetailUniqueClicks          :: Int
    , clickDetailUniqueClickPercentage :: Double
    , clickDetailLastClick             :: Text
    , clickDetailABSplit               :: Maybe ABSplit
    , clickDetailCampaignId            :: CampaignId
    } deriving (Show, GHC.Generics.Generic)


-- |
--
--

instance FromJSON ClickDetail where
  parseJSON =
    genericParseJSON defaultOptions
      { fieldLabelModifier     = camelTo2 '_' . drop 11
      , constructorTagModifier = camelTo2 '_'
      }

-- |
--
--

instance ToJSON ClickDetail where
  toJSON =
    prefixToJSON "clickDetail"


-- |
--
--

instance FromJSON (ListResponse ClickDetail) where
  parseJSON =
    listResponseFromJSON "urls_clicked"


-- |
--
--

type GetObjectList a =
  QueryParam "count" Int
    :> QueryParam "offset" Int
    :> Get '[JSON] (ListResponse a)

-- |
--
--

type ReportApi =
    GetObjectList Report

  :<|>
    Capture "campaign_id" CampaignId
      :> Get '[JSON] Report

  :<|>
    Capture "campaign_id" CampaignId
      :> "click-details"
      :> GetObjectList ClickDetail

-- |
--
--

data ReportClient =
  ReportClient
    { -- |
      --
      -- Get all reports

      getReports
        :: Maybe Int -- count
        -> Maybe Int -- offset
        -> ClientM (ListResponse Report)

      -- |
      --
      -- Get report for a given campaign.

    ,  getReport
        :: CampaignId
        -> ClientM Report

      -- |
      --
      -- Get detailed information about links clicked in campaigns.

    , getReportClickDetails
        :: CampaignId
        -> Maybe Int
        -> Maybe Int
        -> ClientM (ListResponse ClickDetail)

    }
  deriving (GHC.Generics.Generic)

-- |
--
--

instance Generics.SOP.Generic ReportClient

-- |
--
--

instance (Client ReportApi ~ client) => ClientLike client ReportClient
