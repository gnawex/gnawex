{-# OPTIONS_GHC -Wno-orphans #-}

module Muridae.API.QueryParam
  ( Sort (..)
  , IndividualCost (..)
  , UnitQuantity (..)
  )
where

import Data.Text (Text)
import Servant.API

data Sort a = Asc a | Desc a
  deriving stock (Eq, Show)

data IndividualCost = IndividualCost
  deriving stock (Eq, Show)

data UnitQuantity = UnitQuantity

instance FromHttpApiData (Sort IndividualCost) where
  parseQueryParam :: Text -> Either Text (Sort IndividualCost)
  parseQueryParam = \case
    "individual_cost.desc" -> pure (Desc IndividualCost)
    "individual_cost.asc" -> pure (Asc IndividualCost)
    invalid -> Left $ "Unable to parse query param: " <> invalid
