module MuridaeWeb.Handler.ItemListing
  ( getListingsOfItem
  , create
  , updateStatus
  , index
  )
where

import Control.Exception (ErrorCall)
import Control.Monad.Catch (catch)
import Effectful.Error.Static (throwError)
import Muridae.ItemListing qualified as ItemListing
import MuridaeWeb.Handler.Item.Types (TradableItemId)
import MuridaeWeb.Handler.ItemListing.Types
  ( CreateTradableItemListing
  , ReqStatus
  , ResListingsUnderItem
  , TradableItemListing
  , TradableItemListingId
  )
import MuridaeWeb.Handler.User qualified as UserHandler (UserId)
import MuridaeWeb.Types (Handler')
import Servant (ServerError (ServerError))
import Servant.API.ContentTypes (NoContent (NoContent))

-------------------------------------------------------------------------------
-- Item listing handlers

index :: Handler' [TradableItemListing]
index = ItemListing.list

-- | Get all the listings under a tradable item
getListingsOfItem :: TradableItemId -> Handler' ResListingsUnderItem
getListingsOfItem = ItemListing.getListingsUnderItem -- TODO: Throw 404

-- TODO: Use auth context
create
  :: Maybe UserHandler.UserId
  -> CreateTradableItemListing
  -> Handler' NoContent
create userId params =
  case userId of
    Just userId' -> do
      _ <-
        ItemListing.create userId' params
          `catch` \(_ :: ErrorCall) -> throwError @ServerError (ServerError 500 "Uh oh" "" [])
      pure NoContent
    Nothing -> throwError @ServerError (ServerError 401 "No permission" "" [])

updateStatus
  :: Maybe UserHandler.UserId
  -> TradableItemListingId
  -> ReqStatus
  -> Handler' TradableItemListing
updateStatus userId listingId params =
  case userId of
    Just userId' -> do
      dbListing <- ItemListing.updateStatus userId' listingId params

      case dbListing of
        Just dbListing' -> pure dbListing'
        Nothing -> throwError @ServerError (ServerError 404 "Item listing not found" "" [])
    -- TODO: Replace (auth context)
    Nothing -> throwError @ServerError (ServerError 401 "No permission" "" [])
