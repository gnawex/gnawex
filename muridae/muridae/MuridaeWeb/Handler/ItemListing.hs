module MuridaeWeb.Handler.ItemListing
  ( getListingsOfItem
  , create
  , updateStatus
  , index
  )
where

import Effectful.Error.Static (runError, throwError)
import Muridae.ItemListing qualified as ItemListing
import MuridaeWeb.Handler.Item.Types (ItemId)
import MuridaeWeb.Handler.ItemListing.Types
  ( CreateItemListing
  , ItemListing
  , ItemListingId
  , ReqStatus
  , ResListingsUnderItem
  )
import MuridaeWeb.Handler.User qualified as UserHandler (UserId)
import MuridaeWeb.Types (Handler')
import Servant (ServerError (ServerError))
import Servant.API.ContentTypes (NoContent (NoContent))
import Effectful.Beam (DbError)

-------------------------------------------------------------------------------
-- Item listing handlers

index :: Handler' [ItemListing]
index = do
  result <- runError @DbError ItemListing.list

  case result of
    Right list -> pure list
    Left (_, _) -> throwError @ServerError (ServerError 500 "" "Unable to connect" [])

-- | Get all the listings under a tradable item
getListingsOfItem :: ItemId -> Handler' ResListingsUnderItem
getListingsOfItem itemId = do
  result <- runError @DbError $ ItemListing.getListingsUnderItem itemId

  case result of
    Right list -> pure list
    Left (_, _) -> throwError @ServerError (ServerError 500 "" "Unable to connect" [])

-- TODO: Use auth context
create
  :: Maybe UserHandler.UserId
  -> CreateItemListing
  -> Handler' NoContent
create userId params =
  case userId of
    Just userId' -> do
      result <- runError @String (ItemListing.create userId' params)

      case result of
        Right _ -> pure NoContent
        Left (_, _) -> throwError @ServerError (ServerError 500 "" "Unable to connect to the DB" [])
    Nothing -> throwError @ServerError (ServerError 401 "No permission" "" [])

updateStatus
  :: Maybe UserHandler.UserId
  -> ItemListingId
  -> ReqStatus
  -> Handler' ItemListing
updateStatus userId listingId params =
  case userId of
    Just userId' -> do
      dbListing <- runError @DbError $ ItemListing.updateStatus userId' listingId params

      case dbListing of
        Left _ -> throwError @ServerError (ServerError 500 "" "Oh no!" [])
        Right dbListing' ->
          case dbListing' of
            Just listing -> pure listing
            Nothing -> throwError @ServerError (ServerError 404 "Item listing not found" "" [])
    -- TODO: Replace (auth context)
    Nothing -> throwError @ServerError (ServerError 401 "No permission" "" [])
