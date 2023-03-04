module Muridae.API.Handler.ItemListing
  ( index
  , create
  , update
  )
where

-------------------------------------------------------------------------------

import Data.Coerce (coerce)
import Data.Int (Int32)
import Data.Maybe (fromJust)
import Data.Vector (Vector)
import Effectful (Eff, liftIO, (:>))
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Servant (runUVerb, throwUVerb)
import GHC.Natural (Natural)
import Muridae.API.QueryParam (IndividualCost, Sort (Asc, Desc))
import Muridae.API.Types (Handler')
import Muridae.DB (DB, UsageError)
import Muridae.Item.Id qualified as Domain
import Muridae.ItemListing (runManageItemListingDB)
import Muridae.ItemListing qualified as ItemListing
import Muridae.ItemListing.Id qualified as Domain
import Muridae.ItemListing.Types qualified as Domain
import Muridae.JSON.DbError (DbError (DbError))
import Muridae.JSON.Item.Id qualified as JSON
import Muridae.JSON.ItemListing qualified as JSON
import Muridae.JSON.ItemListing.Types qualified as JSON
import Muridae.JSON.User qualified as JSON
import Muridae.User.Id qualified as Domain
import Servant (Union, WithStatus (WithStatus), respond)

-------------------------------------------------------------------------------
-- Item listing handlers

index
  :: Maybe (Sort IndividualCost)
  -> Maybe JSON.ItemId
  -> Maybe Bool
  -> Handler'
      ( Union
          '[ WithStatus 200 (Vector JSON.ItemListing)
           , WithStatus 500 JSON.ItemListingIndex500
           ]
      )
index sortCost itemId isActive = do
  let
    itemId' = case itemId of
      Just (JSON.ItemId i) -> Domain.FilterByItemId (Domain.ItemId i)
      Nothing -> Domain.NoItemIdFilter

    orderByIndividualCost = case sortCost of
      Just (Asc _) -> Domain.Asc
      Just (Desc _) -> Domain.Desc
      Nothing -> Domain.Unordered

    listingStatus = case isActive of
      Just True -> Domain.Listed
      Just False -> Domain.Delisted
      Nothing -> Domain.ListedAndDelisted

  liftIO $ print sortCost
  result <-
    runErrorNoCallStack @UsageError $
      runErrorNoCallStack @Domain.ItemListingParseError $
        runManageItemListingDB $
          ItemListing.indexItemListings
            orderByIndividualCost
            itemId'
            listingStatus

  runUVerb $ case result of
    (Left usageError) -> do
      liftIO (print usageError)
      (throwUVerb . WithStatus @500 . JSON.IndexDbError . DbError) usageError
    (Right (Left parseError)) ->
      ( throwUVerb
          . WithStatus @500
          . JSON.IndexParseError
          . JSON.MkItemListingParseError'
      )
        parseError
    (Right (Right itemListings)) ->
      respond (WithStatus @200 $ JSON.serializeItemListing <$> itemListings)

-- -- TODO: Use auth context
create
  :: Maybe JSON.UserId
  -> JSON.CreateItemListing
  -> Handler'
      ( Union
          '[ JSON.ItemListing
           , WithStatus 401 String
           , WithStatus 500 JSON.ItemListingCreate500
           ]
      )
create userId params = runUVerb $ do
  case userId of
    Nothing ->
      throwUVerb @(WithStatus 401 String) (WithStatus @401 "Not allowed to do that")
    Just userId' -> do
      -- TODO: Check if item with the given ID actually exists
      result <-
        runCreate
          (coerce userId')
          (coerce params.item_id)
          (JSON.parseItemListingType params.listing_type)
          -- NOTE: This is fine because `CreateItemListing` checks if these are
          -- greater than zero.
          (fromJust $ Domain.mkBatchedBy params.batched_by)
          (fromJust $ Domain.mkUnitQuantity params.unit_quantity)
          (fromJust $ Domain.mkCost params.cost)

      case result of
        (Left usageError) -> do
          liftIO (print usageError)
          (throwUVerb . WithStatus @500 . JSON.CreateDbError . DbError) usageError
        (Right (Left parseError)) ->
          ( throwUVerb
              . WithStatus @500
              . JSON.CreateParseError
              . JSON.MkItemListingParseError'
          )
            parseError
        (Right (Right itemListing)) ->
          respond (JSON.serializeItemListing itemListing)
 where
  runCreate
    :: DB :> es
    => Domain.UserId
    -> Domain.ItemId
    -> Domain.ItemListingType
    -> Domain.BatchedBy
    -> Domain.UnitQuantity
    -> Domain.Cost
    -> Eff
        es
        ( Either
            UsageError
            ( Either Domain.ItemListingParseError Domain.ItemListing
            )
        )
  runCreate userId' itemId listingType batchedBy unitQuantity cost =
    runErrorNoCallStack @UsageError $
      runErrorNoCallStack @Domain.ItemListingParseError $
        runManageItemListingDB
          ( ItemListing.createItemListing
              userId'
              itemId
              listingType
              batchedBy
              unitQuantity
              cost
          )

update
  :: Maybe JSON.UserId
  -> JSON.ItemListingId
  -> JSON.UpdateItemListing
  -> Handler'
      ( Union
          '[ JSON.ItemListing
           , WithStatus 201 JSON.ItemListing
           , -- TODO: Replace @String@ with something else more specific
             WithStatus 401 String
           , WithStatus 404 String
           , WithStatus 500 JSON.ItemListingUpdate500
           ]
      )
update userId listingId params = runUVerb $
  case userId of
    Just userId' -> do
      result <-
        runErrorNoCallStack @UsageError
          . runErrorNoCallStack @Domain.ItemListingParseError
          . runManageItemListingDB
          $ ItemListing.updateItemListing
            (coerce userId')
            (coerce listingId)
            ( fromJust
                . Domain.mkUnitQuantity
                . fromIntegral @Natural @Int32
                <$> params.unit_quantity
            )
            params.active

      case result of
        (Left usageError) -> do
          liftIO (print usageError)
          (throwUVerb . WithStatus @500 . JSON.UpdateDbError . DbError) usageError
        (Right (Left parseError)) ->
          ( throwUVerb
              . WithStatus @500
              . JSON.UpdateParseError
              . JSON.MkItemListingParseError'
          )
            parseError
        (Right (Right itemListing)) -> do
          liftIO (print itemListing)
          maybe
            (throwUVerb (WithStatus @404 @String "Item listing does not exist"))
            (respond . JSON.serializeItemListing)
            itemListing
    Nothing -> throwUVerb (WithStatus @401 @String "Not allowed to do that")
