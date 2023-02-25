{-# LANGUAGE ScopedTypeVariables #-}

module Muridae.API.Handler.ItemListing (index, create) where

-------------------------------------------------------------------------------

import Data.Coerce (coerce)
import Data.Maybe (fromJust)
import Data.Vector (Vector)
import Effectful (Eff, liftIO, (:>))
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Servant (runUVerb, throwUVerb)
import Muridae.API.Types (Handler')
import Muridae.DB (DB, UsageError)
import Muridae.Item.Id qualified as Domain
import Muridae.ItemListing (runManageItemListingDB)
import Muridae.ItemListing qualified as ItemListing
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
  :: Handler'
      ( Union
          '[ WithStatus 200 (Vector JSON.ItemListing)
           , WithStatus 500 JSON.ItemListingIndex500
           ]
      )
index = do
  result <-
    runErrorNoCallStack @UsageError $
      runErrorNoCallStack @Domain.ItemListingParseError $
        runManageItemListingDB ItemListing.indexItemListings

  runUVerb $ case result of
    (Left usageError) -> do
      liftIO (print usageError)
      (throwUVerb . WithStatus @500 . JSON.IndexDbError . DbError) usageError
    (Right (Left parseError)) ->
      (throwUVerb . WithStatus @500 . JSON.IndexParseError) parseError
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
          (throwUVerb . WithStatus @500 . JSON.CreateParseError) parseError
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

-- updateStatus
--   :: Maybe UserHandler.UserId
--   -> ItemListingId
--   -> ReqStatus
--   -> Handler'
--       ( Union
--           '[ ItemListing
--            , WithStatus 401 String
--            , WithStatus 404 String
--            , WithStatus 500 DbError
--            ]
--       )
-- updateStatus userId listingId params =
--   -- TODO: Replace (auth context)
--   -- Nothing ->
--   --   throwUVerb @(WithStatus 401 String) (WithStatus @401 "Not allowed to do that")
--   runUVerb $
--     maybe
--       (throwUVerb @(WithStatus 401 String) (WithStatus @401 "Not allowed to do that"))
--       (doUpdate listingId params)
--       userId
--  where
--   doUpdate
--     :: (DB :> es)
--     => ItemListingId
--     -> ReqStatus
--     -> UserHandler.UserId
--     -> Eff
--         ( Error
--             ( Union
--                 '[ ItemListing
--                  , WithStatus 401 String
--                  , WithStatus 404 String
--                  , WithStatus 500 DbError
--                  ]
--             )
--             : es
--         )
--         ( Union
--             '[ ItemListing
--              , WithStatus 401 String
--              , WithStatus 404 String
--              , WithStatus 500 DbError
--              ]
--         )
--   doUpdate listingId' params' userId' =
--     runErrorNoCallStack @DbError (ItemListing.updateStatus userId' listingId' params')
--       >>= either
--         ( throwUVerb @(WithStatus 500 DbError)
--             @[ ItemListing
--              , WithStatus 401 String
--              , WithStatus 404 String
--              , WithStatus 500 DbError
--              ]
--             . WithStatus @500
--         )
--         ( maybe
--             ( throwUVerb
--                 @(WithStatus 404 String)
--                 (WithStatus @404 "Item listing does not exist")
--             )
--             respond
--         )
--
