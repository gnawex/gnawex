module Muridae.API.Handler.ItemListing (index) where

import Data.Vector (Vector)
import Effectful (liftIO)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Servant (runUVerb, throwUVerb)
import Hasql.Pool (DbError (DbError))
import Muridae.API.Types (Handler')
import Muridae.DB (UsageError)
import Muridae.ItemListing (runManageItemListingDB)
import Muridae.ItemListing qualified as ItemListing
import Muridae.ItemListing.Types (ItemListingParseError)
import Muridae.JSON.ItemListing qualified as JSON (serializeItemListing)
import Muridae.JSON.ItemListing.Types qualified as JSON
  ( ItemListing
  , ItemListingIndex500 (DbError, ParseError)
  )
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
      runErrorNoCallStack @ItemListingParseError $
        runManageItemListingDB ItemListing.indexItemListings

  runUVerb $ case result of
    (Left usageError) -> do
      liftIO (print usageError)
      (throwUVerb . WithStatus @500 . JSON.DbError . DbError) usageError
    (Right (Left parseError)) ->
      throwUVerb (WithStatus @500 (JSON.ParseError parseError))
    (Right (Right itemListings)) ->
      respond (WithStatus @200 $ JSON.serializeItemListing <$> itemListings)

-- >>= either (throwUVerb . WithStatus @500) (respond . WithStatus @200)

-- -- TODO: Use auth context
-- create
--   :: Maybe UserHandler.UserId
--   -> CreateItemListing
--   -> Handler' (Union '[NoContent, WithStatus 401 String, WithStatus 500 DbError])
-- create userId params = runUVerb $ do
--   case userId of
--     Just userId' ->
--       runErrorNoCallStack @DbError (ItemListing.create userId' params)
--         >>= either (throwUVerb . WithStatus @500) (\_ -> respond NoContent)
--     Nothing ->
--       throwUVerb @(WithStatus 401 String)
--         (WithStatus @401 "Not allowed to do that")

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
