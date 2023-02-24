{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module MuridaeApi.ItemTest (module MuridaeApi.ItemTest) where

--import Data.Coerce (coerce)
--import Data.Pool (Pool, withResource)
--import Data.Text (Text)
--import Database.Beam.Postgres (Connection)
--import Database.PostgreSQL.Simple (execute)
--import Effectful (runEff)
--import Effectful.Beam (DbError, runDB)
--import Effectful.Error.Static (runError)
--import Muridae.Environment (MuridaeEnv (MuridaeEnv), mkPool)
--import Muridae.Item qualified as Item
--import Muridae.ItemListing qualified as ItemListing
--import Muridae.JSON.Item
--  ( Item (Item)
--  , ItemDetails (ItemDetails)
--  , ItemId (ItemId)
--  , ReqItem (ReqItem)
--  )
--import Muridae.API.Handler.ItemListing.Types
--  ( CreateItemListing (CreateItemListing)
--  , ItemListing (ItemListing)
--  , ItemListingId (ItemListingId)
--  , ItemListingType (BUY, SELL)
--  )
--import Muridae.API.Handler.User (UserId (UserId))
--import Muridae.JSON.PooledListing
--  ( PooledBuyListing (PooledBuyListing)
--  , PooledSellListing (PooledSellListing)
--  )
--import Muridae.API.Route (APIv1 (publicRoutes), PublicRoutes (items))
--import Muridae.API.Route.Item (getListingsUnderItem, index, show)
--import Muridae.API.Server (mkApplication)
--import Network.HTTP.Client (defaultManagerSettings, newManager)
--import Network.Wai (Application)
--import Network.Wai.Handler.Warp (Port)
--import Network.Wai.Handler.Warp qualified as Warp
--import Servant (NamedRoutes, Proxy (Proxy), Union, WithStatus (WithStatus))
--import Servant.Client
--  ( AsClientT
--  , BaseUrl (baseUrlPort)
--  , ClientError
--  , ClientM
--  , client
--  , matchUnion
--  , mkClientEnv
--  , parseBaseUrl
--  , runClientM
--  , (//)
--  )
--import Test.Hspec (Spec, around, describe, it, runIO, shouldBe)
--import Prelude hiding (show)

----------------------------------------------------------------------------------
---- Move these to a test helper

--mkTestConnPool :: IO (Pool Connection)
--mkTestConnPool =
--  runEff $
--    mkPool
--      "host='localhost' port=5432 dbname='gnawex_test' user='postgres'"
--      5
--      20

--muridaeApp :: Pool Connection -> IO Application
--muridaeApp pool = do
--  pure $ mkApplication (MuridaeEnv pool)

--muridaeClient :: APIv1 (AsClientT ClientM)
--muridaeClient = client (Proxy :: Proxy (NamedRoutes APIv1))

--getItems :: ClientM (Union '[[Item], WithStatus 500 DbError])
--getItems = muridaeClient // publicRoutes // items // index

--showItem
--  :: ItemId
--  -> ClientM
--      ( Union
--          '[ WithStatus 200 ItemDetails
--           , WithStatus 404 String
--           , WithStatus 500 DbError
--           ]
--      )
--showItem = muridaeClient // publicRoutes // items // show

--getItemListings
--  :: ItemId
--  -> Maybe ItemListingType
--  -> ClientM
--      ( Union
--          '[ WithStatus
--              200
--              [ItemListing]
--           , WithStatus 500 DbError
--           ]
--      )
--getItemListings = muridaeClient // publicRoutes // items // getListingsUnderItem

----------------------------------------------------------------------------------

--setup :: (Port -> IO ()) -> IO ()
--setup f = do
--  pool <- mkTestConnPool

--  withResource pool $ \conn -> do
--    execute @[Text] conn "TRUNCATE app.tradable_items, app.users, app.tradable_item_listings RESTART IDENTITY CASCADE" []

--    execute @[Text]
--      conn
--      "INSERT INTO app.users (hunter_id, username, password, role) VALUES (?, ?, ?, ?);"
--      ["1", "sekun", "hunter2", "verified_user"]

--    execute @[Text]
--      conn
--      "INSERT INTO app.users (hunter_id, username, password, role) VALUES (?, ?, ?, ?);"
--      ["2", "deez", "hunter2", "verified_user"]

--  runEff . runDB pool . runError @DbError $ do
--    Item.create_ (ReqItem "Timesplit Rune" "Bruh" "https://mhwiki.com/Timesplit+Rune")
--    Item.create_ (ReqItem "Rare Map Dust" "Bruh" "https://mhwiki.com/Rare+Map+Dust")
--    ItemListing.create (UserId 1) (CreateItemListing (ItemId 1) BUY 1 5 210)
--    ItemListing.create (UserId 1) (CreateItemListing (ItemId 1) BUY 1 5 200)
--    ItemListing.create (UserId 2) (CreateItemListing (ItemId 1) BUY 1 5 210)
--    ItemListing.create (UserId 2) (CreateItemListing (ItemId 1) SELL 1 5 250)
--    ItemListing.create (UserId 1) (CreateItemListing (ItemId 1) SELL 1 9 250)

--  Warp.testWithApplication (muridaeApp pool) f

----------------------------------------------------------------------------------
---- Tests

--spec_itemTests :: Spec
--spec_itemTests =
--  around setup $ do
--    baseUrl <- runIO $ parseBaseUrl "http://localhost"
--    manager <- runIO $ newManager defaultManagerSettings

--    let
--      clientEnv port = mkClientEnv manager (baseUrl {baseUrlPort = port})

--    describe "Item public routes" $ do
--      it "should respond with a list of items" $ \port -> do
--        result <- runClientM getItems (clientEnv port)

--        let
--          result' :: Either ClientError (Maybe [Item])
--          result' = matchUnion @[Item] <$> result

--          items :: Either ClientError (Maybe [(ItemId, Text, Text, Text)])
--          items =
--            (fmap . fmap . fmap)
--              (\(Item itemId name desc link _ _ _) -> (itemId, name, desc, link))
--              result'

--        items
--          `shouldBe` Right
--            ( Just
--                [
--                  ( ItemId 1
--                  , "Timesplit Rune"
--                  , "Bruh"
--                  , "https://mhwiki.com/Timesplit+Rune"
--                  )
--                ,
--                  ( ItemId 2
--                  , "Rare Map Dust"
--                  , "Bruh"
--                  , "https://mhwiki.com/Rare+Map+Dust"
--                  )
--                ]
--            )

--      it "should respond with item's details" $ \port -> do
--        result <- runClientM (showItem (ItemId 1)) (clientEnv port)

--        let
--          result' :: Either ClientError (Maybe (WithStatus 200 ItemDetails))
--          result' = matchUnion @(WithStatus 200 ItemDetails) <$> result

--          itemDetails =
--            (fmap . fmap)
--              ( \(WithStatus (ItemDetails itemId name desc link pooledBuy pooledSell)) ->
--                  (itemId, name, desc, link, pooledBuy, pooledSell)
--              )
--              result'

--        itemDetails
--          `shouldBe` Right
--            ( Just
--                ( ItemId 1
--                , "Timesplit Rune"
--                , "Bruh"
--                , "https://mhwiki.com/Timesplit+Rune"
--                , [PooledBuyListing 210 1 10, PooledBuyListing 200 1 5]
--                , [PooledSellListing 250 1 14]
--                )
--            )

--      it "should respond with item's listings" $ \port -> do
--        result <- runClientM (getItemListings (ItemId 1) Nothing) (clientEnv port)

--        let
--          result' :: Either ClientError (Maybe [ItemListing])
--          result' = (fmap . fmap) coerce (matchUnion @(WithStatus 200 [ItemListing]) <$> result)

--          itemListings =
--            (fmap . fmap . fmap)
--              ( \(ItemListing listingId itemId userId listingType batch qty cost _ _ _) ->
--                  (listingId, itemId, userId, listingType, batch, qty, cost)
--              )
--              result'

--        itemListings
--          `shouldBe` Right
--            ( Just
--                [ (ItemListingId 1, ItemId 1, UserId 1, BUY, 1, 5, 210)
--                , (ItemListingId 2, ItemId 1, UserId 1, BUY, 1, 5, 200)
--                , (ItemListingId 3, ItemId 1, UserId 2, BUY, 1, 5, 210)
--                , (ItemListingId 4, ItemId 1, UserId 2, SELL, 1, 5, 250)
--                , (ItemListingId 5, ItemId 1, UserId 1, SELL, 1, 9, 250)
--                ]
--            )
