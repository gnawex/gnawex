module MuridaeWeb.Handler.ItemListing
  ( getListingsOfItem
  , create
  , updateStatus
  , index
  )
where

import Effectful (Eff, (:>))
import Effectful.Beam (DB, DbError)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.Servant (runUVerb, throwUVerb)
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
import Servant (Union, WithStatus (WithStatus), respond)
import Servant.API.ContentTypes (NoContent (NoContent))

-------------------------------------------------------------------------------
-- Item listing handlers

index
  :: Handler'
      ( Union
          '[ WithStatus 200 [ItemListing]
           , WithStatus 500 DbError
           ]
      )
index =
  runUVerb $
    runErrorNoCallStack @DbError ItemListing.list
      >>= either (throwUVerb . WithStatus @500) (respond . WithStatus @200)

-- | Get all the listings under a tradable item
getListingsOfItem
  :: ItemId
  -> Handler'
      ( Union
          '[ ResListingsUnderItem
           , WithStatus 500 DbError
           ]
      )
getListingsOfItem itemId =
  runUVerb $
    runErrorNoCallStack @DbError (ItemListing.getListingsUnderItem itemId)
      >>= either (throwUVerb . WithStatus @500) respond

-- TODO: Use auth context
create
  :: Maybe UserHandler.UserId
  -> CreateItemListing
  -> Handler' (Union '[NoContent, WithStatus 401 String, WithStatus 500 DbError])
create userId params = runUVerb $ do
  case userId of
    Just userId' ->
      runErrorNoCallStack @DbError (ItemListing.create userId' params)
        >>= either (throwUVerb . WithStatus @500) (\_ -> respond NoContent)
    Nothing ->
      throwUVerb @(WithStatus 401 String)
        (WithStatus @401 "Not allowed to do that")

updateStatus
  :: Maybe UserHandler.UserId
  -> ItemListingId
  -> ReqStatus
  -> Handler'
      ( Union
          '[ ItemListing
           , WithStatus 401 String
           , WithStatus 404 String
           , WithStatus 500 DbError
           ]
      )
updateStatus userId listingId params =
  -- TODO: Replace (auth context)
  -- Nothing ->
  --   throwUVerb @(WithStatus 401 String) (WithStatus @401 "Not allowed to do that")
  runUVerb $
    maybe
      (throwUVerb @(WithStatus 401 String) (WithStatus @401 "Not allowed to do that"))
      (doUpdate listingId params)
      userId
 where
  doUpdate
    :: (DB :> es)
    => ItemListingId
    -> ReqStatus
    -> UserHandler.UserId
    -> Eff
        ( Error
            ( Union
                '[ ItemListing
                 , WithStatus 401 String
                 , WithStatus 404 String
                 , WithStatus 500 DbError
                 ]
            )
            : es
        )
        ( Union
            '[ ItemListing
             , WithStatus 401 String
             , WithStatus 404 String
             , WithStatus 500 DbError
             ]
        )
  doUpdate listingId' params' userId' =
    runErrorNoCallStack @DbError (ItemListing.updateStatus userId' listingId' params')
      >>= either
        ( throwUVerb @(WithStatus 500 DbError)
            @[ ItemListing
             , WithStatus 401 String
             , WithStatus 404 String
             , WithStatus 500 DbError
             ]
            . WithStatus @500
        )
        ( maybe
            ( throwUVerb
                @(WithStatus 404 String)
                (WithStatus @404 "Item listing does not exist")
            )
            respond
        )
