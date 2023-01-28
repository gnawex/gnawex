module Muridae.Item (list, create_, findDetails_) where

import Data.Coerce (coerce)
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity)
import Effectful (Eff, type (:>))
import Effectful.Beam (DB, DbError, queryDebug)
import Effectful.Error.Static (Error)
import Muridae.Item.Model qualified as ItemModel
import Muridae.Item.Types
  ( Item
      ( _created_at
      , _deleted_at
      , _description
      , _id
      , _name
      , _updated_at
      , _wiki_link
      )
  , ItemId (ItemId)
  )
import Muridae.ItemListing.Model qualified as ItemListingModel
import Muridae.ItemListing.Types
  ( PooledBuyListing
  , PooledSellListing
  , mkPooledBuyListing
  , mkPooledSellListing
  )
import MuridaeWeb.Handler.Item.Types qualified as Handler

--------------------------------------------------------------------------------

list :: (DB :> es) => Eff (Error DbError : es) [Handler.Item]
list = queryDebug putStrLn ItemModel.all <&> fmap parseDBItem

create_ :: (DB :> es) => Handler.ReqItem -> Eff (Error DbError : es) ()
create_ params =
  queryDebug putStrLn (ItemModel.create params)


-- | Finds an item's details including its pooled buy and sell listings
findDetails_
  :: (DB :> es)
  => Handler.ItemId
  -> Eff
      (Error DbError : es)
      (Maybe (Item Identity, [PooledBuyListing], [PooledSellListing]))
findDetails_ itemId =
  queryDebug putStrLn $ do
    item <- ItemModel.find itemId

    (pooledBuys, pooledSells) <-
      ItemListingModel.getListingsUnderItem (coerce itemId)

    let
      pooledBuys' = mapM mkPooledBuyListing pooledBuys
      pooledSells' = mapM mkPooledSellListing pooledSells

    pure $ pure (,,) <*> item <*> pooledBuys' <*> pooledSells'

--------------------------------------------------------------------------------

parseDBItem :: Item Identity -> Handler.Item
parseDBItem dbItem =
  Handler.Item
    { id = coerce dbItem._id
    , name = dbItem._name
    , description = dbItem._description
    , wiki_link = dbItem._wiki_link
    , created_at = dbItem._created_at
    , updated_at = dbItem._updated_at
    , deleted_at = dbItem._deleted_at
    }
