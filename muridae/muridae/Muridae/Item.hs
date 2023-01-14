module Muridae.Item (list, create_) where

import Data.Coerce (coerce)
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity)
import Effectful (Eff, type (:>))
import Effectful.Beam (DB, queryDebug)
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
import MuridaeWeb.Handler.Item.Types qualified as Handler

list :: (DB :> es) => Eff es [Handler.TradableItem]
list = queryDebug putStrLn ItemModel.all <&> fmap parseDBItem

create_ :: (DB :> es) => Handler.ReqTradableItem -> Eff es ()
create_ params =
  queryDebug putStrLn (ItemModel.create params)

parseDBItem :: Item Identity -> Handler.TradableItem
parseDBItem dbItem =
  Handler.TradableItem
    { id = coerce dbItem._id
    , name = dbItem._name
    , description = dbItem._description
    , wiki_link = dbItem._wiki_link
    , created_at = dbItem._created_at
    , updated_at = dbItem._updated_at
    , deleted_at = dbItem._deleted_at
    }
