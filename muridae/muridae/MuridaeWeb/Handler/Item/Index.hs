module MuridaeWeb.Handler.Item.Index where

import Data.Coerce (coerce)
import Data.Functor.Identity (Identity)
import Effectful.Beam (queryDebug)
import Muridae.Item.Types (
  Item (
    _created_at,
    _deleted_at,
    _description,
    _id,
    _name,
    _updated_at,
    _wiki_link
  ),
  ItemId (ItemId),
 )
import MuridaeWeb.Handler.Item.Types qualified as Handler
import MuridaeWeb.Types (Handler')
import qualified Muridae.Item.Model as ItemModel

indexItems :: Handler' [Handler.TradableItem]
indexItems = queryDebug print ItemModel.all >>= pure . (fmap parseDBItem)
 where
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
