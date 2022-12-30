module MuridaeWeb.Handler.Item.Index where

import qualified DB.TradableItem as DB.TradableItem
import qualified DB.Types as DB
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity)
import Effectful.Beam (queryDebug)
import qualified MuridaeWeb.Handler.Item.Types as Handler
import MuridaeWeb.Types (Handler')

indexItems :: Handler' [Handler.TradableItem]
indexItems = queryDebug print DB.TradableItem.all >>= pure . (fmap parseDBItem)
 where
  parseDBItem :: DB.TradableItem Identity -> Handler.TradableItem
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
