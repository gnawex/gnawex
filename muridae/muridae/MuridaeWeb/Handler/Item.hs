module MuridaeWeb.Handler.Item (module MuridaeWeb.Handler.Item) where

import Effectful.Beam (DbError)
import Effectful.Error.Static (runError, throwError)
import Muridae.Item qualified as Item
import MuridaeWeb.Handler.Item.Types (Item)
import MuridaeWeb.Handler.Item.Types qualified as Handler
import MuridaeWeb.Types (Handler')
import Servant (ServerError (ServerError))
import Servant.API.ContentTypes (NoContent (NoContent))

create :: Handler.ReqItem -> Handler' NoContent
create params = do
  result <- runError @DbError $ Item.create_ params

  case result of
    Left _ -> throwError @ServerError (ServerError 500 "" "" [])
    Right _ -> pure NoContent

indexItems :: Handler' [Item]
indexItems = do
  result <- runError @DbError Item.list

  case result of
    Left _ -> throwError @ServerError (ServerError 500 "" "" [])
    Right list -> pure list
