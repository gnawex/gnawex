module MuridaeWeb.Handler.Item (module MuridaeWeb.Handler.Item) where

import Effectful.Beam (DbError)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Servant (runUVerb, throwUVerb)
import Muridae.Item qualified as Item
import MuridaeWeb.Handler.Item.Types (Item)
import MuridaeWeb.Handler.Item.Types qualified as Handler
import MuridaeWeb.Types (Handler')
import Servant
  ( Union
  , WithStatus (WithStatus)
  , respond
  )
import Servant.API.ContentTypes (NoContent (NoContent))

create
  :: Handler.ReqItem -> Handler' (Union '[WithStatus 201 NoContent, WithStatus 500 DbError])
create params =
  runUVerb $
    runErrorNoCallStack @DbError (Item.create_ params)
      >>= either
        (throwUVerb . WithStatus @500)
        (\_ -> respond (WithStatus @201 NoContent))

indexItems :: Handler' (Union '[[Item], WithStatus 500 DbError])
indexItems =
  runUVerb $
    runErrorNoCallStack @DbError Item.list
      >>= either (throwUVerb . WithStatus @500) respond
