module MuridaeWeb.Server (mkServer, runMuridae) where

import Control.Exception (bracket)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Kind (Type)
import qualified Data.Pool as Pool
import Effectful (Eff, IOE, runEff)
import Effectful.Beam (runDB)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.Reader.Static (runReader)
import Muridae.Environment (MuridaeEnv, getMuridaeEnv, pool)
import qualified MuridaeWeb.Handler.Item.Create as ItemHandler
import qualified MuridaeWeb.Handler.Item.Index as ItemHandler
import qualified MuridaeWeb.Handler.Item.Listing as ListingHandler
import MuridaeWeb.Route (
  API (API, adminRoutes, publicRoutes),
  AdminRoutes (AdminRoutes, items),
  PublicRoutes (PublicRoutes, items),
 )
import qualified MuridaeWeb.Route.Admin.Item as AdminItem
import MuridaeWeb.Route.ItemListing (index)
import qualified MuridaeWeb.Route.ItemListing as ItemListing
import MuridaeWeb.Route.Item (indexItems)
import qualified MuridaeWeb.Route.Item as Items
import MuridaeWeb.Types (Handler')
import qualified Network.Wai.Handler.Warp as Warp
import Servant (ServerError)
import Servant.Server (Application, Handler)
import Servant.Server.Generic (AsServerT, genericServeT)

-- TODO: Generate docs

mkServer :: MuridaeEnv -> Application
mkServer muridaeEnv =
  genericServeT
    ( \app ->
        effToHandler $
          runDB (pool muridaeEnv) $
            runReader muridaeEnv $
              app
    )
    muridaeServer

muridaeServer :: API (AsServerT Handler')
muridaeServer =
  API
    { publicRoutes =
        PublicRoutes
          { items =
              Items.Routes'
                { indexItems = ItemHandler.indexItems
                , listings =
                    ItemListing.Routes'
                      { index = ListingHandler.index
                      , create = ListingHandler.create
                      }
                }
          }
    , adminRoutes =
        AdminRoutes
          { items =
              AdminItem.Routes'
                { index = ItemHandler.indexItems
                , create = ItemHandler.create
                }
          }
    }

runMuridae :: IO ()
runMuridae =
  bracket
    (runEff getMuridaeEnv)
    (runEff . shutdownMuridae)
    (Warp.run 8080 . mkServer)

shutdownMuridae :: MuridaeEnv -> Eff '[IOE] ()
shutdownMuridae = liftIO . Pool.destroyAllResources . pool

effToHandler :: forall (a :: Type). Eff '[Error ServerError, IOE] a -> Handler a
effToHandler computation = do
  v <- liftIO . runEff . runErrorNoCallStack @ServerError $ computation

  either throwError pure v
