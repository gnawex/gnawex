module MuridaeWeb.Server (runMuridae) where

import Control.Exception (bracket)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Kind (Type)
import Data.Pool qualified as Pool
import Effectful (Eff, IOE, runEff)
import Effectful.Beam (runDB)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.Reader.Static (runReader)
import Muridae.Environment
  ( MuridaeEnv
  , getMuridaeEnv
  , pool
  )
import MuridaeWeb.Handler.Item qualified as ItemHandler
import MuridaeWeb.Handler.ItemListing qualified as ItemListingHandler
import MuridaeWeb.Route
  ( APIv1 (APIv1, adminRoutes, publicRoutes)
  , AdminRoutes (AdminRoutes, items)
  , PublicRoutes (PublicRoutes, itemListings, items)
  )
import MuridaeWeb.Route.Admin.Item qualified as AdminItem
import MuridaeWeb.Route.Item qualified as ItemRoute
import MuridaeWeb.Route.ItemListing (index)
import MuridaeWeb.Route.ItemListing qualified as ItemListingRoute
import MuridaeWeb.Types (Handler')
import Network.Wai.Handler.Warp qualified as Warp
import Servant (ServerError)
import Servant.Server (Application, Handler)
import Servant.Server.Generic (AsServerT, genericServeT)

-- TODO: Generate docs

-- | Makes an application for @wai@ to run
mkApplication :: MuridaeEnv -> Application
mkApplication muridaeEnv =
  genericServeT
    (effToHandler . runDB (muridaeEnv.pool) . runReader muridaeEnv)
    muridaeAPIv1

-- | Maps the routes and their handlers
muridaeAPIv1 :: APIv1 (AsServerT Handler')
muridaeAPIv1 =
  let
    itemRoutes =
      ItemRoute.Routes'
        { index = ItemHandler.indexItems
        , show = ItemHandler.showDetails

        }

    itemListingRoutes =
      ItemListingRoute.Routes'
        { index = ItemListingHandler.index
        , create = ItemListingHandler.create
        , updateStatus = ItemListingHandler.updateStatus
        }

    adminItemRoutes =
      AdminItem.Routes'
        { index = ItemHandler.indexItems
        , create = ItemHandler.create
        }
   in
    APIv1
      { publicRoutes =
          PublicRoutes
            { items = itemRoutes
            , itemListings = itemListingRoutes
            }
      , adminRoutes =
          AdminRoutes
            { items = adminItemRoutes
            }
      }

-- | Runs the @muridae@ server
runMuridae :: IO ()
runMuridae =
  bracket
    (runEff getMuridaeEnv)
    (runEff . shutdownMuridae)
    (Warp.run 8080 . mkApplication)

-- | Shuts down @muridae@ as well as destroys the DB pool
shutdownMuridae :: MuridaeEnv -> Eff '[IOE] ()
shutdownMuridae env = liftIO (Pool.destroyAllResources env.pool)

effToHandler :: forall (a :: Type). Eff '[Error ServerError, IOE] a -> Handler a
effToHandler computation = do
  v <- liftIO . runEff . runErrorNoCallStack @ServerError $ computation

  either throwError pure v
