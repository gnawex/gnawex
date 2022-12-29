{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module MuridaeWeb.Server (mkServer, runMuridae) where

import Control.Exception (bracket)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Kind (Type)
import qualified Data.Pool as Pool
import Effectful (Eff, IOE, runEff)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.Reader.Static (runReader)
import Muridae.Environment (MuridaeEnv (MuridaeEnv), getMuridaeEnv)
import MuridaeWeb.Handlers.Items.Index (indexItems)
import MuridaeWeb.Routes (
  API (API, publicRoutes),
  PublicRoutes (PublicRoutes, items),
 )
import MuridaeWeb.Routes.Items (index)
import qualified MuridaeWeb.Routes.Items as Item
import MuridaeWeb.Types (Handler')
import Servant (ServerError)
import Servant.Server (Application, Handler)
import Servant.Server.Generic (AsServerT, genericServeT)
import qualified Network.Wai.Handler.Warp as Warp

-- TODO: Generate docs

mkServer :: MuridaeEnv -> Application
mkServer muridaeEnv =
  genericServeT
    (\app -> effToHandler $ runReader muridaeEnv $ app)
    muridaeServer

muridaeServer :: API (AsServerT Handler')
muridaeServer =
  API
    { publicRoutes =
        PublicRoutes
          { items = Item.Routes'{index = indexItems}
          }
    }

runMuridae :: IO ()
runMuridae =
  bracket
    (runEff getMuridaeEnv)
    (runEff . shutdownMuridae)
    (Warp.run 8080 . mkServer)

shutdownMuridae :: MuridaeEnv -> Eff '[IOE] ()
shutdownMuridae (MuridaeEnv pool) =
  liftIO $ Pool.destroyAllResources pool

effToHandler :: forall (a :: Type). Eff '[Error ServerError, IOE] a -> Handler a
effToHandler computation = do
  v <- liftIO . runEff . runErrorNoCallStack @ServerError $ computation

  either throwError pure v
