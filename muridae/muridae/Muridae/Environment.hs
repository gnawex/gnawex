module Muridae.Environment (MuridaeEnv (..), getMuridaeEnv) where

import Data.ByteString (ByteString)
import Data.Pool (
    Pool,
    PoolConfig (
        PoolConfig,
        createResource,
        freeResource,
        poolCacheTTL,
        poolMaxResources
    ),
 )
import Data.Pool qualified as Pool
import Data.Time (NominalDiffTime)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple qualified as Beam
import Effectful (Eff, Effect, IOE, MonadIO (liftIO), type (:>))

data MuridaeEnv = MuridaeEnv
    { pool :: Pool Connection
    }

getMuridaeEnv :: Eff '[IOE] MuridaeEnv
getMuridaeEnv = do
    -- TODO: Get from environment through Reader or something. Idk.
    let tempConnStr = "host='localhost' port=5432 dbname='gnawex_db' user='postgres'"
    connPool <- mkPool tempConnStr 5 20

    pure (MuridaeEnv connPool)

mkPool ::
    forall (es :: [Effect]).
    (IOE :> es) =>
    -- PG connection information
    -- e.g host='localhost' port=5432 dbname='gnawex_development' user='postgres'
    ByteString ->
    -- Max time to acquire a connection from pool
    NominalDiffTime ->
    -- Max number of connections in a pool
    Int ->
    Eff es (Pool Connection)
mkPool connectionInfo timeout poolSize =
    liftIO . Pool.newPool $
        PoolConfig
            { createResource = Beam.connectPostgreSQL connectionInfo
            , freeResource = Beam.close
            , poolCacheTTL = realToFrac timeout
            , poolMaxResources = poolSize
            }
