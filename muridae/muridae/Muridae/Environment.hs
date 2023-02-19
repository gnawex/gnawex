module Muridae.Environment
  ( MuridaeEnv (..)
  , getMuridaeEnv
  )
where

import Effectful (Eff, IOE, MonadIO (liftIO), (:>))
import Muridae.DB (Pool, Settings, acquire)

newtype MuridaeEnv = MuridaeEnv
  { pool :: Pool
  }

getMuridaeEnv :: Eff '[IOE] MuridaeEnv
getMuridaeEnv = do
  -- TODO: Get from environment through Reader or something. Idk.
  pool <- mkPool dbSettings 20

  pure (MuridaeEnv pool)
 where
  dbSettings = "host='localhost' port=5432 dbname='gnawex_db' user='postgres'"

mkPool :: (IOE :> es) => Settings -> Int -> Eff es Pool
mkPool dbSettings poolCapacity =
  liftIO $ acquire poolCapacity Nothing dbSettings
