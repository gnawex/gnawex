{-# LANGUAGE ExistentialQuantification #-}

module Effectful.Beam (module Effectful.Beam) where

import Control.Monad.Catch (Exception, SomeException, catch)
import Data.Aeson (ToJSON (toJSON), Value (String), object)
import Data.Int (Int32)
import Data.Kind (Type)
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import Database.Beam.Postgres (Connection, Pg)
import Database.Beam.Postgres qualified as Beam
import Database.PostgreSQL.Simple (query)
import Database.PostgreSQL.Simple.Transaction (withTransactionSerializable)
import Effectful (Dispatch (Static), DispatchOf, Eff, Effect, IOE, type (:>))
import Effectful.Dispatch.Static
  ( SideEffects (WithSideEffects)
  , StaticRep
  , evalStaticRep
  , getStaticRep
  , unsafeEff_
  )
import Effectful.Error.Static (Error, throwError)
import Text.Builder qualified

-------------------------------------------------------------------------------
-- Types

data DB :: Effect

data DbError = forall (e :: Type). (Exception e, Show e) => DbError e

-------------------------------------------------------------------------------
-- Instances

type instance DispatchOf DB = 'Static 'WithSideEffects

newtype instance StaticRep DB = DB (Pool Connection)

instance ToJSON DbError where
  toJSON :: DbError -> Value
  toJSON (DbError _e) =
    object
      [ ("message", String "GNAWEX was unable to establish a connection with the database.")
      , ("help", String "If you are the GNAWEX admin, ensure that the DB credentials are valid.")
      ]

-------------------------------------------------------------------------------

runDB
  :: forall (es :: [Effect]) (a :: Type)
   . (IOE :> es)
  => Pool Connection
  -> Eff (DB ': es) a
  -> Eff es a
runDB connPool = evalStaticRep (DB connPool)

authQueryDebug
  :: forall (es :: [Effect]) (a :: Type)
   . (DB :> es)
  => (String -> IO ())
  -- TODO: Use `UserId`
  -> Int32
  -> Pg a
  -> Eff es a
authQueryDebug debug userId pg = do
  DB connPool <- getStaticRep

  unsafeEff_ $
    withResource connPool $
      \conn -> withTransactionSerializable conn $ do
        -- TODO: Just an experiment
        _ <-
          query
            @[Text]
            @[Text]
            conn
            "SELECT set_config('auth.user_id', ?, true)"
            [Text.Builder.run . Text.Builder.decimal $ userId]

        Beam.runBeamPostgresDebug debug conn pg

queryDebug
  :: forall (es :: [Effect]) (a :: Type)
   . (DB :> es)
  => (String -> IO ())
  -> Pg a
  -> Eff (Error DbError : es) a
queryDebug debug pg = do
  DB connPool <- getStaticRep

  catch @(Eff (Error DbError : es)) @SomeException
    ( unsafeEff_ $ do
        withResource connPool $
          \conn ->
            withTransactionSerializable conn $ do
              Beam.runBeamPostgresDebug debug conn pg
    )
    -- TODO: Add metadata to @DbError@
    (throwError @DbError . DbError @SomeException)
