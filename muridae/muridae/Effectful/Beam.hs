module Effectful.Beam where

import Data.Kind (Type)
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import Database.Beam.Postgres (Connection, Pg)
import Database.Beam.Postgres qualified as Beam
import Database.PostgreSQL.Simple (query_)
import Database.PostgreSQL.Simple.Transaction (withTransactionSerializable)
import Effectful (Dispatch (Static), DispatchOf, Eff, Effect, IOE, type (:>))
import Effectful.Dispatch.Static (
    SideEffects (WithSideEffects),
    StaticRep,
    evalStaticRep,
    getStaticRep,
    unsafeEff_,
 )

data DB :: Effect

type instance DispatchOf DB = 'Static 'WithSideEffects
newtype instance StaticRep DB = DB (Pool Connection)

runDB ::
    forall (es :: [Effect]) (a :: Type).
    (IOE :> es) =>
    Pool Connection ->
    Eff (DB ': es) a ->
    Eff es a
runDB connPool = evalStaticRep (DB connPool)

queryDebug ::
    forall (es :: [Effect]) (a :: Type).
    (DB :> es) =>
    (String -> IO ()) ->
    Pg a ->
    Eff es a
queryDebug debug pg = do
    DB connPool <- getStaticRep

    unsafeEff_ $
        withResource connPool $
            \conn -> withTransactionSerializable conn $ do
                -- TODO: Just an experiment
                _ <- query_ @[Text] conn "SELECT set_config('auth.user_id', '1', true)"
                result <- Beam.runBeamPostgresDebug debug conn pg

                pure result
