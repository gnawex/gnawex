module Muridae.ItemTxn.Model where

-- import Data.Functor.Identity (Identity)
-- import Database.Beam.Postgres (Pg)
-- import Effectful (Eff, type (:>))
-- import Effectful.Beam (DB)
-- import Effectful.Internal.Effect (Effect)
-- import Muridae.ItemListing.Types (ItemBuyListing, ItemSellListing, ItemListing (_cost))
-- import Effectful.Error.Static (Error)

-- createTxn ::
--   forall (es :: [Effect]).
--   (DB :> es, Error () :> es) =>
--   ItemBuyListing Identity ->
--   ItemSellListing Identity ->
--   Eff es (Pg ())
-- createTxn buy sell
--   | buy._cost == sell._cost = _
--   | otherwise = _

