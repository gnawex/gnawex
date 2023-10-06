-- Revert gnawex:listings from pg

BEGIN;

DROP TRIGGER set_listing_user_id ON app.tradable_item_listings;
DROP FUNCTION app.set_listing_user_id();

REVOKE SELECT ON TABLE app.tradable_item_listings FROM anon, verified_user;
REVOKE SELECT, INSERT, UPDATE (active) ON TABLE app.tradable_item_listings FROM api;
REVOKE ALL ON TABLE app.tradable_item_listings_id_seq FROM verified_user;
REVOKE SELECT, UPDATE ON TABLE app.tradable_item_listings FROM gnawex_merchant;

-- FIXME: This is broken for some reason. Why does dropping an index not work?
-- It says it doesn't exist. Hmm...
DROP INDEX IF EXISTS app.active_id;

DROP TABLE app.tradable_item_listings;
DROP TYPE app.LISTING_TYPE;

COMMIT;
