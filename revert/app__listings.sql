-- Revert gnawex:listings from pg

BEGIN;

DROP TRIGGER set_listing_user_id ON app.listings;
DROP FUNCTION app.set_listing_user_id();

REVOKE EXECUTE ON FUNCTION app.adjust_listing FROM verified_user;
DROP TRIGGER adjust_listing ON app.listings;
DROP FUNCTION app.adjust_listing();

REVOKE SELECT, INSERT, UPDATE (is_active) ON TABLE app.listings FROM api;
REVOKE ALL ON TABLE app.listings_listing_id_seq FROM verified_user;
REVOKE SELECT, UPDATE ON TABLE app.listings FROM gnawex_merchant;

-- FIXME: This is broken for some reason. Why does dropping an index not work?
-- It says it doesn't exist. Hmm...
DROP INDEX IF EXISTS active_id;

DROP TABLE app.listings;
DROP TYPE app.LISTING_TYPE;

COMMIT;
