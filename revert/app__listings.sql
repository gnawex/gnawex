-- Revert gnawex:listings from pg

BEGIN;

REVOKE SELECT, UPDATE ON TABLE app.listings FROM gnawex_merchant;

-- FIXME: This is broken for some reason. Why does dropping an index not work?
-- It says it doesn't exist. Hmm...
DROP INDEX IF EXISTS active_id;

DROP TABLE app.listings;
DROP TYPE app.LISTING_TYPE;

COMMIT;
