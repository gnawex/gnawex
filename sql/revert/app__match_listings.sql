-- Revert gnawex:match_listing from pg

BEGIN;

DROP TRIGGER match_listings ON app.tradable_item_listings;
DROP FUNCTION app.match();

COMMIT;
