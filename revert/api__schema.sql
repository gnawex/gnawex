-- Revert gnawex:api_schema from pg

BEGIN;

REVOKE EXECUTE ON FUNCTION api.get_item FROM anon, verified_user;
REVOKE SELECT ON api.items FROM anon, verified_user;
REVOKE SELECT ON api.transactions FROM verified_user;
REVOKE EXECUTE ON FUNCTION api.register FROM anon;
REVOKE EXECUTE ON FUNCTION api.logout FROM verified_user;
REVOKE EXECUTE ON FUNCTION api.refresh_session FROM verified_user;
REVOKE EXECUTE ON FUNCTION api.login FROM anon;
REVOKE EXECUTE ON FUNCTION api.current_user FROM verified_user;
REVOKE SELECT, UPDATE(username) ON api.users FROM verified_user;
REVOKE SELECT ON api.listings FROM anon, verified_user;
REVOKE INSERT (item_id, quantity, cost, type, batch, is_active) ON api.listings FROM verified_user;
REVOKE USAGE ON SCHEMA api FROM anon, verified_user;

DROP SCHEMA api CASCADE;

COMMIT;
