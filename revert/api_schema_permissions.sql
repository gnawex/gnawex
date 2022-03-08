-- Revert gnawex:api_schema_permissions from pg

BEGIN;

SET LOCAL ROLE api;

REVOKE SELECT ON api.transactions FROM verified_user;
REVOKE EXECUTE ON FUNCTION api.register FROM anon;
REVOKE EXECUTE ON FUNCTION api.logout FROM verified_user;
REVOKE EXECUTE ON FUNCTION api.refresh_session FROM verified_user;
REVOKE EXECUTE ON FUNCTION api.login FROM anon;
REVOKE EXECUTE ON FUNCTION api.current_user FROM verified_user;
REVOKE SELECT, UPDATE(username) ON api.users FROM verified_user;
REVOKE SELECT ON api.items FROM anon, verified_user;

REVOKE USAGE ON SCHEMA api FROM anon, verified_user;

COMMIT;
