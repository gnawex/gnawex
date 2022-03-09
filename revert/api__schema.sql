-- Revert gnawex:api_schema from pg

BEGIN;

REVOKE SELECT ON api.items FROM anon, verified_user;
REVOKE SELECT ON api.transactions FROM verified_user;
REVOKE EXECUTE ON FUNCTION api.register FROM anon;
REVOKE EXECUTE ON FUNCTION api.logout FROM verified_user;
REVOKE EXECUTE ON FUNCTION api.refresh_session FROM verified_user;
REVOKE EXECUTE ON FUNCTION api.login FROM anon;
REVOKE EXECUTE ON FUNCTION api.current_user FROM verified_user;
REVOKE SELECT, UPDATE(username) ON api.users FROM verified_user;
REVOKE USAGE ON SCHEMA api FROM anon, verified_user;

DROP VIEW api.transactions;
DROP FUNCTION api.register;
DROP FUNCTION api.logout;
DROP FUNCTION api.refresh_session;
DROP FUNCTION api.login;
DROP FUNCTION api.current_user;
DROP TYPE api.user;
DROP VIEW api.users;
DROP VIEW api.items;

DROP SCHEMA api;

COMMIT;
