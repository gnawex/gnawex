-- Deploy gnawex:api_schema_permissions to pg

BEGIN;

SET LOCAL ROLE api;

GRANT USAGE ON SCHEMA api TO anon, verified_user;
GRANT SELECT, UPDATE(username) ON api.users TO verified_user;
GRANT EXECUTE ON FUNCTION api.current_user TO verified_user;
GRANT EXECUTE ON FUNCTION api.login TO anon;
GRANT EXECUTE ON FUNCTION api.refresh_session to verified_user;
GRANT EXECUTE ON FUNCTION api.logout TO verified_user;
GRANT EXECUTE ON FUNCTION api.register TO anon;
GRANT SELECT ON api.transactions TO verified_user;
GRANT SELECT ON api.items TO anon, verified_user;

COMMIT;
