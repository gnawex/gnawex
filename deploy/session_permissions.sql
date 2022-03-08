-- Deploy gnawex:session_permissions to pg

BEGIN;

-- XXX Add DDLs here.
SET LOCAL ROLE auth;

GRANT EXECUTE ON FUNCTION auth.login TO anon, api;
GRANT EXECUTE ON FUNCTION auth.refresh_session TO verified_user;
GRANT EXECUTE ON FUNCTION auth.logout TO verified_user;
GRANT EXECUTE ON FUNCTION auth.session_user_id TO anon;
GRANT EXECUTE ON FUNCTION auth.authenticate TO anon;

COMMIT;
