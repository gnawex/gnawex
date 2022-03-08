-- Revert gnawex:session_permissions from pg

BEGIN;

REVOKE EXECUTE ON FUNCTION auth.login FROM anon, api;
REVOKE EXECUTE ON FUNCTION auth.refresh_session FROM verified_user;
REVOKE EXECUTE ON FUNCTION auth.logout FROM verified_user;
REVOKE EXECUTE ON FUNCTION auth.session_user_id FROM anon;
REVOKE EXECUTE ON FUNCTION auth.authenticate FROM anon;

COMMIT;
