-- Revert gnawex:sessions from pg
-- requires: sessions

BEGIN;

REVOKE EXECUTE ON FUNCTION auth.login FROM anon, api;
REVOKE EXECUTE ON FUNCTION auth.refresh_session FROM verified_user;
REVOKE EXECUTE ON FUNCTION auth.logout FROM verified_user;
REVOKE EXECUTE ON FUNCTION auth.session_user_id FROM anon;
REVOKE EXECUTE ON FUNCTION auth.authenticate FROM anon;

DROP FUNCTION auth.clean_sessions;
DROP FUNCTION auth.login;
DROP FUNCTION auth.refresh_session;
DROP FUNCTION auth.logout;
DROP FUNCTION auth.authenticate;
DROP FUNCTION auth.session_user_id;
DROP VIEW auth.active_sessions;
DROP TABLE auth.sessions;

COMMIT;
