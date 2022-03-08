-- Revert gnawex:sessions from pg
-- requires: sessions

BEGIN;

SET LOCAL ROLE auth;

DROP FUNCTION auth.clean_sessions;
DROP FUNCTION auth.login;
DROP FUNCTION auth.refresh_session;
DROP FUNCTION auth.logout;
DROP FUNCTION auth.authenticate;
DROP FUNCTION auth.session_user_id;
DROP VIEW auth.active_sessions;
DROP TABLE auth.sessions;

COMMIT;
