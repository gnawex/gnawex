-- Revert gnawex:roles from pg

BEGIN;

REVOKE anon, verified_user, banned_user FROM authenticator;

DROP ROLE authenticator;
DROP ROLE anon;
DROP ROLE verified_user;
DROP ROLE banned_user;
DROP ROLE auth;
DROP ROLE api;

COMMIT;
