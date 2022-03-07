-- Revert gnawex:roles from pg

BEGIN;

DROP ROLE authenticator;
DROP ROLE anon;
DROP ROLE verified_user;
DROP ROLE banned_user;
DROP ROLE auth;
DROP ROLE api;

COMMIT;
