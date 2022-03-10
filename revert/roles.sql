-- Revert gnawex:roles from pg

BEGIN;

REVOKE anon, verified_user, banned_user, gnawex_merchant FROM authenticator;

ALTER DEFAULT PRIVILEGES GRANT EXECUTE ON functions TO public;
ALTER DEFAULT PRIVILEGES FOR ROLE auth, api GRANT EXECUTE ON functions TO public;

DROP ROLE gnawex_merchant;
DROP ROLE authenticator;
DROP ROLE anon;
DROP ROLE verified_user;
DROP ROLE banned_user;
DROP ROLE auth;
DROP ROLE api;

COMMIT;
