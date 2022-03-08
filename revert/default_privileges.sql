-- Revert gnawex:default_privileges from pg
-- requires: roles

BEGIN;

ALTER DEFAULT PRIVILEGES GRANT EXECUTE ON functions TO public;
ALTER DEFAULT PRIVILEGES FOR ROLE auth, api GRANT EXECUTE ON functions TO public;

COMMIT;
