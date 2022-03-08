-- Deploy gnawex:default_privileges to pg

BEGIN;

-- Removes default privileges to execute functions
ALTER DEFAULT PRIVILEGES REVOKE EXECUTE ON functions FROM public;

-- Like the previous, except for the roles `auth` and `api`
ALTER DEFAULT PRIVILEGES FOR ROLE auth, api REVOKE EXECUTE ON functions FROM public;

COMMIT;
