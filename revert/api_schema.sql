-- Revert gnawex:api_schema from pg

BEGIN;

SET LOCAL ROLE api;

DROP VIEW api.transactions;
DROP FUNCTION api.register;
DROP FUNCTION api.logout;
DROP FUNCTION api.refresh_session;
DROP FUNCTION api.login;
DROP FUNCTION api.current_user;
DROP TYPE api.user;
DROP VIEW api.users;
DROP VIEW api.items;

DROP SCHEMA api;

COMMIT;
