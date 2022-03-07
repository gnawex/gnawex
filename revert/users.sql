-- Revert gnawex:users from pg

BEGIN;

DROP TRIGGER cryptpassword ON app.users;
DROP FUNCTION app.cryptpassword();
DROP TABLE app.users;
DROP TYPE USER_ROLE;

COMMIT;
