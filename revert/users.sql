-- Revert gnawex:users from pg

BEGIN;

ALTER TABLE app.users DISABLE ROW LEVEL SECURITY;

DROP TRIGGER cryptpassword ON app.users;
DROP FUNCTION app.cryptpassword();
DROP TABLE app.users;
DROP TYPE USER_ROLE;

COMMIT;
