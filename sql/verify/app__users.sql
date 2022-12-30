-- Verify gnawex:users on pg

BEGIN;

SELECT id, hunter_id, username, password, role
  FROM app.users
  WHERE false;

ROLLBACK;
