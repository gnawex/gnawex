DROP TRIGGER cryptpassword ON users;
DROP FUNCTION cryptpassword();
DROP FUNCTION current_user_id();

DROP TABLE users;
DROP TYPE USER_ROLE;

DROP EXTENSION citext;
DROP EXTENSION pgcrypto;
