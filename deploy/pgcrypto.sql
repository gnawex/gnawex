-- Deploy gnawex:pgcrypto to pg

BEGIN;

CREATE EXTENSION pgcrypto;

COMMIT;
