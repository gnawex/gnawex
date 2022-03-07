-- Deploy gnawex:citext to pg

BEGIN;

CREATE EXTENSION citext;

COMMIT;
