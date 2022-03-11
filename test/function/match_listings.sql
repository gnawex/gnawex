BEGIN;

SELECT plan(1);

SELECT has_function('app', 'match', 'Check if the automated matcher exists');

SELECT * FROM finish();

ROLLBACK;
