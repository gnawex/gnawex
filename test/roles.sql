BEGIN;

SELECT plan(7);

SELECT has_role('auth');
SELECT has_role('api');
SELECT has_role('authenticator');
SELECT has_role('anon');
SELECT has_role('gnawex_merchant');
SELECT has_role('verified_user');
SELECT has_role('banned_user');

SELECT * FROM finish();

ROLLBACK;
