BEGIN;

SELECT plan(4);

--------------------------------------------------------------------------------

-- Check if it exists
SELECT has_function('app', 'current_user_id', 'Checks the current user ID');

-- Setting some test config data
SELECT set_config('auth.user_id' :: TEXT, '1', TRUE);

SELECT is(app.current_user_id(), '1' :: INTEGER);

SELECT set_config('auth.user_id' :: TEXT, NULL, TRUE);

SELECT is(app.current_user_id(), NULL);

--------------------------------------------------------------------------------

SELECT has_function('app', 'cryptpassword', 'Check if passwords will be encrypted');

--------------------------------------------------------------------------------


SELECT * FROM finish();

ROLLBACK;
