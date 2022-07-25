BEGIN;

SELECT plan(5);

SELECT has_schema('app', 'Check if the GNAWEX app schema exists');
SELECT schema_privs_are('app', 'api', ARRAY['USAGE']);
SELECT schema_privs_are('app', 'auth', ARRAY['USAGE']);
SELECT schema_privs_are('app', 'gnawex_merchant', ARRAY['USAGE']);
SELECT schema_privs_are('app', 'verified_user', ARRAY['USAGE']);

SELECT * FROM finish();

ROLLBACK;
