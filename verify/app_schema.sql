-- Verify gnawex:app_schema on pg

DO $$
BEGIN
   ASSERT (SELECT has_schema_privilege('app', 'usage'));
END $$;

