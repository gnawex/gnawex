--------------------------------------------------------------------------------
-- Users that are verified and not banned
CREATE ROLE valid_user;

-- dummy user to toy around with
CREATE USER sekun;
GRANT valid_user TO sekun;

--------------------------------------------------------------------------------
-- Role Permissions

-- Items
GRANT SELECT ON items TO valid_user;

-- Listings
GRANT SELECT, INSERT, UPDATE ON listings TO valid_user;
GRANT SELECT, UPDATE ON listings_id_seq TO valid_user;

-- Transactions
GRANT SELECT, INSERT ON transactions TO valid_user;

-- Users
GRANT SELECT (id, is_banned, is_verified) ON users TO valid_user;

-- Functions & procedures
GRANT EXECUTE ON FUNCTION match() TO valid_user;

--------------------------------------------------------------------------------
-- Experiment with RLS

-- Listing Policies

ALTER TABLE listings ENABLE ROW LEVEL SECURITY;

CREATE POLICY listing_read
  ON listings
  FOR SELECT
  USING (TRUE);

CREATE POLICY listing_new
  ON listings
  FOR INSERT
  WITH CHECK (
    user_id = NULLIF(current_setting('rls.user_id', TRUE), '') :: BIGINT
  );

CREATE POLICY listing_update
  ON listings
  FOR UPDATE
  USING (
    user_id = NULLIF(current_setting('rls.user_id', TRUE), '') :: BIGINT
    OR
    (EXISTS (SELECT current_user WHERE current_user = 'gnawex_merchant'))
  )
  WITH CHECK (
    EXISTS (SELECT current_user WHERE current_user = 'gnawex_merchant')
  );

--------------------------------------------------------------------------------
-- Transaction Policies

-- Only allow user to select their transactions
ALTER TABLE transactions ENABLE ROW LEVEL SECURITY;

-- Insert transaction that involves the user, fail otherwise.
CREATE POLICY txn_new
  ON transactions
  FOR INSERT
  WITH CHECK (
    buyer_id = NULLIF(current_setting('rls.user_id', TRUE), '') :: BIGINT 
    OR
    seller_id = NULLIF(current_setting('rls.user_id', TRUE), '') :: BIGINT
  );

-- List those transactions that involve the user, ignoring the rest.
CREATE POLICY txn_read
  ON transactions
  FOR SELECT
  USING (
    buyer_id = NULLIF(current_setting('rls.user_id', TRUE), '') :: BIGINT 
    OR
    seller_id = NULLIF(current_setting('rls.user_id', TRUE), '') :: BIGINT
  );

-- This is the user_id of `sekun` in the table `users`.
-- Should be removed in the future since this should be set by the application.
-- https://pganalyze.com/blog/postgres-row-level-security-ruby-rails
-- SET rls.user_id = 1;
-- SET request.token.claims = '{"email": "foo@example.com", "role": "valid_user"}';

--------------------------------------------------------------------------------

