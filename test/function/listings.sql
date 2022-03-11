BEGIN;

SELECT plan(8);

--------------------------------------------------------------------------------

SELECT has_function('app', 'adjust_listing', 'Check if listing normalizer exists');

--------------------------------------------------------------------------------
-- Item fixture
INSERT
  INTO app.items (item_id, name, description)
  VALUES (99999, 'Test Item', 'This is just a test item');

INSERT
  INTO app.items (item_id, name, description)
  VALUES (99998, 'Test Item', 'This is just a test item');


-- User fixture
INSERT
  INTO app.users (user_id, hunter_id, username, password, role)
  VALUES (99999, 99999, 'foobar', 'hunter2', 'verified_user');

SELECT set_config('auth.user_id' :: TEXT, '99999', TRUE);

--------------------------------------------------------------------------------

SELECT row_eq(
  $$
    INSERT
      INTO app.listings (item_id, user_id, quantity, cost, batch, type)
      VALUES (99999, 99999, 20, 7, 2, 'sell')
      RETURNING quantity, cost, batch;
  $$,
  ROW(20, 7 :: BIGINT, 2)
);


-- This is here since the match listing sets the transaction's role to
-- gnawex_merchant, and this is all in one transaction still. I think.
SET LOCAL ROLE sekun;

SELECT row_eq(
  $$
    INSERT
      INTO app.listings (item_id, user_id, quantity, cost, batch, type)
      VALUES (99999, 99999, 20, 7, 2, 'sell')
      RETURNING quantity, cost, batch;
  $$,
  ROW(20, 7 :: BIGINT, 2)
);

-- This is here since the match listing sets the transaction's role to
-- gnawex_merchant, and this is all in one transaction still. I think.
SET LOCAL ROLE sekun;

SELECT row_eq(
  $$
    INSERT
      INTO app.listings (item_id, user_id, quantity, cost, batch, type)
      VALUES (99999, 99999, 20, 1, 2, 'sell')
      RETURNING quantity, cost, batch;
  $$,
  ROW(20, 1 :: BIGINT, 2)
);

-- This is here since the match listing sets the transaction's role to
-- gnawex_merchant, and this is all in one transaction still. I think.
SET LOCAL ROLE sekun;

SELECT row_eq(
  $$
    INSERT
      INTO app.listings (item_id, user_id, quantity, cost, batch, type)
      VALUES (99999, 99999, 20, 2, 20, 'sell')
      RETURNING quantity, cost, batch;
  $$,
  ROW(40, 1 :: BIGINT, 10)
);

-- This is here since the match listing sets the transaction's role to
-- gnawex_merchant, and this is all in one transaction still. I think.
SET LOCAL ROLE sekun;

SELECT row_eq(
  $$
    INSERT
      INTO app.listings (item_id, user_id, quantity, cost, batch, type)
      VALUES (99999, 99999, 20, 2, 5, 'sell')
      RETURNING quantity, cost, batch;
  $$,
  ROW(20, 2 :: BIGINT, 5)
);

-- This is here since the match listing sets the transaction's role to
-- gnawex_merchant, and this is all in one transaction still. I think.
SET LOCAL ROLE sekun;

SELECT row_eq(
  $$
    INSERT
      INTO app.listings (item_id, user_id, quantity, cost, batch, type)
      VALUES (99999, 99999, 20, 3, 9, 'sell')
      RETURNING quantity, cost, batch;
  $$,
  ROW(60, 1 :: BIGINT, 3)
);

-- This is here since the match listing sets the transaction's role to
-- gnawex_merchant, and this is all in one transaction still. I think.
SET LOCAL ROLE sekun;

SELECT row_eq(
  $$
    INSERT
      INTO app.listings (item_id, user_id, quantity, cost, batch, type)
      VALUES (99999, 99999, 20, 2, 2, 'sell')
      RETURNING quantity, cost, batch;
  $$,
  ROW(40, 1 :: BIGINT, 1)
);

--------------------------------------------------------------------------------

SELECT * FROM finish();

ROLLBACK;

