-- Deploy gnawex:api_schema to pg

BEGIN;

--------------------------------------------------------------------------------

CREATE SCHEMA api AUTHORIZATION api;

COMMENT ON SCHEMA api IS
  'Schema that defines what is suitable to be exposed through PostgREST';

-- Everything in this transaction from this point forward will be owned by `api`
SET LOCAL ROLE api;

GRANT USAGE ON SCHEMA api TO anon, verified_user;

--------------------------------------------------------------------------------

CREATE VIEW api.users AS
  SELECT user_id, username
    FROM app.users;

GRANT SELECT, UPDATE(username) ON api.users TO verified_user;

--------------------------------------------------------------------------------

CREATE TYPE api.user AS (
  user_id   BIGINT,
  hunter_id BIGINT,
  username  TEXT
);

CREATE FUNCTION api.current_user()
  RETURNS api.user
  LANGUAGE sql
  SECURITY DEFINER
  AS $$
    SELECT user_id, hunter_id, username
      FROM app.users
      WHERE user_id = app.current_user_id();
  $$;

COMMENT ON FUNCTION api.current_user IS
  'Information about the currently authenticated user';

GRANT EXECUTE ON FUNCTION api.current_user TO verified_user;

--------------------------------------------------------------------------------

CREATE FUNCTION api.login(username CITEXT, password TEXT)
  RETURNS VOID
  LANGUAGE plpgsql
  AS $$
    DECLARE
      session_token TEXT;
    BEGIN
      SELECT auth.login(login.username, login.password) INTO session_token;

      IF session_token IS NULL THEN
        raise insufficient_privilege
          using detail = 'invalid credentials';
      END IF;

      PERFORM set_config(
        'response.headers',
        '[{"Set-Cookie": "session_token='
          || session_token
          || '; Path=/; Max-Age=600; HttpOnly"}]',
          TRUE
      );
    END;
  $$;

COMMENT ON FUNCTION api.login IS
  'Creates a new session given valid credentials';

GRANT EXECUTE ON FUNCTION api.login TO anon;

--------------------------------------------------------------------------------

CREATE FUNCTION api.refresh_session()
  RETURNS VOID
  LANGUAGE plpgsql
  AS $$
    DECLARE
      session_token TEXT;
    BEGIN
      SELECT current_setting('request.cookies', FALSE)::json->>'session_token'
        INTO STRICT session_token;

      PERFORM auth.refresh_session(session_token);

      PERFORM set_config(
        'response.headers',
        '[{"Set-Cookie": "session_token='
          || session_token
          || '; Path=/; Max-Age=600; HttpOnly"}]',
          TRUE
      );
    END;
  $$;

COMMENT ON FUNCTION api.refresh_session IS
  'Reset the expiration time of the given session';

GRANT EXECUTE ON FUNCTION api.refresh_session to verified_user;

--------------------------------------------------------------------------------

CREATE FUNCTION api.logout()
  RETURNS VOID
  LANGUAGE plpgsql
  AS $$
    BEGIN
      PERFORM auth.logout(
        current_setting('request.cookies', TRUE)::json->>'session_token'
      );

      PERFORM set_config(
        'response.headers',
        '[{"Set-Cookie": "session_token=; Path=/"}]',
        TRUE
      );
    END;
  $$;

COMMENT ON FUNCTION api.logout IS
  'Expires the given session and resets the session cookie';

GRANT EXECUTE ON FUNCTION api.logout TO verified_user;

--------------------------------------------------------------------------------

CREATE FUNCTION api.register(username CITEXT, hunter_id BIGINT, password TEXT)
  RETURNS VOID
  SECURITY DEFINER
  LANGUAGE plpgsql
  AS $$
    BEGIN
      INSERT INTO app.users (username, hunter_id, password, role)
        VALUES (
          register.username,
          register.hunter_id,
          register.password,

          -- TODO: Should not default to this, but it's here for now.
          'verified_user'
        );

      PERFORM api.login(username, password);
    END;
  $$;

COMMENT ON FUNCTION api.register IS
  'Registers a new user and creates a new session for that account';

GRANT EXECUTE ON FUNCTION api.register TO anon;

--------------------------------------------------------------------------------

CREATE VIEW api.transactions AS
  SELECT buy_order FROM app.transactions;

GRANT SELECT ON api.transactions TO verified_user;

--------------------------------------------------------------------------------

CREATE VIEW api.listings AS
  SELECT
      item_id,
      user_id,
      quantity,
      cost,
      type,
      batch,
      is_active
    FROM app.listings;

GRANT SELECT ON api.listings TO anon, verified_user;
GRANT INSERT (item_id, quantity, cost, type, batch, is_active) ON api.listings TO verified_user;

--------------------------------------------------------------------------------

CREATE TYPE api.ITEM_RECORD AS (name TEXT, description TEXT, buy_listings JSONB, sell_listings JSONB);

CREATE FUNCTION api.get_item(item_id BIGINT)
  RETURNS TABLE (name TEXT, description TEXT, buy_listings JSONB, sell_listings JSONB)
  LANGUAGE sql IMMUTABLE
  AS $$
    -- I considered using `INTO STRICT _` but I didn't get the error message I
    -- wanted. Found this: https://postgrest.org/en/v8.0/api.html#singular-or-plural
    -- So this needs to be in the headers:
    -- `Accept: application/vnd.pgrst.object+json`
    -- I also considered handling the exception, but setting the aforementioned
    -- header is so much easier.
    WITH buy_listings AS (
      WITH grouped_buy_orders AS (
        SELECT listings.cost, sum(listings.quantity) AS quantity, listings.batch
          FROM app.listings
          WHERE type = 'buy'
            AND is_active = true
            AND listings.item_id = get_item.item_id
            GROUP BY listings.batch, listings.cost
            ORDER BY
              listings.cost DESC,
              listings.batch ASC
        FETCH FIRST 5 ROWS ONLY
     )
     -- JSON? Nah. Me, and my homies love JSONB.
     -- https://stackoverflow.com/a/70629577
     SELECT coalesce(jsonb_agg(row_to_json(grouped_buy_orders)), '[]' :: JSONB)
       FROM grouped_buy_orders
    ), sell_listings AS (
      WITH grouped_sell_orders AS (
        SELECT listings.cost, sum(listings.quantity) AS quantity, listings.batch
          FROM app.listings
          WHERE type = 'sell'
            AND is_active = true
            AND listings.item_id = get_item.item_id
            GROUP BY listings.batch, listings.cost
            ORDER BY
              listings.cost ASC,
              listings.batch ASC
        FETCH FIRST 5 ROWS ONLY
     )
     SELECT coalesce(jsonb_agg(row_to_json(grouped_sell_orders)), '[]' :: JSONB)
       FROM grouped_sell_orders
    )
    SELECT
        tradable_items.name,
        tradable_items.description,
        buy_listings.coalesce AS buy_listings,
        sell_listings.coalesce AS sell_listings
      FROM app.tradable_items, buy_listings, sell_listings
      WHERE tradable_items.id = get_item.item_id
      GROUP BY tradable_items.name, tradable_items.description, buy_listings, sell_listings;
  $$;

GRANT EXECUTE ON FUNCTION api.get_item TO anon, verified_user;

SET LOCAL ROLE postgres;

ALTER DATABASE gnawex_db SET search_path TO app,api,auth,public;

--------------------------------------------------------------------------------

COMMIT;

