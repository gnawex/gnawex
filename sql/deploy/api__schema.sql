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
  SELECT id, username
    FROM app.users;

GRANT SELECT, UPDATE(username) ON api.users TO verified_user;

--------------------------------------------------------------------------------

CREATE TYPE api."User" AS (
  id        BIGINT,
  hunter_id BIGINT,
  username  TEXT
);

CREATE FUNCTION api.current_user()
  RETURNS api."User"
  LANGUAGE sql
  SECURITY DEFINER
  AS $$
    SELECT id, hunter_id, username
      FROM app.users
      WHERE id = app.current_user_id();
  $$;

COMMENT ON FUNCTION api.current_user IS
  'Information about the currently authenticated user';

GRANT EXECUTE ON FUNCTION api.current_user TO verified_user;

--------------------------------------------------------------------------------

CREATE FUNCTION api.login(username TEXT, password TEXT)
  RETURNS TEXT
  LANGUAGE plpgsql
  AS $$
    DECLARE
      session_token TEXT;
    BEGIN
      SELECT auth.login(login.username :: CITEXT, login.password)
        INTO session_token;

      IF session_token IS NULL THEN
        RAISE insufficient_privilege
          USING detail = 'invalid credentials';
      END IF;

      RETURN session_token;
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
      SELECT current_setting('request.session_token', false)
        INTO STRICT session_token;

      PERFORM auth.refresh_session(session_token);
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
  SELECT buy_item_listing__id FROM app.tradable_item_transactions;

GRANT SELECT ON api.transactions TO verified_user;

--------------------------------------------------------------------------------

CREATE VIEW api.tradable_item_listings AS
  SELECT
      tradable_item__id,
      user__id,
      unit_quantity,
      current_unit_quantity,
      cost,
      type,
      active
    FROM app.tradable_item_listings;

GRANT SELECT ON api.tradable_item_listings TO anon, verified_user;
GRANT INSERT (tradable_item__id, unit_quantity, current_unit_quantity, cost, type, active) ON api.tradable_item_listings TO verified_user;

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
        SELECT tradable_item_listings.cost
             , sum(tradable_item_listings.unit_quantity) AS unit_quantity
          FROM app.tradable_item_listings
          WHERE type = 'buy'
            AND active = true
            AND tradable_item_listings.tradable_item__id = get_item.item_id
          GROUP BY tradable_item_listings.cost
          ORDER BY
            tradable_item_listings.cost DESC
        FETCH FIRST 5 ROWS ONLY
     )
     -- JSON? Nah. Me, and my homies love JSONB.
     -- https://stackoverflow.com/a/70629577
     SELECT coalesce(jsonb_agg(row_to_json(grouped_buy_orders)), '[]' :: JSONB)
       FROM grouped_buy_orders
    ), sell_listings AS (
      WITH grouped_sell_orders AS (
        SELECT tradable_item_listings.cost, sum(tradable_item_listings.unit_quantity) AS unit_quantity
          FROM app.tradable_item_listings
          WHERE type = 'sell'
            AND active = true
            AND tradable_item_listings.tradable_item__id = get_item.item_id
            GROUP BY tradable_item_listings.cost
            ORDER BY
              tradable_item_listings.cost ASC
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

--------------------------------------------------------------------------------

COMMIT;

