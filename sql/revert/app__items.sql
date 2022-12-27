-- Revert gnawex:items from pg

BEGIN;

REVOKE
    SELECT,
    INSERT (name, description),
    UPDATE (name, description),
    DELETE
  ON TABLE app.tradable_items
  FROM api;

REVOKE SELECT ON TABLE app.tradable_items FROM anon, verified_user;
REVOKE ALL ON TABLE app.tradable_items_id_seq FROM api, verified_user;

DROP TABLE app.tradable_items;

COMMIT;
