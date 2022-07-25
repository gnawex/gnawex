-- Revert gnawex:items from pg

BEGIN;

REVOKE
    SELECT,
    INSERT (name, description),
    UPDATE (name, description),
    DELETE
  ON TABLE app.items
  FROM api;

REVOKE SELECT ON TABLE app.items FROM anon, verified_user;
REVOKE ALL ON TABLE app.items_item_id_seq FROM api, verified_user;

DROP TABLE app.items;

COMMIT;
