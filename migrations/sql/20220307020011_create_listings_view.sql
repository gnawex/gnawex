CREATE VIEW item_listings AS
  SELECT items.id, items.name, listings.quantity, listings.cost, listings.type
  FROM listings
  INNER JOIN items
    ON listings.item_id = items.id;

--------------------------------------------------------------------------------

GRANT SELECT ON item_listings TO anon;

--------------------------------------------------------------------------------
