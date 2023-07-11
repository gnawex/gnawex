DROP TRIGGER normalize_item_order ON item_orders;
DROP TRIGGER set_item_order_user_id ON item_orders;

DROP FUNCTION adjust_item_order();
DROP FUNCTION set_item_order_user_id();

DROP TABLE item_orders;
DROP TYPE ORDER_KIND;
