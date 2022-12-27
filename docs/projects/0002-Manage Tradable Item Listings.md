---
title: Manage Tradable Item Listings
subtitle: Proposal#0001
description: Project spec on item listings
icon: material/checkbox-multiple-blank-circle-outline
---

# 0002 - Manage Tradable Item Listings

:material-file-document: [Proposal#0001](../proposals/0001-Items.md)

## Description

A user must create an item listing if they wish to sell/buy an item on GNAWEX.
The amount that the user must input must be how many they have in their
inventory, and not based on how many they _may_ have in the future - no matter
how certain.

## :material-wrench: Operations

- View active listings
- View inactive listings
- Create
    - Users may create up to 10 tradable item listings
- Reduce quantity
    - Users must only be allowed to update the unit quantity, and only in amounts
that are not in any non-cancelled transaction. e.g If I make a sell listing with
an item quantity of 10, and 6 of it is involved in completed/pending
transactions, then I can only reduce the quantity to a maximum of 4.
    - Reducing the quantity to 0 will automatically delist
- Relist: Puts the listing back on GNAWEX's active listings. Hence may be
matched in the future. Can only relist if the item's available quantity is >0.
- Delist: Removes the listing from GNAWEX's active listings. Hence must not be
matched.

## :material-eye: Permissions

Role | View active listings | View inactive listings | Reduce quantity | Relist | Delist
:-- | -- | -- | -- | -- | --
Admin | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark:
Moderator | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark:
User | :white_check_mark: | :white_check_mark: [^1] | :white_check_mark: [^1] | :white_check_mark: [^1] | :white_check_mark: [^1]
Guest (non-logged in user) | :white_check_mark: | :x: | :x: | :x: | :x:

## :material-database: Database

### Tables

#### `app.tradable_item_listings`

Column name | Description | Type | Required | Nullable | Default
:-- | -- | -- | -- | -- | --
`id` | To uniquely identify an item listing | `BIGINT GENERATED ALWAYS AS IDENTITY` | `true` | `false` | Supplied by Postgres
`user_id` | To associate a listing to a user | `BIGINT` | `true` | `false` | -
`tradable_item_id` | To associate listing to a tradable item | `BIGINT` | `true` | `false` | -
`type` | What the user wants to do | `app.LISTING_TYPE AS ENUM ('buy', 'sell')` | `true` | `false` | -
`batched_by` | Batches one or more of the same item to be sold as one unit for it to cost as a whole number | `SMALLINT` | `true` | `false` | `1`
`unit_quantity` | Amount of units to be exchanged | `INT` | `true` | `false` | -
`cost` | How much one unit is in SB | `INT` | `true` | `false` | -
`active` | Determines if an item is delisted or not | `BOOLEAN` | `true` | `false` | `true`
`created_at` | To record when this was created | `TIMESTAMP WITH TIME ZONE` | `true` | `false` | `now()`
`updated_at` | To record when this was updated | `TIMESTAMP WITH TIME ZONE` | `false` | `true` | `now()` (if updated, otherwise `null`)

[^1]:
  Only for their own listings

### Triggers

#### `app.normalize_tradable_item_listing`

Before insert, `batched_by`, `cost`, and `quantity` must be normalized based on
their GCD for GNAWEX to match item listings better.