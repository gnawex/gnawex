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

#### `app.users`

Column name | Description | Type | Required | Nullable | Default
:-- | -- | -- | -- | -- | --
`id` | User ID | `BIGINT GENERATED ALWAYS AS IDENTITY` | `true` | `false` | Supplied by Postgres
`hunter_id` | The user's MouseHunt hunter ID | `BIGINT` | `true` | `false` | -
`username` | Username used to log-in | `CITEXT` (case insensitive text) | `true` | `false` | -
`password` | User's password | `TEXT` (hashed) | `true` | `false` | -
`role` | User role | `app.USER_ROLE('verified_user', 'unverified_user', 'banned_user')` | `true` | `false` | `unverified_user` | -

#### `app.tradable_item_listings`

Column name | Description | Type | Required | Nullable | Default
:-- | -- | -- | -- | -- | --
`id` | To uniquely identify an item listing | `BIGINT GENERATED ALWAYS AS IDENTITY` | `true` | `false` | Supplied by Postgres
`user__id` | To associate a listing to a user | `BIGINT` | `true` | `false` | -
`tradable_item__id` | To associate listing to a tradable item | `BIGINT` | `true` | `false` | -
`type` | What the user wants to do | `app.LISTING_TYPE AS ENUM ('buy', 'sell')` | `true` | `false` | -
`batched_by` | Batches one or more of the same item to be sold as one unit for it to cost as a whole number | `SMALLINT` | `true` | `false` | `1`
`unit_quantity` | Amount of units to be exchanged | `INT` | `true` | `false` | -
`cost` | How much one unit is in SB | `INT` | `true` | `false` | -
`active` | Determines if an item is delisted or not | `BOOLEAN` | `true` | `false` | `true`
`created_at` | To record when this was created | `TIMESTAMP WITH TIME ZONE` | `true` | `false` | `now()`
`updated_at` | To record when this was updated | `TIMESTAMP WITH TIME ZONE` | `false` | `true` | `now()` (if updated, otherwise `null`)

[^1]:
  Only for their own listings

### Functions

#### `app.adjust_item_listing`

Normalizes a listing's `batched_by`, `cost`, and `quantity` based on their GCD.

### `app.set_listing_user_id`

Overrides the `user__id` field of a listing to whoever is authenticated via
PG's settings.

### Triggers

#### `app.normalize_tradable_item_listing`

Before insert, `batched_by`, `cost`, and `quantity` must be normalized based on
their GCD for GNAWEX to match item listings better.

#### `app.set_listing_user_id`

Before insert, it triggers `app.set_listing_user_id()` which sets the user ID
(owner) of the listing. The user ID is fetched from via
`current_setting('auth.user_id', TRUE)`. Hence, has to be set via
`set_config('auth.user_id', <ID_#_AS_STRING>, TRUE)`. It doesn't matter if the
user ID isn't provided in the insert query itself, or if it even is because
the above function will override it.
