---
title: Tradable Items
subtitle: Proposal#0001
description: Project spec on implementing tradable items
icon: material/checkbox-multiple-blank-circle-outline
---

# 0001 - Tradable Items

:material-file-document: [Proposal#0001](../proposals/0001-Items.md)

## Tradable Items

Tradable items are items that are sent via the _Send Supplies_. GNAWEX must
consistently keep an updated record of these items.

## :material-wrench: Operations

An item may be created, updated, and deleted. The delete operation must not be
a hard delete (i.e DB row is removed). Rather, the `deleted_at` field is set,
and will be considered to the back-end as non-existent.

## :material-eye: Permissions

Role | Read | Create | Update | Delete
:-- | -- | -- | -- | --
Admin | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark:
Moderator | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark:
User | :white_check_mark: | :x: | :x: | :x:
Guest (non-logged in user) | :white_check_mark: | :x: | :x: | :x:

## :material-database: Database

### Tables

#### `app.tradable_items`

Column name | Description | Type | Required | Nullable | Default
-- | -- | -- | -- | -- | --
`id` | To uniquely identify the item | `BIGINT GENERATED ALWAYS AS IDENTITY` | `true` | `false` | Supplied by Postgres
`name` | Name of an item | `TEXT` | `true` | `false` | -
`wiki_link` | MH wiki link to item | `TEXT` | `true` | `false` | -
`description` | The MH wiki description | `TEXT` | `true` | `false` | -
`created_at` | To record when this was created | `TIMESTAMP WITH TIME ZONE` | `true` | `false` | `now()`
`updated_at` | To record when this was updated | `TIMESTAMP WITH TIME ZONE` | `false` | `true` | `now()` (if updated, otherwise `null`)
`deleted_at` | To record weth this was deleted | `TIMESTAMP WITH TIME ZONE` | `false` | `true` | `now` (if deleted, otherwise `null`)
