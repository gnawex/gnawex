---
title: Automated Matching
subtitle: Proposal#0001
description: Have GNAWEX automatically match item listings for the user
icon: material/checkbox-multiple-blank-circle-outline
---

# 0003 - Automated Item Listings Matching

:material-file-document: [Proposal#0001](../proposals/0001-Items.md)

## Description

Item listings may be matched by GNAWEX if it is able to. The matching will
depend mainly on the listing's `type` (match buy with sell, and vice versa),
`batched_by`, and `cost`. It should try to do the same with `unit_quantity`,
but since the quantity doesn't have to match exactly for GNAWEX to do so, it
isn't a hard requirement.

The main priority is by listing date since matches should be done in a FIFO
basis.



## :material-eye: Permissions

Role | Avail automated matching feature
:-- | --
Admin | :white_check_mark:
Moderator | :white_check_mark:
User | :white_check_mark:
Guest (non-logged in user) | :x:

## :material-database: Database

The implementation of the matching may either be implemented in the DB-level,
which has already been experimented in [PR#35](https://github.com/gnawex/gnawex/pull/35),
or done in the application-level for reasons like type-safety. The former still
requires all operations necessary to be in the same transaction, with 'safe
enough' transaction settings.

But should it be done in the DB-level, these are what's needed (at the top level):

### Triggers

#### `app.match_listings`

After insert, and for each row inserted to `app.tradable_item_listings`, the
trigger will invoke the function below.

### Functions

#### `app.match()`

Should match the listing that was inserted with all possible listings that
fulfill its `unit_quantity`.

*[FIFO]: First In, First Out. Or first come, first served.
*[DB]: Database
