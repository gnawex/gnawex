---
title: Proof of Item Listing Match
subtitle: Proposal#0001
description: How GNAWEX generates a proof of match
icon: material/checkbox-multiple-blank-circle-outline
---

# 0004 - Proof of Item Listing Match

:material-file-document: [Proposal#0001](../proposals/0001-Items.md)

## Description

Once GNAWEX finds one or more listings to match with the listing just created,
it has to generate one or more proofs that the listings were indeed matched.
This is the job for the **Item Listing Transaction**. The listing transaction(s)
must start off as `PENDING` for the parties involved to execute the exchange in
MouseHunt.

If the completed transactions add up to any of the involved listings' quantity,
then only the affected listings must be automatically delisted as it has nothing
else to exchange.

In the following diagram, we'll assume that all listings:

- are of item CRM; and
- cost the same SB

``` mermaid
graph LR
  LIST_ONE[/User: Larry<br/>Quantity: 25<br/>Batch: 4<br/>Type: Sell/] --> TXN_ONE[Transact#1<br/><br/>Quantity: 2];
  LIST_TWO[/User: Bob<br/>Quantity: 2<br/>Batch: 4<br/>Type: Buy/] --> TXN_ONE;
  TXN_ONE --> LIST_ONE_A[/User: Larry<br/>Quantity: 23<br/>Batch: 4<br/>Type: Sell/]
  TXN_ONE --> LIST_TWO_A[/User: Bob<br/>Quantity: 0<br/>Batch: 4<br/>Type: Buy/]

  LIST_ONE_A --> TXN_TWO[Transact#2<br/><br/>Quantity: 22]
  LIST_THREE[/User: Steve<br/>Quantity: 22<br/>Batch: 4<br/>Type: Buy/] --> TXN_TWO
  TXN_TWO --> LIST_ONE_B[/User: Larry<br/>Quantity: 1<br/>Batch: 4<br/>Type: Sell/]
  TXN_TWO --> LIST_THREE_B[/User: Steve<br/>Quantity: 0<br/>Batch: 4<br/>Type: Buy/]
```

New listings must not be created as a result of the transaction output. Rather,
this is just to show that provided with the transactions, we're able to determine
what the final quantity is for the item listing. The quantity column must not be
manually updated.

## :material-wrench: Operations

- Create pending: all transactions created must be `PENDING` first because GNAWEX does not
execute the actual exchange in MouseHunt
- Cancel: if both parties mutually agree to terminate the exchange
- Complete: if both parties have performed the exchange in MouseHunt

## :material-eye: Permissions

Role | Create pending TXN  | Cancel TXN | Complete TXN
:-- | -- | -- | --
System | :white_check_mark: | :x: | :x: |
Admin | :x: | :white_check_mark: | :white_check_mark:
Moderator | :x: | :white_check_mark: | :white_check_mark:
User | :x: | :white_check_mark: [^1] | :white_check_mark: [^1]
Guest (non-logged in user) | :x: | :x: | :x:

## :material-database: Database

### Tables

#### `app.tradable_item_transactions`

Column name | Description | Type | Required | Nullable | Default
:-- | -- | -- | -- | -- | --
`id` | To uniquely identify an item transaction | `BIGINT GENERATED ALWAYS AS IDENTITY` | `true` | `false` | Supplied by Postgres
`buy_item_listing_id` | To associate a transaction to a buy listing | `BIGINT` | `true` | `false` | -
`sell_item_listing_id` | To associate a transaction to a sell listing | `BIGINT` | `true` | `false` | -
`status` | What the status is of the transaction | `app.TRANSACTION_STATUS AS ENUM ('PENDING', 'COMPLETED', 'CANCELLED')` | `true` | `false` | `PENDING`
`buyer_agreed` | If the buyer agreed to the new status | `BOOLEAN` | `true` | `false` | `false`
`seller_agreed` | If the seller agreed to the new status | `BOOLEAN` | `true` | `false` | `false`
`created_at` | To record when this was created | `TIMESTAMP WITH TIME ZONE` | `true` | `false` | `now()`
`updated_at` | To record when this was updated | `TIMESTAMP WITH TIME ZONE` | `false` | `true` | `now()` (if updated, otherwise `null`)
`quantity` | How many of an item is being transacted | `INT` | `true` | `false` | -

[^1]:
  Only for transactions they're involved in as the seller/buyer

*[TXN]: Abbreviation of a listing transaction, not a DB transaction.
*[SB]: super|BRIE+ cheese
*[CRM]: Calcified Rift Mist (an MH item)
