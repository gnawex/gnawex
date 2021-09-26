# Trading

## Listings

An **ask** and **buy** each require the same information:

- Amount - The quantity you want to bid/ask for
- Price - The price per batch
- Batch - The smallest amount of items allowed to be transacted

Batches are needed for scenarios where items are priced <1SB each. With batching, you can sell 2 Enerchi charms for 1 SB+ per batch.

## Transactions (TXNs)

A transaction involves a bid and ask. These listings involved can be more than one depending on the situation.

```mermaid
flowchart LR
  Ask[Ask: 200SB x 5 TSR] --> C([Transaction 1])
  Bid[Bid: 200SB x 1 TSR] --> C
  C --> NewAsk[Ask: 200SB x 4 TSR]
  C --> NewBid[Bid: 200SB x 0 TSR]
```

### Expiration

Transactions automatically expire 5 minutes after you've gone offline. The justification for this is that the involved parties must be active to ensure swift fulfillment.

## The transaction flow

Both flows of buy and sell are identical.

```mermaid
flowchart TD
  A([Trader creates listing]) --> B[Listing created];
  B --> C{Has match?};
  C --> |Yes| H[Ask & bid reduced accordingly];
  H --> |and| D[Create one or more pending TXNs];
  D --> |and| Info[Exchange hunter information per TXN];
  C --> |No| E((End));
  Info --> F[/Traders: complete/cancel TXN/];
  F --> G{Close?};
  G --> |Completed| J[TXN marked as completed];
  G --> |Cancelled| I[TXN marked as cancelled];
  I --> |and| K[Revert ask & bid amount]
  K --> E
  J --> E
```

## Matching

### Partial fulfillment

Once you've created a bid, GNAWEX will attempt to look for one or more asks that match your price point and quota. It doesn't guarantee that it can fulfill your entire order; Partial orders are possible.

### Multiple TXNs

As mentioned earlier, it is also possible for GNAWEX to involve more than one listing. If your bid cannot be fulfilled by one ask listing but can be done by two, two transactions will be created.

```mermaid
flowchart LR
  AskA[Ask 1: 200SB x 2 TSR] --> TxnA([Transaction 1])
  Bid[Bid: 200SB x 6 TSR] --> TxnA

  Bid --> TxnB([Transaction 2])
  AskB[/Ask 2: 200SB x 5 TSR/] --> TxnB

  TxnB --> NewAskB[Ask 2: 200SB x 0 TSR]
  TxnA --> NewAskA[Ask 1: 200SB x 1 TSR]

  TxnA --> NewBid[Bid: 200SB x 0 TSR]
  TxnB --> NewBid
```

## Terms

- Txn - Shorthand for *transaction*
- Ask - A sell listing
- Bid - A buy listing
- Transaction - When an ask and bid match, a transaction is created with these as inputs to associate these two listings, along with information such as amount involved.

