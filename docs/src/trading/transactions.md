# Transactions (TXNs)

A **transaction** involves a bid and ask. These listings involved can be more
than one depending on the situation.

> **Note:** The transaction flow is identical with items, maps, and bounties.
> The only difference is the data needed.

> **Note:** The transaction flow is identical with items, maps, and bounties. The only difference is the data needed.

```mermaid
flowchart LR
  Ask[Ask: 200SB x 5 TSR] --> C([Transaction 1])
  Bid[Bid: 200SB x 1 TSR] --> C
  C --> NewAsk[Ask: 200SB x 4 TSR]
  C --> NewBid[Bid: 200SB x 0 TSR]
```

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
