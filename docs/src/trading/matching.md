# Matching

## Partial fulfillment

Once you've created a bid, GNAWEX will attempt to look for one or more asks that
match your price point and quota. It doesn't guarantee that it can fulfill your
entire order; Partial orders are possible.

## Multiple TXNs

As mentioned earlier, it is also possible for GNAWEX to involve more than one
listing. If your bid cannot be fulfilled by one ask listing but can be done by
two, two transactions will be created.

```mermaid
flowchart LR
  AskA[Ask 1: 200SB x 2 TSR] --> TxnA([Transaction 1])
  Bid[Bid: 200SB x 6 TSR] --> TxnA

  AskB[Ask 2: 200SB x 5 TSR] --> TxnB

  TxnB --> NewAskB[Ask 2: 200SB x 1 TSR]
  TxnA --> NewAskA[Ask 1: 200SB x 0 TSR]

  TxnA --> NewBid[Bid: 200SB x 4 TSR]

  NewBid --> TxnB([Transaction 2])
  TxnB --> NewNewBid[Bid: 200SB x 0 TSR]
```
