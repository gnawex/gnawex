# Item Listings

Users may create listings for [[Tradable Items]] if they wish to exchange it. Item listings have two types: 1) BUY, and 2) SELL. A user may have more than one listing for the same item, as well as listings of opposite types.

> [!note]
> A user may only have a **max of 10** listings overall. i.e across different items, listing types, etc.

A simple listing will contain the ff data:
- User ID: User who created the listing
- Item ID: Item being exchanged
- Date created: Date the listing was created
- Type: BUY or SELL
- Cost: How much to buy/sell for
- Quantity: How many of something to exchange

This might seem enough but one has to remember that not all items can be exchanged for a whole number amount of SB (e.g 0.5 SB for 1 _Item A_). At the same time, it's not possible to send 0.5 SB to someone! To include items with not-so-ideal costs, _batching_ has to happen.

## Batching

A user could BUY/SELL by batches if the cost of an item is not a nice whole number. Say we have 100 of some _Item A_ that we would like to SELL for 0.25 SB, we could batch it by 4s so that we could bump up the cost to 1 SB. Then, the minimum quantity that can be transacted will be 4x of _Item A_, and will at least cost 1 SB.

Here's an example listing of the above:

User | Item | Type | Batch | Quantity | Cost
---- | ---- | ---- | ----- | -------- | ----
Larry | Calcified Rift Mist | BUY  | 4     | 25       | 1

So every 4 of CRM will be bought as one unit. See [[Transactions]] for a better example.