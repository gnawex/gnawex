# Items

Items that are allowed to be exchanged are ones (including gilded treasure maps) that may be traded via the _Send Supplies_ option in a MouseHunt profile. These must be exchanged with/for SUPER|brie+ (SB). ^91e5f9

GNAWEX has to have the ff data for its items:

- Name
- Description
- Image
- MH Wiki link

## Listings

Users may create listings for [[Items]] if they wish to exchange it. Item listings have two types: 1) BUY, and 2) SELL. A user may have more than one listing for the same item, as well as listings of opposite types.

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

### Batching

A user could BUY/SELL by batches if the cost of an item is not a nice whole number. Say we have 100 of some _Item A_ that we would like to SELL for 0.25 SB, we could batch it by 4s so that we could bump up the cost to 1 SB. Then, the minimum quantity that can be transacted will be 4x of _Item A_, and will at least cost 1 SB.

Here's an example listing of the above:

- User: Larry
- Item: Calcified Rift Mist
- Type: `BUY`
- Batch 4
- Quantity: 25
- Cost: 1

Every 4 of <span title="Acronym for Calcified Rift Mist">CRM</span> will be bought as one unit.

### Modifying a listing

Once posted, a listing cannot be deleted but its _quantity_ may be edited but must take note of the following:

- The user can not increase the quantity of an active listing.
- The user may decrease the quantity of a listing as long as there is at least that much left not involved in a `PENDING` or `COMPLETED` transaction. e.g If the listing currently has a quantity of 25 that has yet to be involved in a `PENDING`/`COMPLETED` transaction, then they may reduce it by a max of 25. 

### Closing a listing

#### Automated

A listing must be automatically closed if the user reduces the listing quantity to 0, and if it is not involved in any `PENDING` transactions. Generally speaking, all listings must be closed when their quantities have been exchanged, and if all transactions facilitating those exchanges have `COMPLETED`.

#### Manual

A user may close a listing without the "reducing quantity to 0" requirement. But they must settle all `PENDING` transactions first.

## Transactions (TXNs)

A transaction (TXN) is a proof that a match occurred between a BUY and a SELL listing that is automatically created by GNAWEX. It also details the quantity of an item that was transacted along with other metadata such as timestamps.

The automated process will primarily rely on the listings' `cost`, `batched_by`, and `quantity`. The `cost` and `batched_by` have to be exactly the same, while the `quantity` only needs to match at least one. e.g quantities 1 and 10 will still match.

### Multiple TXNs of the same listing

It's possible for BUY listing to be matched with multiple SELL listings, and vice versa, if the BUY listing's quantity cannot be fulfilled by just one SELL listing.

Here's an example:

> [!info]
> The following tables are not supposed to be representations of the DB tables. Rather, just used to illustrate a point to the reader.
> 
> We're also going to assume that the following listings are of the same item.

ID | User | Type | Batch | QTY | Cost
-- |---- | ---- | ----- | -------- | ----
3  | Larry | BUY  | 4 | 100 | 1
2  | Bob | SELL | 4 | 25 | 1
1  | Steve | SELL | 4| 50 | 1

We know that Larry's listing can be matched with both Bob's and Steve's listings, and even more. Say Larry's listing is created _after_ the others, this will trigger a match, and generate two transactions: a transaction between Larry and Bob, and one between Larry and Steve. 

Buy ID | Sell ID | Amount | Status
-- | -- | -- | --
3 | 2  | 25 | PENDING
3 | 1 | 50 | PENDING

Item listings after the transactions:

User | Type | Batch | QTY | Cost
---- | ---- | ----- | -------- | ----
Larry | BUY  | 4 | 25 | 1
Bob | SELL | 4 | 0 | 1
Steve | SELL | 4| 0 | 1

Now, both Bob and Steve have their listings fulfilled while Larry needs one or more sellers to fulfill the last 25.

### Status

Since we do not have access to MouseHunt's API, we cannot automatically fulfill the transaction in the users' behalf. Instead, we provide them with the other's information (like their Hunter ID) for them to fulfill the exchange, and then have them mark it as done.

Here are the possible states (status) of a transaction:

- `PENDING`: Newly created and has not yet been acted upon. This 'holds' the item quantity being transacted.
- Conclusive statuses
	- `COMPLETED`: When both parties complete the exchange on their end. This indefinitely holds the item quantity being transacted.
	- `CANCELLED`: When a user gets cold feet and decides to not push through with the exchange. This 'releases' the item quantity being transacted, and thus may be involved in another future transaction.
> [!info]
> To conclude a transaction, both parties (the buyer and seller) need to mark the transaction as such. Otherwise, the transaction will still be considered as `PENDING`.

tl;dr: `PENDING`, and `COMPLETED` transactions will hold their respective item quantities. When held, this will reduce the listing quantity by the transactions quantity. When `CANCELLED`, its quantity is restored back and may be involved in future transactions.

## FAQ

> [!question]- How do I match my item listing with another user's?
> This is done automatically by GNAWEX.

> [!question]- Why can't I trade SB for SB?
> It's pointless!

> [!question]- How do I create a transaction?
> This must not be manually done. There is no need to manually ask another user if they would like to exchange something because GNAWEX performs the matching in behalf of the user. When GNAWEX does so, it also automatically creates a transaction linking these two listings together.

> [!question]- What happens to the item quantity while the transaction is `PENDING`?
> The quantity involved in the transaction must be held, and must not be involved in another transaction.

> [!question]- What happens to the item quantity when the transaction is `CANCELLED`?
> The quantity may be involved in another transaction.

> [!question]- What happens to the item quantity when the transaction is `COMPLETED`?
> It must not be involved in future transactions.

> [!question]- May I modify a transaction's quantity?
> No. Besides the transaction status, all of the other information must be handled by GNAWEX.
