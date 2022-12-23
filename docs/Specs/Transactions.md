# Transactions (TXNs)

A transaction (TXN) is a proof that a match occurred between a BUY and a SELL listing. It also details the quantity of an item that was transacted along with other metadata such as timestamps. 

## Multiple TXNs of the same listing

It's possible for BUY listing to be matched with multiple SELL listings, and vice versa, if the BUY listing's quantity cannot be fulfilled by just one SELL listing.

Here's an example:

> [!info]
> The following tables are not supposed to be representations of the DB tables. Rather, just used to illustrate a point to the reader.

ID | User | Item | Type | Batch | Quantity | Cost
-- |---- | ---- | ---- | ----- | -------- | ----
3  | Larry | Calcified Rift Mist | BUY  | 4 | 100 | 1
2  | Bob | Calcified Rift Mist | SELL | 4 | 25 | 1
1  | Steve | Calcified Rift Mist | SELL | 4| 50 | 1

We know that Larry's listing can be matched with both Bob's and Steve's listings, and even more. Say Larry's listing is created _after_ the others, this will trigger a match, and generate two transactions: a transaction between Larry and Bob, and one between Larry and Steve. 

Buy Listing | Sell Listing | Amount | Transacted At | Status
-- | -- | -- | -- | --
3 | 2  | 25 | 22/12/2022 | PENDING
3 | 1 | 50 | 22/12/2022 | PENDING

Item listings after the transactions:

User | Item | Type | Batch | Quantity | Cost
---- | ---- | ---- | ----- | -------- | ----
Larry | Calcified Rift Mist | BUY  | 4 | 25 | 1
Bob | Calcified Rift Mist | SELL | 4 | 0 | 1
Steve | Calcified Rift Mist | SELL | 4| 0 | 1

Now, both Bob and Steve have their listings fulfilled while Larry needs one or more sellers to fulfill the last 25.

> [!question] What happens to the item quantity while the transaction is on-going, and what happens afterwards?
> With the above example, a certain amount is held. This amount cannot be involved in another transaction if it's still `PENDING`, or if it gets marked as `COMPLETED`. It's still possible for the quantity to be 'released' back if the transaction gets `CANCELLED`.

## Status

Since we do not have access to MouseHunt's API, we cannot automatically fulfill the transaction in the users' behalf. Instead, we provide them with the other's information (like their Hunter ID) for them to fulfill the exchange, and then have them mark it as done.

Here are the possible states (status) of a transaction:

- PENDING: Newly created and has not yet been acted upon. This 'holds' the item quantity being transacted.
- COMPLETED: When a user completes the exchange on their end. This indefinitely holds the item quantity being transacted.
- CANCELLED: When a user gets cold feet and decides to not push through with the exchange. This 'releases' the item quantity being transacted, and thus may be involved in another future transaction.