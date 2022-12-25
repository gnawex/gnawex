# Available scripts

> In the future, I'm going to rely on `Makefile` instead of `fish` scripts
> since it's tedious to manually load the script into the environment.

For these scripts to work, you need `psql` and `postgresql` setup. `psql` comes
with `postgresql`.

## `migrate`

Takes **all** migration files from `migrations/`, and loads it in `gnawex_db`.

```sh
# This is pretty much it
migrate
```

## `seed-db`

`seed-db` expects 1 argument, which can either be `dev`, or `prod`. If you're
using this for a dev environment, use `dev`, otherwise if it's for production
then use `prod`. You know how it goes. The seed files are in `seeds/` as `.csv`
with headers, and delimited with `,`.

Here are the files those two look for:

- `seed-db dev`
  - `seeds/users.csv`: Dummy users
  - `seeds/items.csv`: Tradable MouseHunt items
  - `seeds/listings.csv`: Dummy listings
- `seed-db prod`: **TODO**

## `drop-db`

Drops `gnawex_db`.

## `reset-db`

This is a convenience function that resets `gnawex_db`.

```sh
drop-db
createdb gnawex_db --user postgres
migrate
seed-db dev

# Or you could just run this:
reset-db
```
