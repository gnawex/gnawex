# GNAWEX

A third-party exchange for [MouseHunt](https://mousehuntgame.com).

## Project structure

```
.
├── bin           Scripts
├── docs          Documentation for the entire project
├── flake.lock    Lock file for Nix
├── flake.nix     Reproducible dev environments
├── LICENSE       How you can use the software, and other terms
├── migrations    Migration scripts
├── README.md     Read it!
└── seeds         Database seeds
```

## Getting started

Note: I'm currently experimenting with a lot of stuff, especially with `plpgsql`
, so the setup is pretty barebones at the moment. No UI, and all that stuff.

The setup requires PostgreSQL 14.1 which should come with both the DB server,
and a terminal client `psql`.

Make sure you start with a fresh database. You can name it whatever you like
as of now, but I'll be using `gnawex_dev_db`, and `gnawex_db` for dev, and prod
respectively. You could also just drop the entire database if you want.

1. Create the database `createdb gnawex_db --username postgres`

2. Run the migrations. You have to set the `user`/`password`/`host`/`port` if
   ever it varies from the default.

```sh
psql \
  --user postgres \
  --dbname=gnawex_db \
  --file migrations/2022-02-22-23-36-00_create_users.sql && \
psql \
  --dbname=gnawex_db \
  --user postgres \
  --file migrations/2022-02-22-23-42-00_create_items.sql && \
psql \
  --dbname=gnawex_db \
  --user postgres \
  --file migrations/2022-02-22-23-43-00_create_listings.sql
```

3. Seed the database

```sh
psql \
  --user=postgres \
  --dbname=gnawex_db \
  --command "\copy users FROM 'seeds/users.csv' delimiter ',' csv header" && \
psql \
  --user postgres \
  --dbname=gnawex_db \
  --command "\copy items FROM 'seeds/items.csv' delimiter ',' csv header" && \
psql \
  --user postgres \
  --dbname=gnawex_db \
  --command "\copy listings FROM 'seeds/listings.csv' delimiter ',' csv header"
```

### Project structure

- `migrations/`: Tables, functions, procedures, triggers, etc.
- `seeds/`: Sample seed data. Right now it's pretty basic.
