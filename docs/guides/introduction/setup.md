# Setup

There isn't much to setup here since GNAWEX is still in its infancy. I'm
currently working on the database side of things before I start working on the
application-level, since PostgreSQL is surprisingly powerful, more than I
definitely expected.

## Database

I'm using version 14.1 of PostgreSQL, although 14.x should be fine. I don't
guarantee backwards compatibility with <14 because it's tiring.

### Creating `gnawex_db`

If you're using NixOS, you're in luck. In your `configuration.nix` file, just
add this:

```nix
services.postgresql = {
  enable = true;
  package = pkgs.postgresql_14;
  authentication = pkgs.lib.mkOverride 14 ''
    local all all trust
    host all all localhost trust
    host all all ::1/128 trust
  '';
};
```

This sets up a PostgreSQL server with a default user `postgres` (no password).
Currently, I'm not utilizing permissions and policies so using the default
`postgres` user is fine.

The project assumes you named the database as `gnawex_db`, which you can do
with `createdb gnawex_db --user postgres`.

### Applying migrations

For schema migrations, I'm using [`flyway`](https://github.com/flyway/flyway)
since it's quite convenient.

The migration scripts can be found in `./sql` from the project root. You can
manually load each `.sql` file with `\i path/to/file.sql` while in `psql`,
but `flyway` makes it easier.

**Example**

```sh
sekun@nixos ~/P/gnawex (chore/use-flyway-for-schema-migrations)> flyway migrate
Flyway is up to date
Flyway Community Edition 8.5.2 by Redgate
See what's new here: https://flywaydb.org/documentation/learnmore/releaseNotes#8.5.2

Database: jdbc:postgresql://localhost:5432/gnawex_db (PostgreSQL 14.1)
Successfully validated 6 migrations (execution time 00:00.017s)
Current version of schema "public": << Empty Schema >>
Migrating schema "public" to version "20220222233600 - create users"
Migrating schema "public" to version "20220222234200 - create items"
Migrating schema "public" to version "20220222234300 - create listings"
Migrating schema "public" to version "20220228153327 - create match listing"
Migrating schema "public" to version "20220301143724 - create role"
Migrating schema "public" to version "20220307020011 - create listings view"
Successfully applied 6 migrations to schema "public", now at version v20220307020011 (execution time 00:00.149s)
sekun@nixos ~/P/gnawex (chore/use-flyway-for-schema-migrations)> psql gnawex_db -U postgres
psql (14.1)
Type "help" for help.

gnawex_db=# \d
                  List of relations
 Schema |         Name          |   Type   |  Owner   
--------+-----------------------+----------+----------
 public | flyway_schema_history | table    | postgres
 public | item_listings         | view     | postgres
 public | items                 | table    | postgres
 public | items_id_seq          | sequence | postgres
 public | listings              | table    | postgres
 public | listings_id_seq       | sequence | postgres
 public | listings_item_id_seq  | sequence | postgres
 public | listings_user_id_seq  | sequence | postgres
 public | transactions          | table    | postgres
 public | users                 | table    | postgres
 public | users_hunter_id_seq   | sequence | postgres
 public | users_id_seq          | sequence | postgres
(12 rows)
```

> **Note:** I'm no longer maintaining the old scripts in `bin/functions.fish`.
> So don't bother using them! All but the seeding script.

### Viewing migration statuses

If you're curious which migrations have been applied, you can use
`flyway info`, and the output would look something like this:

```
sekun@nixos ~/P/gnawex (chore/use-flyway-for-schema-migrations)> flyway info
Flyway is up to date
Flyway Community Edition 8.5.2 by Redgate
See what's new here: https://flywaydb.org/documentation/learnmore/releaseNotes#8.5.2

Database: jdbc:postgresql://localhost:5432/gnawex_db (PostgreSQL 14.1)
Schema version: 20220307020011

+-----------+----------------+----------------------+------+---------------------+---------+
| Category  | Version        | Description          | Type | Installed On        | State   |
+-----------+----------------+----------------------+------+---------------------+---------+
| Versioned | 20220222233600 | create users         | SQL  | 2022-03-07 14:42:29 | Success |
| Versioned | 20220222234200 | create items         | SQL  | 2022-03-07 14:42:29 | Success |
| Versioned | 20220222234300 | create listings      | SQL  | 2022-03-07 14:42:29 | Success |
| Versioned | 20220228153327 | create match listing | SQL  | 2022-03-07 14:42:29 | Success |
| Versioned | 20220301143724 | create role          | SQL  | 2022-03-07 14:42:29 | Success |
| Versioned | 20220307020011 | create listings view | SQL  | 2022-03-07 14:42:29 | Success |
+-----------+----------------+----------------------+------+---------------------+---------+
```

The `State` column tells you which migrations have been successfully applied.

### `pgAdmin`

`pgAdmin` can be pretty useful, and I personally use it for `EXPLAIN` to
understand the query planner better. You can either set it up manually, or use
nix.

```sh
# At the project root, `gnawex/`

# Run the nix shell. If you're using nix-direnv, then it should automatically
# load the environment for you. So you can skip this, if ever.
nix develop

# If you're using sudo, replace doas with sudo
doas pgadmin4
```

You'll need elevated privileges since `pgadmin` needs write permissions in
`/var/lib/pgadmin`. Doing this for the first time will ask for credentials
you want to set, it doesn't really matter since it's just for a dev env, just
remember it.

You should see something like this:

```sh
sekun@nixos ~/P/gnawex (main) [1]> doas pgadmin4
doas (sekun@nixos) password: 
NOTE: Configuring authentication for SERVER mode.

Enter the email address and password to use for the initial pgAdmin user account:

Email address: postgres@example.com
Password: 
Retype password:
Starting pgAdmin 4. Please navigate to http://127.0.0.1:5050 in your browser.
2022-03-01 12:12:37,060: WARNING	werkzeug:	WebSocket transport not available. Install eventlet or gevent and gevent-websocket for improved performance.
 * Serving Flask app 'pgadmin' (lazy loading)
 * Environment: production
   WARNING: This is a development server. Do not use it in a production deployment.
   Use a production WSGI server instead.
 * Debug mode: off
```

`pgadmin` should be available in the URL specified above. In my case, it's
http://127.0.0.1:5050.

