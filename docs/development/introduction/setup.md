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
You can create a user/role that matches with your OS' user so that you don't
need to keep appending `psql`/`sqitch` commands with `-U postgres`, and
`--db-user postgres` respectively. We'll get to what `sqitch` is in a bit.

My OS user is `sekun`. Replace `sekun` with whatever yours is.

```
$ psql -U postgres
postgres# CREATE ROLE sekun SUPERUSER CREATEDB CREATEROLE LOGIN;
CREATE ROLE
```

This creates a passwordless role `sekun`. Now you can use `psql`/`sqitch`
without specifying the `postgres` user.

This project needs a database called `gnawex_db`. Create one with this:
`createdb gnawex_db`.

### Applying migrations

For schema migrations, I'm using [`sqitch`](https://sqitch.org) out of
convenience. If you're using `nix`, it should already be a part of the nix
dev shell environment (how convenienit). If you're using something else, check
out their website for installation instructions.

`sqitch` is interested in 3 folders: `deploy`, `revert`, and `verify`. `deploy`
contains the migration scripts that you want to apply. `revert` when you want
to rollback, and `verify` checks if things were applied as you expect them to
be. These scripts are just plain SQL. Whatever is considered valid by
PostgreSQL is fine.

> By this stage, it is assumed that you've setup PostgreSQL 14.1, created
> `gnawex_db`, and have `sqitch` installed.

**Example**

```sh
[sekun@nixos:~/Projects/gnawex]$ sqitch deploy
Deploying changes to db:pg:gnawex_db
  + pgcrypto .......... ok
  + citext ............ ok
  + pgtap ............. ok
  + app_schema ........ ok
  + users ............. ok
  + items ............. ok
  + listings .......... ok
  + transactions ...... ok
  + match_listings .... ok
  + roles ............. ok
  + role_permissions .. ok
```

If ever something goes wrong, it'll revert it to the last change that succeeded.
But that's what it looks like if all goes well.

### Viewing migration statuses

To see what migration `sqitch` has applied, you can use `sqitch status`:

```
[sekun@nixos:~/Projects/gnawex]$ sqitch status
# On database db:pg:gnawex_db
# Project:  gnawex
# Change:   117b3847f4e26b654dcd925378ef7ff4810fd641
# Name:     role_permissions
# Deployed: 2022-03-08 16:20:32 +0800
# By:       sekun <sekun@nixos>
#
Nothing to deploy (up-to-date)
```

Here it tells you that the latest change is `role_permissions`. Which is good
because at the time of writing, this is the latest migration we want. If you
want to see the list of migrations planned, you can use `sqitch plan`:

```
# Project: gnawex
# File:    sqitch.plan

Deploy 3d07e4afe1abd5eb6fbd5e350650da0535f89ccc
Name:      pgcrypto
Planner:   sekun <sekun@nixos>
Date:      2022-03-08 11:53:13 +0800

    For encryption

Deploy 895451e8c444a571ae42a040d6496c14be5e6240
Name:      citext
Planner:   sekun <sekun@nixos>
Date:      2022-03-08 11:53:41 +0800

    For case-insensitive text

Deploy 66bda001775a8d943894a15e9745a1cd985cfdbd
Name:      pgtap
Planner:   sekun <sekun@nixos>
Date:      2022-03-08 11:54:07 +0800

    For pg unit tests

Deploy ea57600af26c2141dd329660db853152453ff934
Name:      app_schema
Planner:   sekun <sekun@nixos>
Date:      2022-03-08 12:02:39 +0800

    GNAWEX app schema

Deploy cc0ce225344b889bcb8d42db3b501b8cca714f0e
Name:      users
Planner:   sekun <sekun@nixos>
Date:      2022-03-08 12:04:23 +0800

    GNAWEX users

Deploy afb388906019a2959b671c76f625e939077a5848
Name:      items
Planner:   sekun <sekun@nixos>
Date:      2022-03-08 14:56:21 +0800

    Items that can be traded

Deploy dbd0e796eacee0ef094baa45ffee698f760e541f
Name:      listings

...
```

### Rolling back migrations

`sqitch revert`

WIP

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

