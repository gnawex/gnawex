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
    host all all ::1/128 trust
  '';
};
```

This sets up a PostgreSQL server with a default user `postgres` (no password).
Currently, I'm not utilizing permissions and policies so using the default
`postgres` user is fine.

The project assumes you named the database as `gnawex_db`, which you can do
with `createdb gnawex_db --user postgres`.

### Running migration scripts

The migration scripts can be found in `migrations/` at the project root. You
can manually load each `.sql` file with `\i path/to/file.sql`. If you're using
`fish` shell, you could do this:

```sh
# Load the functions
source bin/functions.fish

migrate
```

> *Why manually write this?* I'm manually managing migration files since this
> project isn't that large, and neither is it that important where I need 
> utmost care. It's a great exercise to manually write migration files anyway.

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

