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

