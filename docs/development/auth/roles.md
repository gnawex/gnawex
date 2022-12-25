# Roles

Roles are used to determine who can perform what operation in GNAWEX.

| Name          | Description                                                                       |
|---------------|-----------------------------------------------------------------------------------|
| authenticator | The entrypoint role that GNAWEX starts with.                                      |
| anon          | The role that GNAWEX will switch to when the user is not authenticated.           |
| verified_user | The role that GNAWEX will switch to when the user is authenticated, and verified. |
| banned_user   | The role that GNAWEX will switch to when the user is authenticated, but banned.   |
| auth          | Role that owns the `auth` schema.                                                 |
| api           | Role that owns the `api` schema.                                                  |

## User roles

These ones are defined with `CREATE TYPE ROLE as ENUM (...)`. These are the
roles you'll see in GNAWEX. So far, none of these roles were worked on since
the DB is still actively worked on.

Although, here is what I have in mind:

- `admin`: The system administrator
  - Can appoint moderators
  - Maintain `items`
  - Privileges like banning/unbanning users and mods
- `mod`: Moderates the platform
- `user`: Traders, basically

Anything the user can do, the mod and admin can as well. So all roles here are
able to create listings.

## Database roles

Database roles exist because I'm deferring authorization to Postgres. You can
get the list of roles in the DB by running this in `psql`: `\du`; and this
would show something like this:

```
gnawex_db=> \du
                                        List of roles
    Role name    |                         Attributes                         |  Member of
-----------------+------------------------------------------------------------+--------------
 anon            | Cannot login                                               | {}
 authenticator   | No inheritance                                             | {anon}
 gnawex_merchant | Cannot login, Bypass RLS                                   | {anon}
 postgres        | Superuser, Create role, Create DB, Replication, Bypass RLS | {}
 sekun           |                                                            | {valid_user}
 valid_user      | Cannot login                                               | {}
```

Here's a summary of what each of them does:

- `anon`: Public role. Can view listings, and items.
- `authenticator`: Role that is used to login. This role is still being worked
  on to really flesh out what it's supposed to do.
- `gnawex_merchant`: This isn't meant for anyone, rather this role is used to
  facilitate the exchanging of two listings. Since normal users cannot update
  their own listings, much more other people's listings, a special role needs
  to exist for that. You can think of it as GNAWEX updating the listings in
  your behalf when your listing gets matched with another.
- `postgres`: The highest role achievable. Can do anything.
- `sekun`: An example of a `valid_user`. You can ignore this since I just use
  this for testing.
- `valid_user`: This role is granted to users that are: 1) verified, and 2) not
  banned.

