searchNodes=[{"doc":"","ref":"gnawex.html","title":"GNAWEX","type":"extras"},{"doc":"There isn't much to setup here since GNAWEX is still in its infancy. I'm currently working on the database side of things before I start working on the application-level, since PostgreSQL is surprisingly powerful, more than I definitely expected.","ref":"setup.html","title":"Setup","type":"extras"},{"doc":"I'm using version 14.1 of PostgreSQL, although 14.x should be fine. I don't guarantee backwards compatibility with &lt;14 because it's tiring. Creating gnawex_db If you're using NixOS, you're in luck. In your configuration.nix file, just add this: services.postgresql = { enable = true; package = pkgs.postgresql_14; authentication = pkgs.lib.mkOverride 14 '' local all all trust host all all localhost trust host all all ::1/128 trust ''; }; This sets up a PostgreSQL server with a default user postgres (no password). Currently, I'm not utilizing permissions and policies so using the default postgres user is fine. The project assumes you named the database as gnawex_db , which you can do with mix ecto.create . Applying migrations For schema migrations, I'm using ecto since it's quite convenient to rely on its migrations feature, and I can execute SQL scripts with execute/1 if I parse it properly. The migration scripts can be found in migrations/priv/repo/migrations from the project root. You can manually load each .sql file with \\i path/to/file.sql while in psql , but ecto helps make it more convenient. NOTE: You'll need elixir and erlang setup for this. If you're using nix-direnv , and have nix flakes enabled, you don't have to worry about getting the right dependencies since I already set things up. Example [sekun@nixos:~/Projects/gnawex]$ cd migrations direnv: loading ~/Projects/gnawex/migrations/.envrc direnv: using flake direnv: using cached dev shell direnv: export +AR +AS +CC +CONFIG_SHELL +CXX +HOST_PATH +IN_NIX_SHELL +LD +NIX_BINTOOLS +NIX_BINTOOLS_WRAPPER_TAR GET_HOST_x86_64_unknown_linux_gnu +NIX_BUILD_CORES +NIX_CC +NIX_CC_WRAPPER_TARGET_HOST_x86_64_unknown_linux_gnu +N IX_CFLAGS_COMPILE +NIX_ENFORCE_NO_NATIVE +NIX_HARDENING_ENABLE +NIX_INDENT_MAKE +NIX_LDFLAGS +NIX_STORE +NM +OBJCO PY +OBJDUMP +RANLIB +READELF +SIZE +SOURCE_DATE_EPOCH +STRINGS +STRIP +buildInputs +buildPhase +builder +configure Flags +depsBuildBuild +depsBuildBuildPropagated +depsBuildTarget +depsBuildTargetPropagated +depsHostHost +depsHos tHostPropagated +depsTargetTarget +depsTargetTargetPropagated +doCheck +doInstallCheck +dontAddDisableDepTrack +na me +nativeBuildInputs +out +outputs +patches +phases +propagatedBuildInputs +propagatedNativeBuildInputs +shell +s hellHook +stdenv +strictDeps +system ~PATH ~XDG_DATA_DIRS # You should probably run `mix deps.get` first before this line if it&#39;s your # first time running it. [sekun@nixos:~/Projects/gnawex/migrations]$ mix do ecto.create, ecto.migrate The database for Migrations.Repo has already been created 19:38:03.252 [info] == Running 20220305031642 Migrations.Repo.Migrations.CreateUsers.up/0 forward 19:38:03.255 [info] execute &quot;--------------------------------------------------------------------------------\\n-- `anon` permissions\\n-- NOTE: `anon` is pretty much public&quot; 19:38:03.256 [info] execute &quot;CREATE ROLE anon;&quot; 19:38:03.256 [info] execute &quot;GRANT USAGE ON SCHEMA public TO anon;&quot; 19:38:03.257 [info] execute &quot;--------------------------------------------------------------------------------\\n-- `authenticator` serves as an alternative for `postgres` with less elevated\\n-- permissions. Use this to connect t o the database, or something.&quot; 19:38:03.257 [info] execute &quot;-- NOTE: I don&#39;t know how to set this for prod\\n-- TODO: Use passwd\\nCREATE ROLE aut henticator NOINHERIT LOGIN PASSWORD &#39;foobarbaz&#39;;&quot; 19:38:03.260 [info] execute &quot;GRANT anon TO authenticator;&quot; 19:38:03.261 [info] execute &quot;--------------------------------------------------------------------------------&quot; 19:38:03.261 [info] execute &quot;CREATE TYPE ROLE AS ENUM (&#39;admin&#39;, &#39;mod&#39;, &#39;user&#39;);&quot; ... If you take a look at one of the scripts, it would look something like this: defmodule Migrations.Repo.Migrations.CreateUsers do use Ecto.Migration import Migrations def up do execute_file &quot;./sql/20220222233600_create_users.sql&quot; end def down do execute_each &quot;&quot;&quot; DROP TABLE users; DROP TYPE ROLE; DROP ROLE authenticator; REVOKE USAGE ON SCHEMA public FROM anon; DROP ROLE anon; &quot;&quot;&quot; end end execute_each/1 and execute_file/1 are helper functions I wrote to handle executing each statement since postgrex cannot handle a bunch of statements in a single execute/1 . So essentially, this reads the SQL script, does some basic parsing behind the scenes (it just splits it by double newlines), and runs execute on each list item. up/0 is responsible for applying changes to the schema, while down/0 reverts it. Note: I'm no longer maintaining the old scripts in bin/functions.fish . So don't bother using them! Viewing migration statuses If you're curious which migrations have been applied, you can use mix ecto.migrations , and the output would look something like this: [ sekun @nixos : ~ / Projects / gnawex / migrations ] $ mix ecto . migrations Repo : Migrations.Repo Status Migration ID Migration Name -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- up 20220305031642 create_users up 20220305041253 create_items up 20220305044710 create_listings up 20220305110451 create_match_listing up 20220305112208 create_role The Status column tells you which migrations have been applied, if ever one is reverted, it would be down instead of up . Rolling back migrations If you made changes to how the dropping of a migration is done, or if you just want to drop a migration, you can use mix ecto.rollback --to &lt;MIGRATION_ID&gt; . Here's an example: [ sekun @nixos : ~ / Projects / gnawex / migrations ] $ mix ecto . migrations Repo : Migrations.Repo Status Migration ID Migration Name -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- up 20220305031642 create_users up 20220305041253 create_items up 20220305044710 create_listings up 20220305110451 create_match_listing up 20220305112208 create_role [ sekun @nixos : ~ / Projects / gnawex / migrations ] $ mix ecto . rollback -- to 20220305112208 19 : 46 : 31.763 [ info ] == Running 20220305112208 Migrations.Repo.Migrations.CreateRole . down / 0 forward 19 : 46 : 31.766 [ info ] execute &quot;DROP POLICY txn_read ON transactions;&quot; ... 19 : 46 : 31.772 [ info ] == Migrated 20220305112208 in 0.0 s [ sekun @nixos : ~ / Projects / gnawex / migrations ] $ mix ecto . migrations Repo : Migrations.Repo Status Migration ID Migration Name -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- up 20220305031642 create_users up 20220305041253 create_items up 20220305044710 create_listings up 20220305110451 create_match_listing down 20220305112208 create_role pgAdmin pgAdmin can be pretty useful, and I personally use it for EXPLAIN to understand the query planner better. You can either set it up manually, or use nix. # At the project root, `gnawex/` # Run the nix shell. If you&#39;re using nix-direnv, then it should automatically # load the environment for you. So you can skip this, if ever. nix develop # If you&#39;re using sudo, replace doas with sudo doas pgadmin4 You'll need elevated privileges since pgadmin needs write permissions in /var/lib/pgadmin . Doing this for the first time will ask for credentials you want to set, it doesn't really matter since it's just for a dev env, just remember it. You should see something like this: sekun@nixos ~/P/gnawex (main) [1]&gt; doas pgadmin4 doas (sekun@nixos) password: NOTE: Configuring authentication for SERVER mode. Enter the email address and password to use for the initial pgAdmin user account: Email address: postgres@example.com Password: Retype password: Starting pgAdmin 4. Please navigate to http://127.0.0.1:5050 in your browser. 2022-03-01 12:12:37,060: WARNING werkzeug: WebSocket transport not available. Install eventlet or gevent and gevent-websocket for improved performance. * Serving Flask app &#39;pgadmin&#39; (lazy loading) * Environment: production WARNING: This is a development server. Do not use it in a production deployment. Use a production WSGI server instead. * Debug mode: off pgadmin should be available in the URL specified above. In my case, it's http://127.0.0.1:5050.","ref":"setup.html#database","title":"Setup - Database","type":"extras"},{"doc":"In the future, I'm going to rely on Makefile instead of fish scripts since it's tedious to manually load the script into the environment. For these scripts to work, you need psql and postgresql setup. psql comes with postgresql .","ref":"available-scripts.html","title":"Available scripts","type":"extras"},{"doc":"Takes all migration files from migrations/ , and loads it in gnawex_db . # This is pretty much it migrate","ref":"available-scripts.html#migrate","title":"Available scripts - migrate","type":"extras"},{"doc":"seed-db expects 1 argument, which can either be dev , or prod . If you're using this for a dev environment, use dev , otherwise if it's for production then use prod . You know how it goes. The seed files are in seeds/ as .csv with headers, and delimited with , . Here are the files those two look for: seed-db dev seeds/users.csv : Dummy users seeds/items.csv : Tradable MouseHunt items seeds/listings.csv : Dummy listings seed-db prod : TODO","ref":"available-scripts.html#seed-db","title":"Available scripts - seed-db","type":"extras"},{"doc":"Drops gnawex_db .","ref":"available-scripts.html#drop-db","title":"Available scripts - drop-db","type":"extras"},{"doc":"This is a convenience function that resets gnawex_db . drop-db createdb gnawex_db --user postgres migrate seed-db dev # Or you could just run this: reset-db","ref":"available-scripts.html#reset-db","title":"Available scripts - reset-db","type":"extras"},{"doc":"Roles are used to determine who can perform what operation in the database. There are two kinds of roles, but they can be related in some way. The difference is on where it exists.","ref":"roles.html","title":"Roles","type":"extras"},{"doc":"These ones are defined with CREATE TYPE ROLE as ENUM (...) . These are the roles you'll see in GNAWEX. So far, none of these roles were worked on since the DB is still actively worked on. Although, here is what I have in mind: admin : The system administrator Can appoint moderators Maintain items Privileges like banning/unbanning users and mods mod : Moderates the platform user : Traders, basically Anything the user can do, the mod and admin can as well. So all roles here are able to create listings.","ref":"roles.html#user-roles","title":"Roles - User roles","type":"extras"},{"doc":"Database roles exist because I'm deferring authorization to Postgres. You can get the list of roles in the DB by running this in psql : \\du ; and this would show something like this: gnawex_db =&gt; \\ du List of roles Role name | Attributes | Member of -- -- -- -- -- -- -- -- - + -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- + -- -- -- -- -- -- -- anon | Cannot login | { } authenticator | No inheritance | { anon } gnawex_merchant | Cannot login , Bypass RLS | { anon } postgres | Superuser , Create role , Create DB , Replication , Bypass RLS | { } sekun | | { valid_user } valid_user | Cannot login | { } Here's a summary of what each of them does: anon : Public role. Can view listings, and items. authenticator : Role that is used to login. This role is still being worked on to really flesh out what it's supposed to do. gnawex_merchant : This isn't meant for anyone, rather this role is used to facilitate the exchanging of two listings. Since normal users cannot update their own listings, much more other people's listings, a special role needs to exist for that. You can think of it as GNAWEX updating the listings in your behalf when your listing gets matched with another. postgres : The highest role achievable. Can do anything. sekun : An example of a valid_user . You can ignore this since I just use this for testing. valid_user : This role is granted to users that are: 1) verified, and 2) not banned.","ref":"roles.html#database-roles","title":"Roles - Database roles","type":"extras"}]