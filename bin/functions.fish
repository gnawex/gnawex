#!/usr/local/bin/fish

function migrate --description "Runs all scripts in the migrations folder"
  echo "===========> Starting migrations"
  for file in migrations/*.sql
    echo "-----------> Migrating $file..."

    # TODO: Check if it succeeds or not
    psql gnawex_db \
      --user postgres \
      --file $file

    echo "-----------> Done"
  end
end

function seed-db --description "TODO: Seed gnawex_db"
  switch $argv
  case dev
    echo "===========> Seeding dev db"

    echo "-----------> users from seeds/users.csv"
    psql \
      --user=postgres \
      --dbname=gnawex_db \
      --command "\copy users FROM 'seeds/users.csv' delimiter ',' csv header"
    echo "-----------> Done"

    echo "-----------> items from seeds/items.csv"
    psql \
    --user postgres \
    --dbname=gnawex_db \
    --command "\copy items FROM 'seeds/items.csv' delimiter ',' csv header"
    echo "-----------> Done"

    echo "-----------> listings from seeds/listings.csv"
    psql \
      --user postgres \
      --dbname=gnawex_db \
      --command "\copy listings FROM 'seeds/listings.csv' delimiter ',' csv header" 
    echo "-----------> Done"

  case prod
    echo "===========> Seeding prod db"
    echo "Oh yeah, haven't implemented this yet. Seeding nothing!"

  case '*'
    echo Unknown argument. Available args: dev, prod

  end
end

function drop-db --description "Drops gnawex_db"
  echo "===========> Dropping gnawex_db..."
  dropdb gnawex_db --user postgres
end

function create-migration --description "Creates a migration file that's timestamped"
  if test "$argv" = ""
    echo "I need a migration file name. e.g create-migration create_users"
  else
    echo "Creating migrations/"(date +%4Y%m%d%H%M%S)"_$argv.sql"
    touch "migrations/"(date +%4Y%m%d%H%M%S)"_$argv.sql"
  end
end

function reset-db --description "Resets gnawex_db"
  drop-db
  createdb gnawex_db --user postgres
  migrate
  seed-db dev
end
