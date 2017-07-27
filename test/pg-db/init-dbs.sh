#!/bin/bash
set -e

for db_name in $PG_DBS; do
psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" <<-EOSQL
    CREATE DATABASE ${db_name};
    GRANT ALL PRIVILEGES ON DATABASE ${db_name} TO ${POSTGRES_USER};
EOSQL
done
