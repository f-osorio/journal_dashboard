#!/bin/bash
set -eux

THISDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

#Demo PostgreSQL Database initialisation
psql postgres -c "CREATE USER testuser PASSWORD 'pass'"
#The -O flag below sets the user: createdb -O DBUSER DBNAME
createdb -O testuser metrics

psql postgres -c "ALTER ROLE testuser WITH SUPERUSER"

psql -d metrics -U testuser -f $THISDIR/seed.sql
