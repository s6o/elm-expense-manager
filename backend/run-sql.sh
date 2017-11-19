#!/bin/sh
#
# Execute SQL statements in a single transaction via psql
#
# Usage:
#   ./run-sql.sh {server} {db-name} {sql-script}
#
#   server
#     * -local      - applied psql options: -Upostgres -hlocalhost
#     * -production - applied psql options: -Upostgres -h<configure-your-production-host> 
#
#   dbname - a PostgreSQL database name in the specified server
#
#   script - a SQL or PL/PgSQL script with an absolute or relative path
#

SERVER="${1}"
DB_NAME="${2}"
SQL_SCRIPT="${3}"

###############################################################################

HOST="localhost"
USER="postgres"

###############################################################################

function usage_help() {
cat << USAGEEND

Help

Execute SQL statements in a single transaction via psql

Usage:
  ./run-sql.sh {server} {db-name} {sql-script}

  server
    * -local      - applied psql options: -Upostgres -hlocalhost
    * -production - applied psql options: -Upostgres -h<configure-your-production-host>

  dbname - a PostgreSQL database name in the specified server

  script - a SQL or PL/PgSQL script with an absolute or relative path

USAGEEND
}

###############################################################################

if [ "${#}" -ne 3 ]; then
usage_help
exit 1
fi

case "${SERVER}" in
"-local")
  HOST="localhost"
  ;;
"-production")
  HOST="<configure-your-production-host>"
  ;;
*)
  echo
  echo "Incorrect first argument value ${SERVER}. Valid options: -local or -production"
  echo
  usage_help
  ;;
esac

###############################################################################

transaction_file="${HOME}/.pg_transaction_wrapper"
echo "BEGIN;" > "${transaction_file}"
echo "\i ${SQL_SCRIPT}" >> "${transaction_file}"
echo "COMMIT;" >> "${transaction_file}"

PSQL="psql"
if [ "`uname`" == "Darwin" ]; then
  PSQL="/Applications/Postgres.app/Contents/Versions/latest/bin/psql"
fi

$PSQL \
  -U ${USER} \
  -h ${HOST} \
  -f ${transaction_file} \
  --no-psqlrc \
  --echo-all \
  --set AUTOCOMMIT=off \
  --set ON_ERROR_STOP=on \
  ${DB_NAME}

psql_result="$?"
let psql_result=psql_result+0

rm "${transaction_file}" &> /dev/null

if [ $psql_result -eq 0 ]; then
  exit 0
else
  exit 70
fi
