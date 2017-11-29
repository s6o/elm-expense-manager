#!/bin/sh
#
createdb -E UTF8 elm_expense_manager
./new-secret.sh
./run-sql.sh -local elm_expense_manager ./schema/update-secret.sql
./run-sql.sh -local elm_expense_manager ./schema/pgjwt.sql
./run-sql.sh -local elm_expense_manager ./schema/001-initial.sql

