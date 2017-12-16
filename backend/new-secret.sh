#!/bin/sh
secret="`openssl rand -base64 32`"
echo "ALTER DATABASE elm_expense_manager SET \"app.jwt_secret\" TO '${secret}';" > ./schema/update-secret.sql

cp ./postgrest-defaults.conf ./postgrest.conf
echo "jwt-secret=\"$secret\"" >> postgrest.conf

