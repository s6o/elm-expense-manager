#!/bin/sh
secret="`openssl rand -base64 32`"
echo "ALTER DATABASE elm_expense_manager SET \"app.jwt_secret\" TO '${secret}';" > update-secret.sql

