# Expense Manager

An expense manager in Elm.

## What to expect?
The Expense Manager in Elm is an experiment, to try out a few things differently.
After a year and change of almost daily Elm development and 18000+ (single dev)
SPA in production, a few annoyances have accumulated:

  * tagging of things, redundant things, every input field, every request etc.
  * JSON decoding/encoding - in most cases copy/paste boilarplate, it really
    should be a language feature e.g. Json.Decode.decoder : {..} -> Decoder {..}

There are two sub-project that Expense Manager uses to try address the above issues:
  * elm-meld - https://github.com/s6o/elm-meld
  * elm-drec - https://github.com/s6o/elm-drec

## Progress

### Done
  * Basic Material (elm-mdl) interface with login/logout
  * Currency management
  * Account management

### TODO
  * Category managment
  * Transactions
  * Trash
  * Statistics
  * Management groups (family accounts)

## Requirements
  * backend - PostgreSQL 9.x
  * backend - PostgREST 0.4.3.x https://github.com/begriffs/postgrest
  * frontend - nginx
  * elm-dev - yarn

## Development Setup
Pre-requisites: PostgreSQL installed and running. PostgREST executable in path.
```
    cd elm_expense_manager/backend
    ./setup-db.sh
    ./run-sql.sh -local elm_expense_manager ./schema/demo-data.sql
    postgrest ./postgrest.conf
```

### Nginx Setup
  * frontend - http://localhost:3330
  * backend - http://localhost:3333

```
.
.
.
http {
    .
    .
    .
    
    index   index.html index.htm;

    # general proxy settings
    proxy_buffering    off;
    proxy_set_header   X-Real-IP $remote_addr;
    proxy_set_header   X-Scheme $scheme;
    proxy_set_header   X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_set_header   X-Forwarded-Proto $scheme;
    proxy_set_header   Host $http_host;
    proxy_http_version 1.1;

    # capture websocket connections
    map $http_upgrade $connection_upgrade {
        default upgrade;
        ''      close;
    }

    upstream postgrest {
        server localhost:3333;
    }

    server {
        listen       3330;
        server_name  localhost;
        root         ~/elm-expense-manager/public;

        location ~* "^/api/(?<api_qry>.+)" {
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection $connection_upgrade;
            proxy_read_timeout 10m;
            proxy_send_timeout 10m;
            proxy_pass http://postgrest/$api_qry$is_args$args;
            proxy_redirect http://postgrest/$api_qry$is_args$args http://localhost:3330/api/$api_qry$is_args$args;
        }

        # redirect server error pages to the static page /50x.html
        error_page   500 502 503 504  /50x.html;
        location = /50x.html {
            root   html;
        }

    }

    include servers/*;
}
```
