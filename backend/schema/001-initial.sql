-- API schema

create schema api;

create table api.languages (
  name text not null check(length(name) = 3) primary key
);

create table api.translations (
  pk_id serial primary key
, lang text not null references api.languages(name) on delete cascade on update cascade
, ctx text not null
, key text not null
, txt text not null
, unique (lang, ctx, key)
);

create table api.settings (
  settings JSONB
);

create table api.managers (
  pk_id serial primary key
, email text not null unique
, name text not null
, lang text not null references api.languages(name) on delete cascade on update cascade
);

create table api.management_groups (
  pk_id serial primary key
, name text not null
);

create table api.management_group_members (
  group_id integer references api.management_groups(pk_id) on delete cascade on update cascade
, mgr_id integer references api.managers(pk_id) on delete cascade on update cascade
, primary key (group_id, mgr_id)
);

create table api.currency (
  iso_code text check(length(iso_code) = 3)
, sub_unit_ratio integer default 100
, symbol text check(length(symbol) >= 1 and length(symbol) <= 3)
, decimal_separator text check(length(decimal_separator) = 1)
, thousand_separator text check(length(thousand_separator) = 1)
, primary key (iso_code)
);

create table api.accounts (
  pk_id serial primary key
, mgr_id integer references api.managers(pk_id) on delete cascade on update cascade
, name text not null
, initial_balance bigint
, bank_account text
, bank_name text
);

create table api.payment_types (
  pk_id serial primary key
, mgr_id integer references api.managers(pk_id) on delete cascade on update cascade
, name text not null
);

create table api.categories (
  pk_id serial primary key
, mgr_id integer references api.managers(pk_id) on delete cascade on update cascade
, name text not null check(lower(name) = name)
, parent_path text not null default '/'
);

create table api.transactions (
  name text primary key
);

create table api.account_transactions (
  pk_id serial primary key
, mgr_id integer references api.managers(pk_id) on delete cascade on update cascade
, ts timestamp with time zone not null
, transaction text references api.transactions(name) on delete cascade on update cascade
, amount bigint default 0
, title text not null
, comments text
, pt_id integer references api.payment_types(pk_id) on delete set null on update cascade
, cat_id integer references api.categories(pk_id) on delete set null on update cascade
, source_account integer references api.accounts(pk_id) on delete cascade on update cascade
, target_account integer references api.accounts(pk_id) on delete cascade on update cascade
);

-- Trash / Recycle Bin

create table api.trash (
  pk_id serial primary key
, mgr_id integer references api.managers(pk_id) on delete cascade on update cascade
, ts timestamp with time zone not null default now()
, origin_schema text not null
, origin_table text not null
, record jsonb not null
);

-- when deleting from api.trash use this to restore the original record
create or replace function restore_from_trash() returns trigger as $$
begin
    execute 'insert into ' || OLD.origin_schema || '.' || OLD.origin_table || ' '
        || 'select * from json_populate_record(null::' || OLD.origin_schema || '.'
        || quote_ident(OLD.origin_table) || ', ' || quote_literal(OLD.record) || ')';

    return OLD;
end;
$$ language plpgsql;

create trigger restore_from_trash before delete on api.trash
    for each row execute procedure restore_from_trash();

-- the table for which its deployed needs to have a mgr_id column
-- referencing api.managers(pk_id) and table's primary key should not re-occur;
-- a looooooong sequence will be probably fine given the scope of the application
create or replace function move_to_trash() returns trigger as $$
begin
    insert into api.trash (mgr_id, origin_schema, origin_table, record)
        values (OLD.mgr_id, TG_TABLE_SCHEMA, TG_TABLE_NAME, row_to_json(OLD.*));

    return OLD;
end;
$$ language plpgsql;

create trigger move_to_trash before delete on api.accounts
    for each row execute procedure move_to_trash();

create trigger move_to_trash before delete on api.payment_types
    for each row execute procedure move_to_trash();

create trigger move_to_trash before delete on api.categories
    for each row execute procedure move_to_trash();

create trigger move_to_trash before delete on api.account_transactions
    for each row execute procedure move_to_trash();


-- AUTH roles and access

\i schema/pgjwt.sql

create schema basic_auth;

create table basic_auth.users (
  email text primary key check ( email ~* '^.+@.+\..+$' )
, pass text not null check (length(pass) < 512)
, role name not null default 'webuser' check (length(role) < 512)
);

create or replace function
basic_auth.check_role_exists() returns trigger
    language plpgsql
    as $$
begin
    if not exists (select 1 from pg_roles as r where r.rolname = new.role) then
        raise foreign_key_violation using message =
            'unknown database role: ' || new.role;
        return null;
    end if;
    return new;
end
$$;

drop trigger if exists ensure_user_role_exists on basic_auth.users;
create constraint trigger ensure_user_role_exists
    after insert or update on basic_auth.users
    for each row
    execute procedure basic_auth.check_role_exists();

create or replace function
basic_auth.encrypt_pass() returns trigger
    language plpgsql
    as $$
begin
    if tg_op = 'INSERT' or new.pass <> old.pass then
        new.pass = crypt(new.pass, gen_salt('bf'));
    end if;
    return new;
end
$$;

drop trigger if exists encrypt_pass on basic_auth.users;
create trigger encrypt_pass
    before insert or update on basic_auth.users
    for each row
    execute procedure basic_auth.encrypt_pass();


create type basic_auth.jwt_token as (
  token text
);

create or replace function
basic_auth.user_role(email text, pass text) returns name
    language plpgsql
    as $$
begin
    return (
        select role from basic_auth.users
            where users.email = user_role.email and users.pass = crypt(user_role.pass, users.pass)
    );
end;
$$;

-- login and auth check

create or replace function
api.login(email text, pass text) returns basic_auth.jwt_token
    language plpgsql
    as $$
declare
    _role name;
    result basic_auth.jwt_token;
begin
    -- check email and password
    select basic_auth.user_role(email, pass) into _role;
    if _role is null then
        raise invalid_password using message = 'invalid user or password';
    end if;

    select sign(row_to_json(r), current_setting('app.jwt_secret')) as token
    from (
        select _role as role,
            (select am.pk_id from api.managers am where am.email = login.email) as uid,
            login.email as email,
            extract(epoch from now())::integer + 60*60 as exp
    ) r
    into result;

    return result;
end;
$$;

-- permissions

drop role if exists authenticator;
drop role if exists webuser;
drop role if exists anon;

create role anon;
create role webuser nologin;
create role authenticator noinherit;

grant webuser to authenticator;

grant usage on schema api, basic_auth to anon;
grant select on table pg_authid, basic_auth.users to anon;
grant select on table api.managers to anon;
grant execute on function api.login(text, text) to anon;

-- As of PostgreSQL 9.x
grant all on schema api to webuser;
grant all privileges on all tables in schema api to webuser;
grant all privileges on all sequences in schema api to webuser;
grant all privileges on all functions in schema api to webuser;
