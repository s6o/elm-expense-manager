-- API schema

create schema api;

create table api.languages (
  name text not null check(length(name) = 3) primary key
);

create table api.translations (
  tid serial primary key
, lang text not null references api.languages(name) on delete cascade on update cascade
, ctx text not null
, key text not null
, txt text not null
, unique (lang, ctx)
, unique (lang, key)
);

create table api.managers (
  mid serial primary key
, email text not null unique
, name text not null
, lang text not null references api.languages(name) on delete cascade on update cascade
);

create table api.management_groups (
  mgid serial primary key
, name text not null
);

create table api.management_group_members (
  mgp_id integer references api.management_groups(mgid) on delete cascade on update cascade
, mgr_id integer references api.managers(mid) on delete cascade on update cascade
, primary key (mgp_id, mgr_id)
);

create table api.accounts (
  aid serial primary key
, mgr_id integer references api.managers(mid) on delete cascade on update cascade
, name text not null
, currency_label text check(length(currency_label) = 3)
, currency_sign text check(length(currency_sign) = 1)
, initial_balance numeric(10,2)
, current_balance numeric(10,2)
);

create table api.payment_types (
  pid serial primary key
, mgr_id integer references api.managers(mid) on delete cascade on update cascade
, name text not null
);

create table api.categories (
  cid serial primary key
, mgr_id integer references api.managers(mid) on delete cascade on update cascade
, name text not null
, parent_path text not null default '/'
);

create table api.transactions (
  name text primary key
);

create table api.account_transactions (
  atid serial primary key
, mgr_id integer references api.managers(mid) on delete cascade on update cascade
, ts timestamp with time zone not null 
, transaction text references api.transactions(name) on delete cascade on update cascade
, amount numeric(10,2) default 0.00
, title text not null
, comments text
, pt_id integer references api.payment_types(pid) on delete cascade on update cascade
, cat_id integer references api.categories(cid) on delete cascade on update cascade
, source_account integer references api.accounts(aid) on delete cascade on update cascade
, target_account integer references api.accounts(aid) on delete cascade on update cascade
);


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
        select _role as role, login.email as email,
            extract(epoch from now())::integer + 60*60 as exp
    ) r
    into result;

    return result;
end;
$$;

create or replace function api.check_auth() returns void
    language plpgsql
    as $$
declare
    _email text;
    _role name;
    _row record;
    _auth text;
begin
    _auth = current_setting('request.header.X-Manager-Auth');
    if _auth = 'elm-expense-manager' then
        -- proceed to authentication
        return;
    else
        _email = current_setting('request.jwt.claim.email');
        _role = current_setting('request.jwt.claim.role');

        select into _row * from basic_auth.users
            where email = _email and role = _role limit 1;

        if not found then
            raise insufficient_privilege
                using hint = 'Authenticate first.';
        end if;
    end if;
end
$$;

-- permissions

drop role if exists authenticator;
drop role if exists webuser;
drop role if exists anon;

create role anon;
create role webuser nologin;
create role authenticator noinherit;

grant anon to authenticator;
grant webuser to authenticator;

grant usage on schema api, basic_auth to anon;
grant select on table pg_authid, basic_auth.users to anon;
grant execute on function api.login(text, text) to anon;
grant execute on function api.check_auth() to anon;

grant all on schema api to webuser;
