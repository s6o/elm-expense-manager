insert into basic_auth.users (email, pass) values ('john.doe@lost.net', 'john2doe'), ('jane.doe@lost.net', 'jane2doe');


insert into api.languages (name) values ('eng'), ('est');


insert into api.translations (lang, ctx, key, txt) values
  ('eng', 'transactions', 'expense', 'Expense')
, ('eng', 'transactions', 'income', 'Income')
, ('eng', 'transactions', 'transfer', 'Transfer')
, ('est', 'transactions', 'expense', 'Väljaminek')
, ('est', 'transactions', 'income', 'Sissetulek')
, ('est', 'transactions', 'transfer', 'Ülekanne')
;


insert into api.settings (settings) values
    ('{"formats":{"date":"YYYY-MM-DD","time":"HH:MM:SS"},"first_iso_weekday":1}'::jsonb);


insert into api.managers (email, name, lang) values
  ('jane.doe@lost.net', 'Jane Doe', 'eng')
, ('john.doe@lost.net', 'John Doe', 'eng');


insert into api.management_groups (name) values ('Lost Family');


insert into api.management_group_members (group_id, mgr_id) values
  (1, 1)
, (1, 2);


insert into api.currency (iso_code, symbol, decimal_separator, thousand_separator)
values
  ('IUK', '~', '.', ',')
;

insert into api.accounts (mgr_id, name, initial_balance, bank_account, bank_name)
values
  (1, 'Jane''s account', '42000', 'ISL0912321092838481', 'Mount Banco Islando')
, (2, 'John''s account', '42000', 'ISL1204820390292221', 'Mount Banco Islando')
;


insert into api.payment_types (mgr_id, name) values
  (1, 'Debit card')
, (2, 'Debit card')
;


insert into api.categories (mgr_id, name, parent_path) values
  (1, 'food', '/')
, (1, 'meat', '/food')
, (1, 'vegetables', '/food')
, (1, 'children', '/')
, (1, 'clothes', '/children')
, (1, 'kindergarden', '/children')
, (1, 'activities', '/children')
, (1, 'salary', '/')
, (2, 'food', '/')
, (2, 'meat', '/food')
, (2, 'vegetables', '/food')
, (2, 'children', '/')
, (2, 'clothes', '/children')
, (2, 'kindergarden', '/children')
, (2, 'activities', '/children')
, (2, 'salary', '/')
;


insert into api.transactions (name) values
  ('expense')
, ('income')
, ('transfer')
;
