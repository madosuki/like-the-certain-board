begin;

alter table if exists public."user-table"
  alter column "create-date" set default current_timestamp,
  alter column "latest-date" set default current_timestamp;

update public."user-table"
set "latest-date" = current_timestamp
where "latest-date" is null;

alter table if exists public."user-table"
  alter column "latest-date" set not null;

commit;
