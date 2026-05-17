begin;

alter table if exists public."user-table"
  drop column if exists "board-id";

commit;
