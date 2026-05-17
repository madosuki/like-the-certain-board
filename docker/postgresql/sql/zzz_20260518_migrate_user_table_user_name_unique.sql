begin;

do $$
begin
  if not exists (
    select 1
    from pg_constraint
    where conrelid = 'public."user-table"'::regclass
      and conname = 'uq_user_table_user_name'
  ) then
    alter table public."user-table"
      add constraint uq_user_table_user_name unique ("user-name");
  end if;
end
$$;

commit;
