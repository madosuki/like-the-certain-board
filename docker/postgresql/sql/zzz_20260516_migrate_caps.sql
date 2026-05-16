begin;

create table if not exists public.caps (
id serial primary key,
"user-id" integer not null,
"cap-text" text not null,
"created-at" timestamp not null default current_timestamp,
"updated-at" timestamp not null default current_timestamp
);

do $$
begin
  if not exists (
    select 1
    from pg_constraint
    where conrelid = 'public.caps'::regclass
      and conname = 'uq_user_id'
  ) then
    alter table public.caps
      add constraint uq_user_id unique ("user-id");
  end if;
end
$$;

do $$
begin
  if not exists (
    select 1
    from pg_constraint
    where conrelid = 'public.caps'::regclass
      and conname = 'fk_caps_user_id'
  ) then
    alter table public.caps
      add constraint fk_caps_user_id
      foreign key ("user-id") references public."user-table" (id)
      on delete cascade on update cascade;
  end if;
end
$$;

do $$
begin
  if exists (
    select 1
    from information_schema.columns
    where table_schema = 'public'
      and table_name = 'user-table'
      and column_name = 'cap-text'
  ) then
    insert into public.caps ("user-id", "cap-text", "created-at", "updated-at")
    select id, "cap-text", current_timestamp, current_timestamp
    from public."user-table"
    where "cap-text" is not null
    on conflict ("user-id") do update
      set "cap-text" = excluded."cap-text",
          "updated-at" = current_timestamp;

    alter table public."user-table"
      drop column "cap-text";
  end if;
end
$$;

commit;
