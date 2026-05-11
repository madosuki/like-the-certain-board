begin;

do $$
begin
  if to_regclass('public.kakolog') is not null
     and not exists (
       select 1
       from pg_constraint
       where conrelid = 'public.kakolog'::regclass
         and conname = 'fk_kakolog'
     ) then
    alter table public.kakolog
      add constraint fk_kakolog
      foreign key ("board-id") references public."board-list" (id);
  end if;
end
$$;

do $$
declare
  current_pkey text;
  current_pkey_columns text[];
begin
  if to_regclass('public.threads') is null then
    return;
  end if;

  if exists (select 1 from public.threads where unixtime is null) then
    raise exception 'cannot migrate threads.unixtime to NOT NULL because NULL values exist';
  end if;

  select c.conname, array_agg(a.attname order by k.ordinality)
  into current_pkey, current_pkey_columns
  from pg_constraint c
  join unnest(c.conkey) with ordinality as k(attnum, ordinality) on true
  join pg_attribute a on a.attrelid = c.conrelid and a.attnum = k.attnum
  where c.conrelid = 'public.threads'::regclass
    and c.contype = 'p'
  group by c.conname;

  if current_pkey is not null and current_pkey_columns <> array['id']::text[] then
    execute format('alter table public.threads drop constraint %I', current_pkey);
  end if;
end
$$;

alter table public.threads
  alter column unixtime set not null;

do $$
begin
  if to_regclass('public.threads') is not null
     and not exists (
       select 1
       from pg_constraint
       where conrelid = 'public.threads'::regclass
         and contype = 'p'
     ) then
    alter table public.threads
      add constraint threads_pkey primary key (id);
  end if;
end
$$;

do $$
begin
  if to_regclass('public.threads') is not null
     and not exists (
       select 1
       from pg_constraint
       where conrelid = 'public.threads'::regclass
         and conname = 'uq_threads_unixtime'
     ) then
    alter table public.threads
      add constraint uq_threads_unixtime unique ("board-id", unixtime);
  end if;
end
$$;

do $$
begin
  if to_regclass('public.threads') is not null
     and not exists (
       select 1
       from pg_constraint
       where conrelid = 'public.threads'::regclass
         and conname = 'fk_threads'
     ) then
    alter table public.threads
      add constraint fk_threads
      foreign key ("board-id") references public."board-list" (id);
  end if;
end
$$;

commit;
