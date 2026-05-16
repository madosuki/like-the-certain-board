begin;

create table if not exists public.roles (
id serial primary key,
name varchar(50) not null
);

do $$
begin
  if not exists (
    select 1
    from information_schema.columns
    where table_schema = 'public'
      and table_name = 'user-table'
      and column_name = 'mail'
  ) then
    alter table public."user-table"
      add column mail varchar(255) not null default '';

    alter table public."user-table"
      alter column mail drop default;
  else
    update public."user-table"
    set mail = ''
    where mail is null;

    alter table public."user-table"
      alter column mail set not null;
  end if;
end
$$;

do $$
begin
  if not exists (
    select 1
    from pg_constraint
    where conrelid = 'public.roles'::regclass
      and conname = 'uq_role_name'
  ) then
    alter table public.roles
      add constraint uq_role_name unique (name);
  end if;
end
$$;

insert into public.roles (name)
values ('admin')
on conflict (name) do nothing;

create table if not exists public."user-roles" (
"user-id" integer not null,
"role-id" integer not null
);

do $$
begin
  if not exists (
    select 1
    from pg_constraint
    where conrelid = 'public."user-roles"'::regclass
      and conname = 'user_roles_pkey'
  ) then
    alter table public."user-roles"
      add constraint user_roles_pkey primary key ("user-id", "role-id");
  end if;
end
$$;

do $$
begin
  if not exists (
    select 1
    from pg_constraint
    where conrelid = 'public."user-roles"'::regclass
      and conname = 'fk_user_roles_user'
  ) then
    alter table public."user-roles"
      add constraint fk_user_roles_user
      foreign key ("user-id") references public."user-table" (id)
      on delete cascade on update cascade;
  end if;
end
$$;

do $$
begin
  if not exists (
    select 1
    from pg_constraint
    where conrelid = 'public."user-roles"'::regclass
      and conname = 'fk_user_roles_role'
  ) then
    alter table public."user-roles"
      add constraint fk_user_roles_role
      foreign key ("role-id") references public.roles (id)
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
      and column_name = 'is-admin'
  ) then
    insert into public."user-roles" ("user-id", "role-id")
    select u.id, r.id
    from public."user-table" u
    cross join public.roles r
    where u."is-admin" = true
      and r.name = 'admin'
    on conflict ("user-id", "role-id") do nothing;

    alter table public."user-table"
      drop column "is-admin";
  end if;
end
$$;

commit;
