#!/usr/bin/env bash

set -euo pipefail

usage() {
  echo "Usage: $0 user_name mail db_name db_user password [db_host] [db_port]" >&2
}

if [ "$#" -lt 5 ] || [ "$#" -gt 7 ]; then
  usage
  exit 1
fi

user_name=${1}
mail=${2}
postgre_dbname=${3}
postgre_username=${4}
user_password=${5}
postgre_host=${6:-localhost}
postgre_port=${7:-5432}

if ! command -v psql >/dev/null 2>&1; then
  echo "psql command is required." >&2
  exit 1
fi

if ! command -v openssl >/dev/null 2>&1; then
  echo "openssl command is required." >&2
  exit 1
fi

if ! command -v argon2 >/dev/null 2>&1; then
  echo "argon2 command is required." >&2
  exit 1
fi

salt=$(openssl rand -hex 16)
password_hash=$(printf "%s" "${user_password}" | argon2 "${salt}" -id -t 3 -m 16 -p 1 -l 32 -e | awk '{print $NF}')

psql \
  -h "${postgre_host}" \
  -p "${postgre_port}" \
  -U "${postgre_username}" \
  -d "${postgre_dbname}" \
  -v ON_ERROR_STOP=1 \
  -v user_name="${user_name}" \
  -v mail="${mail}" \
  -v password_hash="${password_hash}" \
  <<'SQL'
begin;

insert into public.roles (name)
values ('admin'),
       ('moderator')
on conflict (name) do nothing;

insert into public."user-table" ("user-name", mail, "password-hash")
values (:'user_name', :'mail', :'password_hash')
on conflict ("user-name") do update
set mail = excluded.mail,
    "password-hash" = excluded."password-hash";

insert into public."user-roles" ("user-id", "role-id")
select created_user.id, admin_role.id
from public."user-table" created_user
cross join public.roles admin_role
where created_user."user-name" = :'user_name'
  and admin_role.name = 'admin'
on conflict ("user-id", "role-id") do nothing;

commit;
SQL
