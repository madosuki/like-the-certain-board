#!/usr/bin/env bash

# this script is still WIP

user_name=${1}
cap_text=${2}

postgre_host=localhost
postgre_dbname=${3}
postgre_username=${4}

user_password=${5}
salt=$(openssl rand -hex 16)

board_id=${6}

password_hash=`echo -n ${user_password} | argon2 ${salt} -id -t 3 -m 16 -p 1 -l 32 -e  | awk '{print $2}'`

psql -h "${postgre_host}" -p 5432 -U "${postgre_username}" "${postgre_dbname}" -c "insert into \"user-table\" (\"board-id\", \"user-name\", \"password-hash\", \"create-date\", \"latest-date\", \"is-admin\", \"cap-text\") values(${board_id}, '${user_name}', '${password_hash}', '1970-01-01', '1970-01-01', true, '${cap_text}')"
