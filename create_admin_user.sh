#!/bin/sh

# this script is still WIP

user_name=${1}
cap_text=${2}

postgre_host=localhost
postgre_dbname=${3}
postgre_username=${4}

user_password=${5}
salt=${6}

board_id=${7}

hash=`echo -n ${user_password} | openssl dgst -sha256 -hmac ${salt} | awk '{print $2}'`

# echo ${hash}

# mysql --ssl -h ${mysql_host} -u ${mysql_username} -p -e "insert into ${mysql_dbname}.\`user-table\` (\`board-id\`, \`user-name\`, hash, \`create-date\`, \`latest-date\`, \`is-admin\`, \`cap-text\`) values(${board_id}, '${user_name}', '${hash}', '1970-01-01', '1970-01-01', 1, '${cap_text}')"


psql -h "${postgre_host}" -p 5432 -U "${postgre_username}" "${postgre_dbname}" -c "insert into \"user-table\" (\"board-id\", \"user-name\", hash, \"create-date\", \"latest-date\", \"is-admin\", \"cap-text\", salt) values(${board_id}, '${user_name}', '${hash}', '1970-01-01', '1970-01-01', true, '${cap_text}', '${salt}')"
