#!/bin/sh

# this script is still WIP

user_name=${1}
cap_text=${2}

mysql_host="127.0.0.1"
mysql_dbname="mysite"
mysql_username="lain"

user_password=${3}
salt=${4}

target=${salt}${user_password}
hash=`echo -n ${target} | sha256sum | awk '{ print $1 }'`

mysql -h ${mysql_host} -u ${mysql_username} -p -e "insert into ${mysql_dbname}.\`user-table\` (\`board-id\`, \`user-name\`, hash, \`create-date\`, \`latest-date\`, \`is-admin\`, \`cap-text\`) values(0, '${user_name}', '${hash}', '1970-01-01', '1970-01-01', 1, '${cap_text}')"

