#!/bin/sh

# this script is still WIP

user_name="user"
cap_text="★管理人"

mysql_host="127.0.0.1"
mysql_dbname="mysite"
mysql_username="sample"

user_password=${1}
mysql_password=${2}

mysql -h ${mysql_host} -u ${user_name} -p ${mysql_password} <<EOF
insert into ${mysql_dbname}.`user-table` (`board-id`, `user-name` hash, `create-date`, `latest-date`, `is-admin`, `cap-test`) values(0, ${user_name}, "", "1970-01-01", "1970-01-01", 1, ${cap_text})
EOF
