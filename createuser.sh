#!/bin/sh

# this script is sample

user_name="admin"
password="pass"
cap_text="★管理人"
url="http://localhost:8080/board-name/api/user"

curl -X POST -d "user_name="${user_name}"&password="${password}"&is_admin=true&cap_text="${cap_text} ${url}
