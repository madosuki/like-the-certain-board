#!/bin/sh

user_name="admin"
password="a97a564e9e2460dbd14fbd7d9067614d9e1205f626e722de475c93204926e0999a0279c716a7ff624160566e36f0fb2354c9dcace18af90b2d863439cf1406a5"
cap_text="★管理人"
url="http://localhost:8080/yarufre/api/user"

curl -X POST -d "user_name="${user_name}"&password="${password}"&is_admin=true&cap_text="${cap_text} ${url}
