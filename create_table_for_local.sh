#!/usr/bin/env bash

username=${1}
dbname=${2}

sql_base="./docker/postgresql/sql/"
sqls=($(ls ${sql_base}))

for i in "${sqls[@]}"
do
    path="${sql_base}""${i}"
    psql -d "${dbname}" -U "${username}" -v ON_ERROR_STOP=1 -f "${path}"
done
