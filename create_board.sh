#!/bin/sh

psql -h localhost -U "$1" "$2" -c "insert into \"board-list\" (name, \"url-name\", \"default-name\") values('やる夫とLisp', 'lisp', '名無しさん');"
