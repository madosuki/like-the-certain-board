#!/usr/bin/env sh

psql -h localhost -U user mysite -c "insert into \"board-list\" (name, \"url-name\", \"default-name\") values('やる夫とLisp', 'lisp', '名無しさん');"

psql -h localhost -U user mysite -c "insert into \"board-terms\" (\"board-id\", body) values(1, '規約');"
