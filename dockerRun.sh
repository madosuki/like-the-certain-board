#!/bin/sh

BOARD_DB_PATH=/root/.roswell/local-projects/like-the-certain-board/

cd /root/.roswell/local-projects/like-the-certain-board/ && APP_ENV=production clackup --server :fcgi --address 0.0.0.0 --port 8888 ./app.lisp
