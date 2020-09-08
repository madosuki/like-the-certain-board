#!/bin/sh

cd /root/.roswell/local-projects/like-the-certain-board/ && APP_ENV=development clackup --server :fcgi --address 0.0.0.0 --port 8888 ./app.lisp
