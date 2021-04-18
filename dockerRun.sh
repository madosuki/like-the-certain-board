#!/bin/sh

export BOARD_SETTINGS_PATH=/roswell/settings/
export KAKOLOG_HTML_DIR_PATH=/roswell/html/
export DAT_DIR_PATH=/roswell/dat/
export APP_ENV=production

cd /root/.roswell/local-projects/like-the-certain-board/ && clackup --server :fcgi --address 0.0.0.0 --port 8888 ./app.lisp
