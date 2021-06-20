#!/bin/sh

export BOARD_SETTINGS_PATH=/roswell/settings/
export KAKOLOG_HTML_DIR_PATH=/roswell/kakolog/html/
export KAKOLOG_DAT_DIR_PATH=/roswell/kakolog/dat/
export DAT_DIR_PATH=/roswell/dat/
export APP_ENV=production

ros update git lack
ros update git ningle
ros update git clack
ros update git generate-like-certain-board-strings
ros update git cl-markup
ros update git quri
ros update git cl-crypt

ros update quicklisp

cd /root/.roswell/local-projects/like-the-certain-board/ && clackup --server :woo --address 0.0.0.0 --port 8888 ./app.lisp
# cd /root/.roswell/local-projects/like-the-certain-board/ && clackup --server :fcgi --address 0.0.0.0 --port 8888 ./app.lisp
