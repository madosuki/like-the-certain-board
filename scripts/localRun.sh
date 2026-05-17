#!/usr/bin/env bash

export BOARD_SETTINGS_PATH=../like_the_certain_board_dirs/settings/
export KAKOLOG_HTML_DIR_PATH=../like_the_certain_board_dirs/kakolog/html/
export KAKOLOG_DAT_DIR_PATH=../like_the_certain_board_dirs/kakolog/dat/
export DAT_DIR_PATH=../like_the_certain_board_dirs/dat/
export APP_ENV=production

# yes | ros update quicklisp

# ros update git quri
# ros update git lack
# ros update git ningle
# ros update git clack
# ros update git generate-like-certain-board-strings
# ros update git cl-markup
# ros update git cl-crypt

APP_ENV=production clackup --server :woo --address 0.0.0.0 --port 8080 ./app.lisp

