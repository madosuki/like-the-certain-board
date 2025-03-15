#!/bin/sh

APP_ENV=development clackup --server :woo --port 8080 ./app.lisp
