#!/bin/sh

docker-compose stop && yes | docker-compose rm && docker-compose build && docker-compose up -d
