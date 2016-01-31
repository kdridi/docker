#!/usr/bin/env bash

set -e

cd $(dirname $0)
./build.sh

clear
source config
docker run --rm -it --net=host -v /tmp/.X11-unix:/tmp/.X11-unix -v $(pwd)/app:/app $CONFIG_APP_NAME
