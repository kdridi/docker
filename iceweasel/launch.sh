#!/bin/bash

cd $(dirname $0)
./build.sh

source config
docker run --rm -it --net=host -v /tmp/.X11-unix:/tmp/.X11-unix $CONFIG_APP_NAME $1