#!/usr/bin/env bash

cd $(dirname $0)
source ../config

docker_build

${docker_run} --rm -it $CONFIG_PKG_NAME $@