#!/usr/bin/env bash

set -e

cd $(dirname $0)

source config
docker build -t $CONFIG_APP_NAME .
