#!/bin/bash

cd $(dirname $0)
source config

docker build -t $CONFIG_APP_NAME .