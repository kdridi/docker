#!/usr/bin/env bash

cd $(dirname $0)
ROOT_DIR=$(pwd)

for i in *; do
	cd $ROOT_DIR
	if [[ -d $i ]]; then
		cd $i
		source ../config
		docker_build
	fi
done