#!/usr/bin/env bash

pushd ~/Development/my-portfolio

docker run -p 8080:8080 --entrypoint sh --interactive --tty my-portfolio:alpine

popd
