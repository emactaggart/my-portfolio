#!/usr/bin/env bash

pushd ~/Development/my-portfolio

docker rmi lisp-base:alpine
docker build . -f build/Dockerfile.lisp-base -t lisp-base:alpine
docker build . -f build/Dockerfile.dev-base -t dev-base:alpine

popd
