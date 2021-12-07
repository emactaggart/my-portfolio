#!/usr/bin/env bash

if [ ! -d "$APPS_MY_PORTFOLIO_ROOT" ]; then
    echo 'The env var $APPS_MY_PORTFOLIO_ROOT does not appear to be set' && exit 1
else
    pushd $APPS_MY_PORTFOLIO_ROOT
fi

docker rmi lisp-base:alpine
docker build . -f build/Dockerfile.lisp-base -v $APPS_MY_PORTFOLIO_ROOT/quicklisp:/root/quicklisp:Z -t lisp-base:alpine
docker build . -f build/Dockerfile.dev-base -t dev-base:alpine

popd
