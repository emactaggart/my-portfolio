#!/usr/bin/env bash

if [ ! -d "$APPS_MY_PORTFOLIO_ROOT" ]; then
    echo 'The env var $APPS_MY_PORTFOLIO_ROOT does not appear to be set' && exit 1
else
    pushd $APPS_MY_PORTFOLIO_ROOT
fi

# To run locally
# docker run -p 8080:80 --entrypoint sh --interactive --tty my-portfolio:alpine

docker run -p 8080:80 --entrypoint sh --interactive --tty my-portfolio:alpine -c /root/my-portfolio.exe

popd
