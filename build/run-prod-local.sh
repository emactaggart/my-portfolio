#!/usr/bin/env bash

pushd ~/Development/my-portfolio

# docker run -p 80:80 --entrypoint sh --interactive --tty my-portfolio:alpine
docker run -p 80:80 --entrypoint sh --interactive --tty my-portfolio:alpine -c /root/my-portfolio.exe

popd
