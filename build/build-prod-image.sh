#!/usr/bin/env bash

pushd ~/Development/my-portfolio

rm --force ~/quicklisp/local-projects/my-portfolio.asd
ln -s /root/my-portfolio/my-portfolio.asd ~/quicklisp/local-projects/

docker run \
    -v ~/Development/my-portfolio:/root/my-portfolio:Z \
    -v ~/quicklisp:/root/quicklisp:Z \
    --entrypoint sbcl \
    dev-base \
    --no-userinit --no-sysinit --non-interactive \
    --load /root/quicklisp/setup.lisp \
    --eval "(ql:quickload :my-portfolio)" \
    --eval "(asdf:make :my-portfolio)"

pwd

docker build --format docker -f build/Dockerfile.prod -t my-portfolio:alpine .

popd
