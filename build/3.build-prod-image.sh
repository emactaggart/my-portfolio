#!/usr/bin/env bash


if [ ! -d "$APPS_MY_PORTFOLIO_ROOT" ]; then
    echo 'The env var $APPS_MY_PORTFOLIO_ROOT does not appear to be set' && exit 1
else
    pushd $APPS_MY_PORTFOLIO_ROOT
fi

ln --symbolic --force /root/my-portfolio/my-portfolio.asd $APPS_MY_PORTFOLIO_ROOT/quicklisp/local-projects/

docker run \
    -v $APPS_MY_PORTFOLIO_ROOT:/root/my-portfolio:Z \
    -v $APPS_MY_PORTFOLIO_ROOT/quicklisp:/root/quicklisp:Z \
    --entrypoint sbcl \
    dev-base \
    --no-userinit --no-sysinit --non-interactive \
    --load /root/quicklisp/setup.lisp \
    --eval "(ql:quickload :my-portfolio)" \
    --eval "(asdf:make :my-portfolio)"

# TODO dir for cached loading/compilation
# /root/.cache/common-lisp/sbcl-2.1.11-linux-x64/root/my-portfolio/src
# pwd

docker build --format docker -f build/Dockerfile.prod -t my-portfolio:alpine .

popd
