#!/usr/bin/env bash

if [ ! -d "$APPS_MY_PORTFOLIO_ROOT" ]; then
    echo 'The env var $APPS_MY_PORTFOLIO_ROOT does not appear to be set' && exit 1
else
    pushd $APPS_MY_PORTFOLIO_ROOT
fi

# project-root="/home/evan/Devlepment/my-portfolio"


ln --symbolic --force /root/my-portfolio/my-portfolio.asd $APPS_MY_PORTFOLIO_ROOT/quicklisp/local-projects/

docker run -i --tty --entrypoint sbcl \
    -p 8080:8080 \
    -v ./.dev.taggrc:/root/.taggrc \
    -v $APPS_MY_PORTFOLIO_ROOT/.cache/common-lisp:/root/.cache/common-lisp:Z \
    -v $APPS_MY_PORTFOLIO_ROOT/quicklisp:/root/quicklisp:Z \
    -v $APPS_MY_PORTFOLIO_ROOT:/root/my-portfolio:Z \
    dev-base --eval "(ql:quickload :my-portfolio)" --eval "(control:start-server)" --quit

popd
