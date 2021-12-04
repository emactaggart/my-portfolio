#!/usr/bin/env bash

pushd ~/Development/my-portfolio

rm --force ~/quicklisp/local-projects/my-portfolio.asd
ln -s /root/my-portfolio/my-portfolio.asd ~/quicklisp/local-projects/

docker run -i --entrypoint sbcl \
    -p 8080:8080 \
    -v ./.dev.taggrc:/root/.taggrc \
    -v ~/Development/my-portfolio:/root/my-portfolio:Z \
    -v ~/quicklisp:/root/quicklisp:Z \
    dev-base --eval "(ql:quickload :my-portfolio)" --eval "(control:start-server)" --quit

popd
