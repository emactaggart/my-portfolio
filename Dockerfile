FROM daewok/sbcl:alpine

# COPY ./ /usr/src/app/

## TODO Securer install of quicklisp (needs to be updated for alpine (no apt-get))
# RUN set -x \
#   && apt-get update && apt-get install -y --no-install-recommends curl && rm -rf /var/lib/apt \
#   && curl "https://beta.quicklisp.org/release-key.txt" > /tmp/quicklisp-release-key.txt \
#   && curl "https://beta.quicklisp.org/quicklisp.lisp" > /tmp/quicklisp.lisp \
#   && curl "https://beta.quicklisp.org/quicklisp.lisp.asc" > /tmp/quicklisp.lisp.asc \
#   && export GNUPGHOME="$(mktemp -d)" \
#   && gpg --batch --import /tmp/quicklisp-release-key.txt \
#   && gpg --batch --verify /tmp/quicklisp.lisp.asc /tmp/quicklisp.lisp \
#   && sync \
#   && sleep 2 \
#   && rm -rf "$GNUPGHOME" /tmp/quicklisp.lisp.asc \
#   && export HOME=/home/lisp \
#   && sbcl --no-sysinit --no-userinit --non-interactive \
#   --load /tmp/quicklisp.lisp \
#   --eval "(quicklisp-quickstart:install)" \
#   --eval "(ql::without-prompting (dolist (imp '(:sbcl :ccl :abcl :ecl)) (ql:add-to-init-file imp)))" \
#   && rm -rf /tmp/*

# FIXME remove this for production (but make available in development?)
# RUN apk add --no-cache bash

# FIXME improve security and fingerprinting for quicklisp install...
RUN cd /tmp && \
  wget https://beta.quicklisp.org/quicklisp.lisp && \
  sbcl --load quicklisp.lisp --quit --eval '(quicklisp-quickstart:install)'

COPY sbclrc /root/.sbclrc
COPY ./ /root/prod/
RUN ln -s /root/prod/prod.asd /root/quicklisp/local-projects/

EXPOSE 8080

WORKDIR /root

ENTRYPOINT ["sbcl", "--eval", "(ql:quickload 'prod)", "--eval", "(control:start-server)"]
