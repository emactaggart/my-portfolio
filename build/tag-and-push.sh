#!/usr/bin/env bash

if [ ! -d "$APPS_MY_PORTFOLIO_ROOT" ]; then
    echo 'The env var $APPS_MY_PORTFOLIO_ROOT does not appear to be set' && exit 1
else
    pushd $APPS_MY_PORTFOLIO_ROOT
fi

# docker run -p 80:80 --entrypoint sh --interactive --tty my-portfolio:alpine
# docker run -p 80:80 --entrypoint sh --interactive --tty my-portfolio:alpine -c /root/my-portfolio.exe

image="my-portfolio"
tag="alpine"
magic_repo_name_prefix="gcr.io/my-portfolio-334306"

# docker tag $image $(docker-aws-prefix $image)
docker tag localhost/$image:$tag $magic_repo_name_prefix/$image:$tag

docker push --remove-signatures $magic_repo_name_prefix/$image:$tag

popd
