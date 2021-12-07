#!/usr/bin/env bash

if [ ! -d "$APPS_MY_PORTFOLIO_ROOT" ]; then
    echo 'The env var $APPS_MY_PORTFOLIO_ROOT does not appear to be set' && exit 1
else
    pushd $APPS_MY_PORTFOLIO_ROOT
fi

image="my-portfolio"
tag="alpine"
repo_name_prefix="gcr.io/my-portfolio-334306"

docker tag localhost/$image:$tag $repo_name_prefix/$image:$tag

docker push --remove-signatures $repo_name_prefix/$image:$tag

popd
