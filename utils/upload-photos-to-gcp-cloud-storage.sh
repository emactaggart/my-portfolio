#!/usr/bin/env bash

if [ ! -d "$APPS_MY_PORTFOLIO_ROOT" ]; then
    echo 'The env var $APPS_MY_PORTFOLIO_ROOT does not appear to be set' && exit 1
else
    pushd $APPS_MY_PORTFOLIO_ROOT
fi


## List available buckets
# `gsutil ls`

## List a specific buckets contents
# `gsutil ls <bucket-name>`

## Enable public access to storage bucket # https://cloud.google.com/storage/docs/access-control/making-data-public?hl=en-GB
# `gsutil iam ch allUsers:objectViewer gs://my-portfolio-static`
## Access publicly accessible objects via
# `https://storage.googleapis.com/BUCKET_NAME/OBJECT_NAME`

## TODO CORS? Prevent websites other than local and prod from accessing these assets?


# Store all static files
# TODO use `gsutil rsync -R`?  `gsutil rsync -R local-dir gs://my-static-assets` # https://cloud.google.com/storage/docs/hosting-static-website?hl=en-GB
# TODO Will this work with updated photo versions?
gsutil cp -R ./static gs://my-portfolio-static
