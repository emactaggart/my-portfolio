#!/usr/bin/env bash

pushd ~/Development/my-portfolio

# docker run -p 80:80 --entrypoint sh --interactive --tty my-portfolio:alpine
# docker run -p 80:80 --entrypoint sh --interactive --tty my-portfolio:alpine -c /root/my-portfolio.exe

image="my-portfolio"
tag="test-3"

# docker tag $image $(docker-aws-prefix $image)
docker tag localhost/$image:alpine $(docker-gcp-prefix $image:$tag)

# docker push --remove-signatures localhost/$image:alpine $(docker-gcp-prefix $image:$tag)

popd
