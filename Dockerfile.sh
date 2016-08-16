#!/bin/bash
cat <<EOF
FROM $BASE_IMAGE
MAINTAINER Artem Ocheredko <a.ocheredko@rbkmoney.com>
COPY ./_build/prod/rel/capi /opt/capi
WORKDIR /opt/capi
CMD bin/fetch-api-pubkey && bin/capi foreground
EXPOSE 8080
LABEL base_image_tag=$BASE_IMAGE_TAG
LABEL build_image_tag=$BUILD_IMAGE_TAG
LABEL branch=$( \
  if [ "HEAD" != $(git rev-parse --abbrev-ref HEAD) ]; then \
    echo $(git rev-parse --abbrev-ref HEAD); \
  elif [ -n "$BRANCH_NAME" ]; then \
    echo $BRANCH_NAME; \
  else \
    echo $(git name-rev --name-only HEAD); \
  fi)
LABEL commit=$(git rev-parse HEAD)
LABEL commit_number=$(git rev-list --count HEAD)
EOF
