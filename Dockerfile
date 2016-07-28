FROM rbkmoney/service_erlang:latest
MAINTAINER Artem Ocheredko <a.ocheredko@rbkmoney.com>
COPY ./_build/prod/rel/capi /opt/capi
CMD /opt/capi/bin/capi foreground
LABEL service_version="semver"
WORKDIR /opt/capi

