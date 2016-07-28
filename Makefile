REBAR := $(shell which rebar3 2>/dev/null || which ./rebar3)
SUBMODULES = schemes/swag apps/capi_proto/damsel
SUBTARGETS = $(patsubst %,%/.git,$(SUBMODULES))

REGISTRY := dr.rbkmoney.com
ORG_NAME := rbkmoney
BASE_IMAGE := "$(REGISTRY)/$(ORG_NAME)/build:latest"

# Note: RELNAME should match the name of
# the first service in docker-compose.yml
RELNAME := capi

TAG = latest
IMAGE_NAME = "$(REGISTRY)/$(ORG_NAME)/$(RELNAME):$(TAG)"

CALL_ANYWHERE := submodules rebar-update compile xref lint dialyze start devrel release clean distclean swagger_regenerate

CALL_W_CONTAINER := $(CALL_ANYWHERE) test

SWAGGER_CODEGEN = $(call which, SWAGGER_CODEGEN)


.PHONY: $(CALL_W_CONTAINER) all containerize push $(UTIL_TARGETS)

all: compile

include utils.mk

# CALL_ANYWHERE
$(SUBTARGETS): %/.git: %
	git submodule update --init $<
	touch $@

submodules: $(SUBTARGETS)

rebar-update:
	$(REBAR) update

compile: submodules rebar-update
	$(REBAR) compile

xref: submodules
	$(REBAR) xref

lint: compile
	elvis rock

dialyze:
	$(REBAR) dialyzer

start: submodules
	$(REBAR) run

devrel: submodules
	$(REBAR) release

release: distclean
	$(REBAR) as prod release

clean:
	$(REBAR) clean

distclean:
	$(REBAR) clean -a
	rm -rfv _build _builds _cache _steps _temp

# CALL_W_CONTAINER
test: submodules
	$(REBAR) ct

# OTHER
containerize: w_container_release
	$(DOCKER) build --force-rm --tag $(IMAGE_NAME) .

push: containerize
	$(DOCKER) push "$(IMAGE_NAME)"

# Shitty generation. Will be replaced when a container with swagger-codegen appear
define swagger_regenerate
	rm -rf $(SWAGGER_APP_PATH)
	$(SWAGGER_CODEGEN) generate -i $(SWAGGER_SCHEME) -l erlang-server -o $(SWAGGER_APP_PATH);
endef

$(SWAGGER_APP_TARGET): $(SWAGGER_SCHEME)
	$(call swagger_regenerate)

swagger_regenerate:
	$(call swagger_regenerate)
