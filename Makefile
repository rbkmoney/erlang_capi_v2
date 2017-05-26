REBAR := $(shell which rebar3 2>/dev/null || which ./rebar3)
SUBMODULES = schemes/swag apps/cp_proto/damsel build_utils
SUBTARGETS = $(patsubst %,%/.git,$(SUBMODULES))

COMPOSE_HTTP_TIMEOUT := 300
export COMPOSE_HTTP_TIMEOUT

UTILS_PATH := build_utils
TEMPLATES_PATH := .

# Name of the service
SERVICE_NAME := capi
# Service image default tag
SERVICE_IMAGE_TAG ?= $(shell git rev-parse HEAD)
# The tag for service image to be pushed with
SERVICE_IMAGE_PUSH_TAG ?= $(SERVICE_IMAGE_TAG)

# Base image for the service
BASE_IMAGE_NAME := service_erlang
BASE_IMAGE_TAG := 13454a94990acb72f753623ec13599a9f6f4f852

BUILD_IMAGE_TAG := a7a54a8f179411f222a82eba612039c3434923ff

CALL_ANYWHERE := all submodules rebar-update compile xref lint dialyze test start devrel release clean distclean swagger.regenerate

CALL_W_CONTAINER := $(CALL_ANYWHERE)

.PHONY: $(CALL_W_CONTAINER) all cover

all: compile

-include $(UTILS_PATH)/make_lib/utils_container.mk
-include $(UTILS_PATH)/make_lib/utils_image.mk

$(SUBTARGETS): %/.git: %
	git submodule update --init $<
	touch $@

submodules: $(SUBTARGETS)

rebar-update:
	$(REBAR) update

generate: swagger.generate

compile: submodules rebar-update generate
	$(REBAR) compile

xref:
	$(REBAR) xref

lint: generate
	elvis rock

dialyze:
	$(REBAR) dialyzer

start: submodules
	$(REBAR) run

devrel: submodules
	$(REBAR) release

release: submodules distclean generate
	$(REBAR) as prod release

clean:
	$(REBAR) cover -r
	$(REBAR) clean

distclean: swagger.distclean
	$(REBAR) clean
	rm -rf _build

cover:
	$(REBAR) cover

# CALL_W_CONTAINER
test: submodules
	$(REBAR) do eunit, ct

# Swagger stuff
SWAGGER_CODEGEN = $(call which, swagger-codegen)
SWAGGER_SCHEME_PATH = schemes/swag
SWAGGER_SCHEME = $(SWAGGER_SCHEME_PATH)/web_deploy/swagger.yaml
SWAGGER_PREFIX = swag_server
SWAGGER_APP_PATH = apps/$(SWAGGER_PREFIX)
SWAGGER_APP_TARGET = $(SWAGGER_APP_PATH)/rebar.config

swagger.generate: $(SWAGGER_APP_TARGET)

swagger.distclean:
	rm -rf $(SWAGGER_APP_PATH)

swagger.regenerate: swagger.distclean swagger.generate

$(SWAGGER_APP_TARGET): $(SWAGGER_SCHEME)
	$(SWAGGER_CODEGEN) generate -i $(SWAGGER_SCHEME) -l erlang-server -o $(SWAGGER_APP_PATH) --additional-properties packageName=$(SWAGGER_PREFIX)

$(SWAGGER_SCHEME): $(SWAGGER_SCHEME_PATH)/.git
	$(MAKE) -C $(SWAGGER_SCHEME_PATH)
