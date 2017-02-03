REBAR := $(shell which rebar3 2>/dev/null || which ./rebar3)
SUBMODULES = schemes/swag apps/cp_proto/damsel build_utils
SUBTARGETS = $(patsubst %,%/.git,$(SUBMODULES))

SWAGGER_CODEGEN ?= $(call which, swagger-codegen)
SWAGGER_SCHEME = schemes/swag/swagger.yaml
SWAGGER_APP_PATH = apps/swagger

SWAGGER_APP_TARGET = $(SWAGGER_APP_PATH)/rebar.config

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
BASE_IMAGE_TAG := 4000337c0ca19978467f62ca6505a03c2569de40

BUILD_IMAGE_TAG := f1290453c50bce74a082e3d936aa5c44fd3750fc

CALL_ANYWHERE := all submodules rebar-update compile xref lint dialyze test start devrel release clean distclean

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

compile: submodules $(SWAGGER_APP_TARGET) rebar-update
	$(REBAR) compile

xref:
	$(REBAR) xref

lint: compile
	elvis rock

dialyze:
	$(REBAR) dialyzer

start: submodules
	$(REBAR) run

devrel: submodules
	$(REBAR) release

release: distclean $(SWAGGER_APP_TARGET)
	$(REBAR) as prod release

clean:
	$(REBAR) cover -r
	$(REBAR) clean

distclean:
	$(REBAR) clean -a
	rm -rfv _build apps/swagger

# CALL_W_CONTAINER
test: submodules
	$(REBAR) ct

# Shitty generation. Will be replaced when a container with swagger-codegen appear
define swagger_regenerate
	rm -rf $(SWAGGER_APP_PATH)
	$(SWAGGER_CODEGEN) generate -i $(SWAGGER_SCHEME) -l erlang-server -o $(SWAGGER_APP_PATH)
endef

$(SWAGGER_APP_TARGET): $(SWAGGER_SCHEME)
	$(call swagger_regenerate)

swagger_regenerate:
	$(call swagger_regenerate)

cover:
	$(REBAR) cover
