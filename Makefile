REBAR := $(shell which rebar3 2>/dev/null || which ./rebar3)
SUBMODULES = schemes/swag build_utils
SUBTARGETS = $(patsubst %,%/.git,$(SUBMODULES))

COMPOSE_HTTP_TIMEOUT := 300
export COMPOSE_HTTP_TIMEOUT

UTILS_PATH := build_utils
TEMPLATES_PATH := .

# Name of the service
SERVICE_NAME := capi-v2
# Service image default tag
SERVICE_IMAGE_TAG ?= $(shell git rev-parse HEAD)
# The tag for service image to be pushed with
SERVICE_IMAGE_PUSH_TAG ?= $(SERVICE_IMAGE_TAG)

# Base image for the service
BASE_IMAGE_NAME := service-erlang
BASE_IMAGE_TAG := c114fc51a7b166d22144fcbf856f217dc7b5946f

BUILD_IMAGE_NAME := build-erlang
BUILD_IMAGE_TAG := 3d676c0635bacae5a05850734894182069ef1b7a

CALL_ANYWHERE := \
	submodules \
	all compile xref lint dialyze update_plt test cover \
	start devrel release clean distclean \
	generate regenerate swag_server.regenerate swag_client.regenerate \
	check_format format

CALL_W_CONTAINER := $(CALL_ANYWHERE)

.PHONY: $(CALL_W_CONTAINER) all

all: compile

-include $(UTILS_PATH)/make_lib/utils_container.mk
-include $(UTILS_PATH)/make_lib/utils_image.mk

$(SUBTARGETS): %/.git: %
	git submodule update --init $<
	touch $@

submodules: $(SUBTARGETS)

generate: swag_server.generate swag_client.generate

regenerate: swag_server.regenerate swag_client.regenerate

compile: submodules generate
	$(REBAR) compile

xref:
	$(REBAR) xref

lint: generate
	elvis rock -V

check_format:
	$(REBAR) fmt -c

format:
	$(REBAR) fmt -w

dialyze:
	$(REBAR) as test dialyzer

update_plt:
	$(REBAR) dialyzer -u true -s false

start: submodules
	$(REBAR) run

release: submodules generate
	$(REBAR) as prod release

clean:
	$(REBAR) cover -r
	$(REBAR) clean

distclean: swag_server.distclean swag_client.distclean
	$(REBAR) clean
	rm -rf _build

cover:
	$(REBAR) cover

# CALL_W_CONTAINER
test: submodules generate
	$(REBAR) do eunit, ct

# Swagger stuff
SWAGGER_CODEGEN = $(call which, swagger-codegen)
SWAGGER_SCHEME_PATH = schemes/swag
SWAGGER_SCHEME = $(SWAGGER_SCHEME_PATH)/swagger.yaml

$(SWAGGER_SCHEME): $(SWAGGER_SCHEME_PATH)/.git

# Swagger server
SWAG_SERVER_PREFIX = swag_server
SWAG_SERVER_APP_PATH = apps/$(SWAG_SERVER_PREFIX)
SWAG_SERVER_APP_TARGET = $(SWAG_SERVER_APP_PATH)/rebar.config

swag_server.generate: $(SWAG_SERVER_APP_TARGET)

swag_server.distclean:
	rm -rf $(SWAG_SERVER_APP_PATH)

swag_server.regenerate: swag_server.distclean swag_server.generate

$(SWAG_SERVER_APP_TARGET): $(SWAGGER_SCHEME)
	$(SWAGGER_CODEGEN) generate \
		-i $(SWAGGER_SCHEME) \
		-l erlang-server \
		-o $(SWAG_SERVER_APP_PATH) \
		--additional-properties \
			packageName=$(SWAG_SERVER_PREFIX)

# Swagger client
SWAG_CLIENT_PREFIX = swag_client
SWAG_CLIENT_APP_PATH = apps/$(SWAG_CLIENT_PREFIX)
SWAG_CLIENT_APP_TARGET = $(SWAG_CLIENT_APP_PATH)/rebar.config

swag_client.generate: $(SWAG_CLIENT_APP_TARGET)

swag_client.distclean:
	rm -rf $(SWAG_CLIENT_APP_PATH)

swag_client.regenerate: swag_client.distclean swag_client.generate

$(SWAG_CLIENT_APP_TARGET): $(SWAGGER_SCHEME)
	$(SWAGGER_CODEGEN) generate \
		-i $(SWAGGER_SCHEME) \
		-l erlang-client \
		-o $(SWAG_CLIENT_APP_PATH) \
		--additional-properties \
			packageName=$(SWAG_CLIENT_PREFIX)
