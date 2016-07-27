REBAR := $(shell which rebar3 2>/dev/null || which ./rebar3)
RELNAME = capi
SUBMODULES = schemes/swag
SUBTARGETS = $(patsubst %,%/.git,$(SUBMODULES))

SWAGGER_SCHEME = schemes/swag/swagger.yaml
SWAGGER_APP_PATH = apps/swagger
SWAGGER_APP_TARGET = $(SWAGGER_APP_PATH)/rebar.config

which = $(if $(shell which $(1) 2>/dev/null),\
	$(shell which $(1) 2>/dev/null),\
	$(error "Error: could not locate $(1)!"))

DOCKER = $(call which, docker)
PACKER = $(call which, packer)
SWAGGER_CODEGEN = $(call which, SWAGGER_CODEGEN)

.PHONY: all submodules compile devrel start test clean distclean dialyze release containerize swagger_regenerate

all: compile

rebar-update:
	$(REBAR) update

$(SUBTARGETS): %/.git: %
	git submodule update --init $<
	touch $@

submodules: $(SUBTARGETS) $(SWAGGER_APP_TARGET)

compile: submodules
	$(REBAR) compile

devrel: submodules
	$(REBAR) release

start: submodules
	$(REBAR) run

test: submodules
	$(REBAR) ct

lint: compile
	elvis rock

xref: submodules
	$(REBAR) xref

clean:
	$(REBAR) clean

distclean:
	$(REBAR) clean -a
	rm -rfv _build _builds _cache _steps _temp

dialyze:
	$(REBAR) dialyzer

BASE_DIR := $(shell pwd)

release: ~/.docker/config.json distclean
	$(DOCKER) run --rm -v $(BASE_DIR):$(BASE_DIR) --workdir $(BASE_DIR) rbkmoney/build rebar3 as prod release

containerize: release ./packer.json
	$(PACKER) build packer.json

~/.docker/config.json:
	test -f ~/.docker/config.json || (echo "Please run: docker login" ; exit 1)

# Shitty generation. Will be replaced when a container with swagger-codegen appear
define swagger_regenerate
	rm -rf $(SWAGGER_APP_PATH)
	$(SWAGGER_CODEGEN) generate -i $(SWAGGER_SCHEME) -l erlang-server -o $(SWAGGER_APP_PATH);
endef

$(SWAGGER_APP_TARGET): $(SWAGGER_SCHEME)
	$(call swagger_regenerate)

swagger_regenerate:
	$(call swagger_regenerate)
