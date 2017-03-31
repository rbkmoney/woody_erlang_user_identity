REBAR := $(shell which rebar3 2>/dev/null || which ./rebar3)
SUBMODULES = build_utils
SUBTARGETS = $(patsubst %,%/.git,$(SUBMODULES))

UTILS_PATH := build_utils
TEMPLATES_PATH := .

BUILD_IMAGE_TAG := 55e987e74e9457191a5b4a7c5dc9e3838ae82d2b

CALL_ANYWHERE := all submodules rebar-update compile xref lint dialyze test start devrel release clean distclean

SERVICE_NAME := woody_user_identity

# Hint: 'test' might be a candidate for CALL_W_CONTAINER-only target
CALL_W_CONTAINER := $(CALL_ANYWHERE)

.PHONY: $(CALL_W_CONTAINER) generate

all: compile

-include $(UTILS_PATH)/make_lib/utils_container.mk

$(SUBTARGETS): %/.git: %
	git submodule update --init $<
	touch $@

submodules: $(SUBTARGETS)

rebar-update:
	$(REBAR) update

compile: submodules rebar-update
	$(REBAR) compile

devrel: submodules
	$(REBAR) release

start: devrel
	$(REBAR) run

test: submodules
	$(REBAR) do eunit, ct

xref: submodules
	$(REBAR) xref

clean:
	$(REBAR) clean

distclean: clean
	rm -rf _build

dialyze:
	$(REBAR) dialyzer

release: distclean
	$(REBAR) as prod release

lint:
	elvis rock

