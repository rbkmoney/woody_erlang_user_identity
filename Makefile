REBAR := $(shell which rebar3 2>/dev/null || which ./rebar3)
SUBMODULES = build_utils
SUBTARGETS = $(patsubst %,%/.git,$(SUBMODULES))

UTILS_PATH := build_utils
TEMPLATES_PATH := .

BUILD_IMAGE_TAG := cd38c35976f3684fe7552533b6175a4c3460e88b

CALL_ANYWHERE := all submodules rebar-update compile xref lint dialyze test clean distclean check_format format

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

lint:
	elvis rock

check_format:
	$(REBAR) fmt -c

format:
	$(REBAR) fmt -w
