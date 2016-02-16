define export_vars
endef

ERL ?= erl
ERLC = erlc
EBIN_DIRS := $(wildcard deps/*/ebin)
REL_DIR = rel
NODE = {{name}}
REL = {{name}}
SCRIPT_PATH  := $(REL_DIR)/$(NODE)/bin/$(REL)
REBAR = `which rebar || ./rebar`

.PHONY: rel deps

all: deps compile

compile: deps
	bash -c "$(export_vars) $(REBAR) compile"

deps: 
	bash -c "$(export_vars) $(REBAR) -q get-deps"
	bash -c "$(export_vars) $(REBAR) check-deps"

clean:
	bash -c "$(export_vars) $(REBAR) clean"
	rm -rf ./deps/*
	rm -rf ./.rebar ./.eunit

test: compile
	bash -c "$(export_vars) $(REBAR) skip_deps=true eunit"

rel: deps
	bash -c "$(export_vars) $(REBAR) compile generate"

start: $(SCRIPT_PATH)
	@./$(SCRIPT_PATH) start

stop: $(SCRIPT_PATH)
	@./$(SCRIPT_PATH) stop

ping: $(SCRIPT_PATH)
	@./$(SCRIPT_PATH) ping

attach: $(SCRIPT_PATH)
	@./$(SCRIPT_PATH) attach

console: $(SCRIPT_PATH)
	@./$(SCRIPT_PATH) console

restart: $(SCRIPT_PATH)
	@./$(SCRIPT_PATH) restart

reboot: $(SCRIPT_PATH)
	@./$(SCRIPT_PATH) reboot

doc:
	bash -c "$(export_vars) $(REBAR) skip_deps=true doc"

dev:
	@erl -pa ebin include deps/*/ebin deps/*/include ebin include -boot start_sasl

analyze: checkplt
	bash -c "$(export_vars) $(REBAR) skip_deps=true dialyze"

buildplt:
	bash -c "$(export_vars) $(REBAR) skip_deps=true build-plt"

checkplt: buildplt
	bash -c "$(export_vars) $(REBAR) skip_deps=true check-plt"
