ERL       ?= erl
ERLC      ?= $(ERL)c
APP       := zotonic
PARSER     =src/erlydtl/erlydtl_parser

GIT_CHECK := $(shell test -d .git && git submodule update --init)
MAKEFILES := $(shell find -L modules priv/sites priv/modules priv/extensions priv/sites/*/modules -maxdepth 2 -name Makefile)

DEPS_DIR := $(shell pwd)/deps

.PHONY: all
all: deps makefile-deps $(PARSER).erl erl ebin/$(APP).app 

.PHONY: erl
erl:
	@$(ERL) -pa $(wildcard deps/*/ebin) -pa ebin -noinput +B \
	  -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

$(PARSER).erl: $(PARSER).yrl
	$(ERLC) -o src/erlydtl $(PARSER).yrl

.PHONY: deps
deps: iconv mimetypes bert.erl dh_date gen_smtp lager mochiweb ua_classifier webzmachine z_stdlib poolboy esql esqlite pgsql esql_pgsql esql_sqlite3
.PHONY: clean_deps
clean_deps:
	(cd deps/iconv; ./rebar clean)
	(cd deps/mimetypes; ./rebar clean)
	(cd deps/bert.erl; ./rebar clean)
	(cd deps/dh_date; ./rebar clean)
	(cd deps/gen_smtp; ./rebar clean)
	(cd deps/lager; ./rebar clean)
	(cd deps/ua_classifier; ./rebar clean)
	(cd deps/webzmachine; ./rebar clean)
	(cd deps/z_stdlib; ./rebar clean)
	(cd deps/poolboy; ./rebar clean)
	(cd deps/esql; ./rebar clean)
	(cd deps/esqlite; ./rebar clean)
	(cd deps/pgsql; ./rebar clean)
	(cd deps/esql_sqlite3; ./rebar clean)
	(cd deps/esql_pgsql; ./rebar clean)

iconv:
	cd deps/iconv && ./rebar compile
mimetypes:
	cd deps/mimetypes && ./rebar compile
bert.erl:
	cd deps/bert.erl && ./rebar compile
dh_date:
	cd deps/dh_date && ./rebar compile
gen_smtp:
	cd deps/gen_smtp && ./rebar compile
lager:
	cd deps/lager && ./rebar compile
mochiweb:
	cd deps/mochiweb && ./rebar compile
ua_classifier:
	cd deps/ua_classifier && ./rebar compile
webzmachine:
	cd deps/webzmachine && ./rebar compile
z_stdlib:
	cd deps/z_stdlib && ./rebar compile
poolboy:
	cd deps/poolboy && ./rebar deps_dir=$(DEPS_DIR) compile
esql:
	cd deps/esql && ./rebar deps_dir=$(DEPS_DIR) compile
esqlite:
	cd deps/esqlite && ./rebar deps_dir=$(DEPS_DIR) compile
pgsql:
	cd deps/pgsql && ./rebar deps_dir=$(DEPS_DIR) compile
esql_sqlite3:
	cd deps/esql_sqlite3 && ./rebar deps_dir=$(DEPS_DIR) compile
esql_pgsql:
	cd deps/esql_pgsql && ./rebar deps_dir=$(DEPS_DIR) compile

makefile-deps:
	@if [ "${MAKEFILES}" != "" ]; then for f in ${MAKEFILES}; do echo $$f; $(MAKE) -C `dirname $$f`; done; fi

.PHONY: docs edocs
docs:
	@echo Building HTML documentation...
	cd doc && make stubs && make html
	@echo HTML documentation is now available in doc/_build/html/

edocs:
	@echo Building reference edoc documentation...
	bin/zotonic generate-edoc

.PHONY: clean_logs
clean_logs:
	@echo "deleting logs:"
	rm -f erl_crash.dump $(PARSER).erl
	rm -f priv/log/*

.PHONY: clean
clean: clean_logs clean_deps
	@echo "removing:"
	@if [ "${MAKEFILES}" != "" ]; then for f in ${MAKEFILES}; do echo $$f; $(MAKE) -C `dirname $$f` clean; done; fi
	rm -f ebin/*.beam ebin/*.app

ebin/$(APP).app:
	cp src/$(APP).app $@
