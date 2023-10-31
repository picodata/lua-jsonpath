.PHONY: dev-prepare
dev-prepare:
	./dev_deps.sh

.PHONY: lint
lint:
	.rocks/bin/luacheck --config=.luacheckrc jsonpath.lua

.PHONY: test
test: build lint
	./test/test.lua

.PHONY: build
build:
	tarantoolctl rocks make jsonpath-pico.scm-1.rockspec

all: build test
