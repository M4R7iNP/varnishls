BUILD_TIME = $(shell date +"%Y/%m/%d %H:%M:%S")
GIT_REVISION = $(shell git log -1 --format="%h")
RUST_VERSION = $(word 2, $(shell rustc -V))
LONG_VERSION = "$(VERSION) ( rev: $(GIT_REVISION), rustc: $(RUST_VERSION), build at: $(BUILD_TIME) )"
BIN_NAME = "varnish_lsp"

export LONG_VERSION

.PHONY = all test clean tree-sitter-vcl build

test:
	cargo test

clean:
	cargo clean

tree-sitter-vcl:
	cd vendor/tree-sitter-vcl && npm ci && npm run build && tree-sitter test

tree-sitter-vtc:
	cd vendor/tree-sitter-vtc && npm ci && npm run build && tree-sitter test

build:
	cargo build
