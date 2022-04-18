all: core-fast web
core-debug:; cd core && zig build
core-fast:; cd core && zig build -Drelease-fast=true
test:; cd core && zig build test

.PHONY: web core

web:; cd web && ./build
clean:; rm -rf web/dist/* core/zig-*
deploy:; cp web/dist/* /restless/www/wisp/
deploy-nodetown: web; scp web/dist/* wisp.town:/restless/www/wisp/

wasm-sanity:
	cd core && zig build -Dtarget=wasm32-wasi && \
	  wasmtime zig-out/bin/wisp.wasm eval "(+ 1 1)"
