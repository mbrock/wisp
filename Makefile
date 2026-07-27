all: core-fast web
core-debug:; cd core && zig build
core-fast:; cd core && zig build -Doptimize=ReleaseFast
test:; cd core && zig build test
bench:; cd core && zig build bench -Doptimize=ReleaseFast
bench-sweep:; python3 benchmarks/cross-language/sweep.py

.PHONY: web core dev pages bench bench-sweep

web: core-fast
	cd web && ./build
pages: web
	rm -rf web/pages
	mkdir -p web/pages/lib web/pages/dist
	cp web/index.html web/index.css web/index.js \
	  web/wisp.js web/wasi.js web/service-worker.js \
	  web/js.wisp web/dexp.wisp web/demo.wisp \
	  web/pages/
	cp web/lib/*.js web/lib/*.js.map web/pages/lib/
	cp web/dist/wisp.wasm web/pages/dist/
	touch web/pages/.nojekyll
dev: web
	cd web && deno run \
	  --watch=server.js,dev-server.wisp,http.wisp,deno-base.wisp,js.wisp \
	  --no-clear-screen \
	  --allow-read --allow-net --allow-run --allow-env \
	  server.js dev-server.wisp
clean:; rm -rf web/dist/* core/zig-*
deploy:; cp web/dist/* /restless/www/wisp/
deploy-nodetown: web; scp web/dist/* wisp.town:/restless/www/wisp/

wasm-sanity:
	cd core && zig build -Dtarget=wasm32-wasi && \
	  wasmtime zig-out/bin/wisp.wasm eval "(+ 1 1)"
