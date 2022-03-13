default: zig-out/bin/wisp

.PHONY: web

SOURCES = \
  00-util.zig 01-word.zig 02-base.zig 03-tidy.zig 04-eval.zig \
  05-read.zig 06-dump.zig 07-xops.zig 08-fops.zig 09-mops.zig \
  0a-repl.zig 0b-disk.zig ff-wisp.zig \
  a0-base.lisp

zig-out/bin/wisp: build.zig $(ZIGSOURCES)
	zig build test

zig-out/lib/wisp.wasm: build.zig $(ZIGSOURCES)
	zig build

web:
	zig build
	cd web && ./build

clean:; rm -rf web/dist/* wasm/* zig-cache zig-out src/zig-cache

deploy:; cp web/dist/* /restless/www/wisp/
deploy-nodetown: web; scp web/dist/* node.town:/restless/www/wisp/
