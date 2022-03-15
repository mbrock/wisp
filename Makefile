default:; zig build test && zig build
test:; zig build test

.PHONY: web

web:
	zig build
	cd web && ./build

clean:; rm -rf web/dist/* zig-cache zig-out src/zig-cache
deploy:; cp web/dist/* /restless/www/wisp/
deploy-nodetown: web; scp web/dist/* wisp.town:/restless/www/wisp/
