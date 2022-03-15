default:; cd core && zig build test && zig build
test:; cd core && zig build test

.PHONY: web

web: default
	cd web && ./build

clean:; rm -rf web/dist/* core/zig-*
deploy:; cp web/dist/* /restless/www/wisp/
deploy-nodetown: web; scp web/dist/* wisp.town:/restless/www/wisp/
