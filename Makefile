default: zig-out/bin/wisp
web: wasm/wisp.js dist/index.html

SOURCES = \
  wisp.c wisp-eval.c \
  wisp-tidy.c wisp-builtins.c \
  wisp-read.c wisp-dump.c

CFLAGS += -g

EM_METHODS = ccall,cwrap,FS,ENV,IDBFS,lengthBytesUTF8,stringToUTF8

EMCCFLAGS += -s WASM=1
EMCCFLAGS += -s EXPORTED_RUNTIME_METHODS=$(EM_METHODS)
EMCCFLAGS += -s EXPORTED_FUNCTIONS=_malloc,_free,_main
EMCCFLAGS += -lidbfs.js
EMCCFLAGS += -s MODULARIZE=1 -s 'EXPORT_NAME="loadWisp"'

zig-out/bin/wisp: build.zig src/wisp.zig src/read.zig src/print.zig
	zig build test

wasm/wisp.js: $(SOURCES)
	emcc $(CFLAGS) $^ -o $@ $(EMCCFLAGS)

dist/index.html: index.tsx index.html wasm/wisp.js wasm/wisp.wasm build
	./build

wisp: $(SOURCES)
	$(CC) $(CFLAGS) $^ -o wisp

test:
	./wisp -e '(funcall (lambda (x . y) y) 1 2 3)'

clean:; rm -rf wisp dist/* wasm/*
