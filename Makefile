default: zig-out/bin/wisp
web: zig-out/lib/wisp.wasm dist/index.html

SOURCES = \
  wisp.c wisp-eval.c \
  wisp-tidy.c wisp-builtins.c \
  wisp-read.c wisp-dump.c

ZIGSOURCES = \
  src/wisp.zig src/base.zig src/read.zig src/print.zig src/eval.zig

CFLAGS += -g

# EM_METHODS = ccall,cwrap,FS,ENV,IDBFS,lengthBytesUTF8,stringToUTF8

# EMCCFLAGS += -s WASM=1
# EMCCFLAGS += -s EXPORTED_RUNTIME_METHODS=$(EM_METHODS)
# EMCCFLAGS += -s EXPORTED_FUNCTIONS=_malloc,_free,_main
# EMCCFLAGS += -lidbfs.js
# EMCCFLAGS += -s MODULARIZE=1 -s 'EXPORT_NAME="loadWisp"'

zig-out/bin/wisp: build.zig $(ZIGSOURCES)
	zig build test

zig-out/lib/wisp.wasm: build.zig $(ZIGSOURCES)
	zig build

# wasm/wisp.js: $(SOURCES)
# 	emcc $(CFLAGS) $^ -o $@ $(EMCCFLAGS)

dist/index.html: index.tsx index.html zig-out/lib/wisp.wasm build
	./build

wisp: $(SOURCES)
	$(CC) $(CFLAGS) $^ -o wisp

test:
	./wisp -e '(funcall (lambda (x . y) y) 1 2 3)'

clean:; rm -rf wisp dist/* wasm/*
