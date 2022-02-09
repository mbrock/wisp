default: wisp web test
web: wasm/wisp.js dist/index.html

SOURCES = \
  wisp.c wisp-eval.c \
  wisp-tidy.c wisp-builtins.c \
  wisp-read.c wisp-dump.c

CFLAGS += -g

EMCCFLAGS += -s WASM=1
EMCCFLAGS += -s EXPORTED_RUNTIME_METHODS=ccall,cwrap,FS,ENV,IDBFS
EMCCFLAGS += -lidbfs.js
EMCCFLAGS += -s MODULARIZE=1 -s 'EXPORT_NAME="loadWisp"'

wasm/wisp.js: $(SOURCES)
	emcc $(CFLAGS) $^ -o $@ $(EMCCFLAGS)

dist/index.html: index.tsx index.html wasm/wisp.js wasm/wisp.wasm build
	./build

wisp: $(SOURCES)
	$(CC) $(CFLAGS) $^ -o wisp

test:
	./wisp -e '(funcall (lambda (x . y) y) 1 2 3)'

clean:; rm -rf wisp dist/* wasm/*
