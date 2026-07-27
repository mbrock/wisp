# nanoarrow

This is the Apache Arrow nanoarrow 0.8.0 amalgamation, generated
from tag `apache-arrow-nanoarrow-0.8.0` at commit
`a579fbf5d192e85b6249935e117de7d02a6dc4e9`.

The bundle was generated with IPC and FlatCC support:

```sh
python ci/scripts/bundle.py \
  --source-output-dir=dist/src \
  --include-output-dir=dist/include \
  --header-namespace= \
  --with-ipc \
  --with-flatcc
```

Wisp declares the small C ABI it uses directly in Zig. The
nanoarrow headers are retained as the authoritative declarations
for the vendored C sources and for auditing that ABI.
