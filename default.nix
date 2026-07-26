{ wisp-version, zig, stdenvNoCC }:

stdenvNoCC.mkDerivation {
  pname = "wisp";
  version = wisp-version;
  src = ./core;

  nativeBuildInputs = [zig];

  XDG_CACHE_HOME = ".cache";

  buildPhase = "zig build";
  testPhase = "zig build test";

  installPhase = ''
    mkdir -p $out/{bin,lib}
    cp zig-out/bin/wisp $out/bin/
    cp zig-out/bin/wisp.wasm $out/lib/
  '';
}
