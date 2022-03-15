{ zig, stdenv }:

stdenv.mkDerivation {
  name = "wisp";
  version = "0.7";
  src = ./.;
  buildInputs = [zig];
  XDG_CACHE_HOME = ".cache";
  testPhase = "zig build test";
  buildPhase = "zig build";
  installPhase = ''
    mkdir -p $out/bin
    cp zig-out/bin/* $out/bin/
  '';
}
