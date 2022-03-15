{ wisp, mkYarnPackage, electron, makeWrapper }:

mkYarnPackage rec {
  pname = "wisp-web";
  version = "0.7.5";
  src = ./web;
  buildInputs = [zig];

  XDG_CACHE_HOME = ".cache";

  testPhase = "zig build test";
  buildPhase = "zig build";
  installPhase = ''
    mkdir -p $out/bin
    cp zig-out/bin/* $out/bin/
  '';
}
