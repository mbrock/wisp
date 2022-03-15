{ wisp, mkYarnPackage, electron, makeWrapper }:

mkYarnPackage rec {
  pname = "wisp-ide";
  version = "0.7.5";
  src = ./.;
  nativeBuildInputs = [makeWrapper];

  buildPhase = ''
    ls -alR deps
    pushd deps/wisp-ide
    cp ${wisp}/lib/wisp.wasm .
    ./build
    popd
  '';

  installPhase = ''
    mkdir -p $out/share/wisp/web
    cp deps/wisp-ide/dist/* $out/share/wisp/web
    mkdir -p $out/bin
    makeWrapper ${electron}/bin/electron $out/bin/wisp-ide \
      --add-flags $out/share/wisp/web/index.html
  '';

  distPhase = ":";
}
