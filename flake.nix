{
  inputs = {
    nixpkgs.url =
      github:nixos/nixpkgs/nixpkgs-unstable;

    flake-utils.url =
      github:numtide/flake-utils;

    zig.url = github:roarkanize/zig-overlay;

    wapm-cli = {
      url = github:wasmerio/wapm-cli;
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, zig, wapm-cli }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            (self: super: {
              zigpkgs = inputs.zig.packages.${system};
            })
          ];
        };

        wisp-json = pkgs.lib.importJSON ./web/package.json;
        wisp-version = wisp-json.version;

        wapm-manifest = pkgs.writeText "wapm.toml" ''
          [package]
          name = "mbrock/wisp"
          version = "${wisp-version}"
          description = "Lisp for WebAssembly"
          license = "AGPL-3.0-or-later"
          repository = "https://github.com/mbrock/wisp"

          [[module]]
          name = "wisp"
          source = "wisp.wasm"
          abi = "wasi"

          [module.interfaces]
          wasi = "0.1.0-unstable"

          [[command]]
          name = "wisp"
          module = "wisp"
        '';

      in rec {
        devShell = packages.wisp;
        defaultPackage = packages.wisp;
        packages = rec {
          zig = inputs.zig.packages.${system}.master.latest;

          wisp = pkgs.callPackage ./default.nix {
            inherit wisp-version;
            inherit zig;
          };

          wisp-ide = pkgs.callPackage ./web/default.nix {
            inherit wisp;
          };

          wisp-release-wapm =
            pkgs.writeShellScriptBin "wisp-release-wapm" ''
              dir=$(mktemp -dt wisp-wapm)
              cd "$dir"
              cp "${wisp}"/bin/wisp.wasm .
              cp "${wapm-manifest}" wapm.toml

              # Not using our wapm package because it doesn't work on OS X.
              wapm publish
            '';

          # This doesn't work on OS X.
          wapm = pkgs.rustPlatform.buildRustPackage rec {
            pname = "wapm";
            version = "0.5.3";
            src = inputs.wapm-cli;
            cargoSha256 = "sha256-OIoQelTuXrOM3Kb21XAAgvPKJXU2cc3AoH/TRBYBiEM=";
            LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";
          };

          inherit (pkgs) wasmer;
        };
      }
    );
}
