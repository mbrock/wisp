{
  inputs = {
    nixpkgs.url =
      github:nixos/nixpkgs/nixpkgs-unstable;

    flake-utils.url =
      github:numtide/flake-utils;

    zig.url = github:roarkanize/zig-overlay;
  };

  outputs = { self, nixpkgs, flake-utils, zig }@inputs:
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

          inherit (pkgs) elixir cargo rust;
        };
      }
    );
}
