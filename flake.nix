{
  description = "Chris Barrett's Emacs configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay/be61e5636";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, emacs-overlay, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        emacsPkg = pkgs.callPackage ./emacs-with-config.nix {
          emacs = emacs-overlay.packages.${system}.emacsNativeComp;
        };
      in
      rec {
        packages.default = emacsPkg;

        apps.default = flake-utils.lib.mkApp {
          drv = emacsPkg;
          exePath = "/bin/emacs";
        };

        devShell = pkgs.mkShell {
          buildInputs = [ emacsPkg pkgs.gnumake ];
        };
      });
}
