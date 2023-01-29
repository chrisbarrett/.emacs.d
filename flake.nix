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

        languageServers = with pkgs; with nodePackages; [
          bash-language-server
          vscode-css-languageserver-bin
          vscode-extensions.dbaeumer.vscode-eslint
          vscode-extensions.graphql.vscode-graphql
          vscode-html-languageserver-bin
          vscode-json-languageserver
          rnix-lsp
          yaml-language-server
        ];

        extraPrograms = with pkgs; [
          (aspellWithDicts (ps: [ ps.en ]))
          delta
          multimarkdown
          nixpkgs-fmt
          ripgrep
          sqlite
        ];

        package = pkgs.callPackage ./builders {
          emacs = emacs-overlay.packages.${system}.emacsNativeComp;
          texProgram = "${pkgs.tectonic}/bin/tectonic";
          withPrograms = languageServers ++ extraPrograms;
          withLispPackages = import ./packages.nix;
        };
      in
      rec {
        packages.default = package;

        apps.default = flake-utils.lib.mkApp {
          drv = package;
          exePath = "/bin/emacs";
        };

        devShell = pkgs.mkShell {
          buildInputs = [ package pkgs.gnumake ];
        };
      });
}
