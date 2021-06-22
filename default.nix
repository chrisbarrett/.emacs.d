let
  # 2020-06-22
  overlayRev = "38d2c58c70adb2f104723f23e8bc5e37dde52988";
  emacs-overlay = import (builtins.fetchTarball {
    url =
      "https://github.com/nix-community/emacs-overlay/archive/${overlayRev}.tar.gz";
  });
in { pkgs ?
  import <nixpkgs> { overlays = [ emacs-overlay (import ./overlays) ]; } }:

let
  inherit (pkgs.lib) strings;

  emacs = pkgs.emacsGcc.overrideAttrs (old: {
    patches = old.patches ++ [
      ./patches/emacs/0001-optional-org-gnus.patch
      ./patches/emacs/0002-dont-warn-on-archives.patch
      ./patches/emacs/0003-prettier-ibuffer.patch
    ];

    postInstall = ''
      ${old.postInstall}
      cp -r $src/src $out/share/emacs/src
    '';
  });

  # Additional programs to be injected into Emacs' environment.

  requiredPrograms = pkgs.symlinkJoin {
    name = "emacs-required-programs";
    paths = with pkgs; [
      (aspellWithDicts (ps: [ ps.en ]))
      multimarkdown
      nixfmt
      ripgrep
      sqlite
    ];
  };

  packages = pkgs.callPackage ./packages.nix rec {

    emacsmirror = args: github (args // { owner = "emacsmirror"; });

    github = { name, repo ? name, rev, owner, sha256, buildInputs ? [ ]
      , patches ? [ ] }:
      pkgs.callPackage ./builder.nix {
        inherit emacs name buildInputs patches;
        src = pkgs.fetchFromGitHub { inherit sha256 repo rev owner; };
      };

    withPatches = pkg: patches: pkg.overrideAttrs (attrs: { inherit patches; });
  };

  builder = pkgs.emacsPackagesNgGen emacs;

  emacsWithPackages =
    (builder.overrideScope' packages.overrides).emacsWithPackages
    packages.packages;

  customPathEntries = strings.concatStringsSep ":" [
    "${requiredPrograms}/bin"
    "${pkgs.jdk}/bin"
  ];

in pkgs.symlinkJoin {
  name = "emacs-wrapped";
  buildInputs = [ pkgs.makeWrapper ];
  paths = [ emacsWithPackages ];
  postBuild = ''
    for program in "$out/Applications/Emacs.app/Contents/MacOS/Emacs" "$out/bin/emacs"; do
      if [ -f "$program" ]; then
        wrapProgram "$program" \
          --prefix PATH ":" "${customPathEntries}" \
          --set NIX_EMACS_SRC_DIR "${emacs}/share/emacs/src/" \
          --set NIX_EMACS_TEX_PROGRAM "${pkgs.tectonic}/bin/tectonic" \
          --set NIX_EMACS_PLANTUML_JAR "${pkgs.plantuml}/lib/plantuml.jar" \
          --set JAVA_HOME "${pkgs.jdk}"
      fi
    done
  '';
}
