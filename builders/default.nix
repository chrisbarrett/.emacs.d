{ pkgs, emacs }:
rec {
  emacsmirror = args: github (args // { owner = "emacsmirror"; });

  github =
    { name
    , repo ? name
    , rev
    , owner
    , sha256
    , buildInputs ? [ ]
    , patches ? [ ]
    , preBuild ? ""
    }:
    pkgs.callPackage ./lisp-package.nix {
      inherit emacs name buildInputs patches preBuild;
      src = pkgs.fetchFromGitHub { inherit sha256 repo rev owner; };
    };
}
