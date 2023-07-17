{ pkgs, emacs, ... }:
let
  inherit (pkgs.lib) strings;
in
{
  pkgsToPathString = ps:
    strings.concatStringsSep ":"
      (map (pkg: "${pkg}/bin") ps);

  fetchers = rec {
    fromGithub =
      { name
      , repo ? name
      , rev
      , owner
      , sha256
      , buildInputs ? [ ]
      , patches ? [ ]
      , preBuild ? ""
      }:
      pkgs.callPackage ./builders/lisp-package.nix {
        inherit emacs name buildInputs patches preBuild;
        src = pkgs.fetchFromGitHub { inherit sha256 repo rev owner; };
      };

    fromEmacsmirror = args: fromGithub (args // { owner = "emacsmirror"; });
  };
}
