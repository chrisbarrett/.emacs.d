self: super:
{
  pkgsToPathString = pkgs:
    super.pkgs.lib.strings.concatStringsSep ":" (map (pkg: "${pkg}/bin") pkgs);

  mkLispFetcher = emacs:
    rec {
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
        super.pkgs.callPackage ./builders/lisp-package.nix {
          inherit emacs name buildInputs patches preBuild;
          src = super.pkgs.fetchFromGitHub { inherit sha256 repo rev owner; };
        };

      fromEmacsmirror = args: fromGithub (args // { owner = "emacsmirror"; });
    };
}
