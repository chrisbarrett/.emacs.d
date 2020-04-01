{ pkgs, emacs }:
rec {
  emacsmirror = args: github (args // { owner = "emacsmirror"; });

  github = { name, repo ? name, rev, owner, sha256, buildInputs ? [], patches ? [] }:
    pkgs.callPackage ./builder.nix {
      inherit emacs;
      inherit name buildInputs patches;
      src = pkgs.fetchFromGitHub {
        inherit sha256 repo rev owner;
      };
    };
}
