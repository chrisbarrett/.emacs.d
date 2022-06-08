self: super:
let nodepkgs = super.pkgs.callPackage ./node { };
in
{
  eslint = nodepkgs.eslint;
  flow = nodepkgs.flow-bin;
  prettier = nodepkgs.prettier;
  typescript = nodepkgs.typescript;
}
