self: super:
let node = super.pkgs.callPackage ./node {};
in {
  eslint = node.eslint;
  flow = node.flow-bin;
  prettier = node.prettier;
  typescript = node.typescript;
}
