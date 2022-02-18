self: super:
let
  withPatches = pkg: patches: pkg.overrideAttrs (attrs: { inherit patches; });
in
{ }
