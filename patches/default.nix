self: super:
let
  withPatches = pkg: patches: pkg.overrideAttrs (attrs: { inherit patches; });
in
{
  markdown-mode = withPatches super.markdown-mode [ ./markdown-mode.patch ];
}
