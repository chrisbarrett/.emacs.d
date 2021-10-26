{ stdenv, fetchurl, writeShellScript, unzip }:

stdenv.mkDerivation rec {
  name = "vscode-eslint-${version}";
  version = "2.1.8";
  buildInputs = [ unzip ];
  src = fetchurl rec {
    url =
      "https://github.com/microsoft/vscode-eslint/releases/download/release%2F${version}/dbaeumer.vscode-eslint-${version}.vsix";
    sha256 = "18yw1c2yylwbvg5cfqfw8h1r2nk9vlixh0im2px8lr7lw0airl28";
  };
  phases = [ "installPhase" ];
  installPhase = ''
    mkdir -p $out/lib
    unzip $src
    cp extension/server/out/eslintServer.js "$out/lib/eslintServer.js"
  '';
}
