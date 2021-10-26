{ stdenv, fetchzip, jdk11, gradle }:
stdenv.mkDerivation rec {
  name = "groovy-lsp-jar-${version}";
  version = "2f67aaad62bec607d3bfdb7e4341f9a0afece51a";

  src = fetchzip rec {
    url =
      "https://github.com/GroovyLanguageServer/groovy-language-server/archive/${version}.zip";
    sha256 = "04m6gr7l0iahzzgrqqmbvrkl930r29csvrs7a6y967s3zh85p9vp";
  };

  noConfigure = true;
  noCheck = true;

  buildInputs = [ jdk11 gradle ];

  buildPhase = ''
    ./gradlew --console=plain build
  '';

  installPhase = ''
    mkdir -p $out/lib/groovy-ls
    cp build/libs/source-all.jar $out/lib/groovy-ls/groovy-ls.jar
  '';
}
