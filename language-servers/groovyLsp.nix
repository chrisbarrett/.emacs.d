{ stdenv, fetchzip, jdk, gradle }:
stdenv.mkDerivation rec {
  name = "groovy-lsp-jar-${version}";
  version = "01f0fe5ab49454958283057ba32353393c04e7e7";

  src = fetchzip rec {
    url =
      "https://github.com/prominic/groovy-language-server/archive/${version}.zip";
    sha256 = "0kbgzk370byr41rrirplb610bxsfqdssinw6fxqkphhzbik6vii8";
  };

  noConfigure = true;
  noCheck = true;

  buildInputs = [ jdk gradle ];

  buildPhase = ''
    ./gradlew --console=plain build
  '';

  installPhase = ''
    cp build/libs/source-all.jar $out
  '';
}
