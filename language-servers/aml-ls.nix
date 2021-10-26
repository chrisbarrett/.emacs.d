{ stdenv, fetchurl }:
stdenv.mkDerivation rec {
  name = "aml-language-server-${version}";
  version = "4.2.0";
  phases = [ "installPhase" ];
  src = fetchurl {
    url =
      "https://repository-master.mulesoft.org/nexus/content/repositories/releases/org/mule/als/als-server_2.12/${version}/als-server_2.12-${version}.jar";
    sha256 = "1476csqi2hyacknmq4kmikif5swc2myi20b9f1pc2i5rz9jkjq7z";
  };
  installPhase = ''
    mkdir -p $out/lib/aml-ls
    cp $src $out/lib/aml-ls/als-server.jar
  '';
}
