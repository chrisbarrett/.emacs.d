{ stdenv, fetchzip }:

stdenv.mkDerivation rec {
  name = "terraform-ls-${version}";
  version = "0.5.4";

  src = fetchzip (if stdenv.isDarwin then {
    url =
      "https://releases.hashicorp.com/terraform-ls/${version}/terraform-ls_${version}_darwin_amd64.zip";
    sha256 = "0jx8hrnsmaalm5imq36gg6s18hhpbpm191rkribvrgkwqdy9d0z4";
  } else {
    url =
      "https://releases.hashicorp.com/terraform-ls/${version}/terraform-ls_${version}_linux_amd64.zip";
    sha256 = "1zc0dhz29pg1asw1zqig08fhy4ddjqji8kns3d5nai96qq7yy5s0";
  });

  noCheck = true;

  installPhase = ''
    mkdir -p $out/bin
    cp $src/terraform-ls $out/bin/terraform-ls
  '';

  meta = {
    description = "Official Terraform language server";
    homepage = "https://github.com/hashicorp/terraform-ls";
    platforms = stdenv.lib.platforms.all;
  };
}
