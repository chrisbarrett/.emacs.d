{ stdenv
, lib
, emacs
, name
, src
, buildInputs ? [ ]
, patches ? [ ]
, preBuild ? ""
, postInstall ? ""
}:

stdenv.mkDerivation {
  inherit name src patches;
  unpackCmd = ''
    test -f "${src}" && mkdir el && cp -p ${src} el/${name}
  '';
  buildInputs = [ emacs ] ++ buildInputs;
  buildPhase = ''
    ${preBuild}
    ARGS=$(find ${
      lib.concatStrings
      (builtins.map (arg: arg + "/share/emacs/site-lisp ") buildInputs)
    } \
                 -type d -exec echo -L {} \;)
    mkdir $out
    export HOME=$out
    ${emacs}/bin/emacs -Q -nw -L . $ARGS --batch -f batch-byte-compile *.el
  '';
  installPhase = ''
    mkdir -p $out/share/emacs/site-lisp
    install *.el* $out/share/emacs/site-lisp
    ${postInstall}
  '';
  meta = {
    description = "Emacs projects from the Internet that just compile .el files";
    homepage = http://www.emacswiki.org;
    platforms = lib.platforms.all;
  };
}
