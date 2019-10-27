with (import ./nix {});

stdenv.mkDerivation {
  name = "build-shell";
  buildInputs = [
    cabal-install
    mesa_glu
    # mesa_noglu.drivers
    libGL
    libGL_driver
    glxinfo
    freeglut
    zlib
    haskell.compiler.ghc865
  ];
  LD_LIBRARY_PATH = with pkgs; "${libGL}/lib:${mesa_glu}/lib:${freeglut}/lib";
}
