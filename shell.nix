with (import ./nix {});

stdenv.mkDerivation {
  name = "build-shell";
  buildInputs = [
    mesa_glu
    libGL
    libGL_driver
    freeglut
    zlib
    haskell.compiler.ghc864
  ];
  LD_LIBRARY_PATH = with pkgs; "${freeglut}/lib";
}
