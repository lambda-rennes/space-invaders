with (import ./nix {});
let srcs = import ./nix/sources.nix;
    ghcide-nix = import srcs.ghcide-nix {};
in

stdenv.mkDerivation {
  name = "build-shell";
  buildInputs = [
    cabal-install
    mesa_glu
    libGL
    libGL_driver
    glxinfo
    git
    freeglut
    zlib
    haskell.compiler.ghc865
    ghcide-nix.ghcide-ghc865
    vscode
  ];
  shellHook = ''
    export PATH="$PATH:$PWD/bin"
  '';
  # I wish we could do without this ugly hack.
  LD_LIBRARY_PATH = with pkgs; "${libGL}/lib:${mesa_glu}/lib:${freeglut}/lib";
}
