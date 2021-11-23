{ pkgs, project, self, packages, ... }:

{
  js = 
    (pkgs.stdenv.mkDerivation rec {
      name = "maka.js";
      src = self;
      
      buildPhase = ''
        idris2 --build ${project}.ipkg
      '';

      installPhase = ''
        mkdir -p $out
        cp ./build/${name} $out/${name}
      '';

      buildInputs = packages;
    });
}