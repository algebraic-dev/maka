{
  description = "Maka";

  inputs = {
    nixpkgs.url     = "github:NixOS/nixpkgs/nixos-unstable";
    utils.url       = "github:numtide/flake-utils";
    idris2-pkgs.url = "github:claymager/idris2-pkgs";
  };

  outputs = { self, nixpkgs, utils, idris2-pkgs }:
    utils.lib.eachDefaultSystem (system:
    let pkgs = import nixpkgs { inherit system; overlays = [ idris2-pkgs.overlay ]; };
        build = pkgs.callPackage ./default.nix { inherit pkgs project self packages; };
        project  = "maka"; 
        packages = with pkgs; [ 
          idris2.withLibs.sop.elab-util
          nodejs 
        ]; 
    in { 
      defaultPackage = build.js;
      devShell = with pkgs; mkShell {
        buildInputs = with pkgs.idris2-pkgs; 
          packages ++ [ lsp.withLibs.sop.elab-util pkgs.rlwrap ];
      };
    });
}
