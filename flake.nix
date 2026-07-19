{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells = {
          default = pkgs.mkShell {
            name = "purescript-custom-shell";
            buildInputs = [
              pkgs.purescript
              pkgs.nodejs_26
              pkgs.entr
              pkgs.esbuild
            ];
            shellHook = ''
              export NODE_OPTIONS="--enable-source-maps"
            '';
          };
       };
     }
  );
}
