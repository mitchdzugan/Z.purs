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
              pkgs.cloc
              pkgs.entr
              pkgs.esbuild
              pkgs.git
              pkgs.just
              pkgs.nodejs_26
              (pkgs.writeShellScriptBin "clocs" ''
                cd /home/dz/Projects/clm-stats.manager
                pwd
                ${pkgs.cloc}/bin/cloc 077d385c93c68b27691a4e606caa9aa1dd1ea9d8 \
                  --not-match-f='package-lock\.json$|yarn\.nix$' \
                  | tail -n +2
                echo ""
                ############################
                cd /home/dz/Projects/clm-stats.app
                pwd
                ${pkgs.cloc}/bin/cloc 8a466eddbbe81fbf285eb416101143da9fc007ed \
                  --not-match-f='package-lock\.json$|yarn\.nix$' \
                  | tail -n +2
                echo ""
                ############################
                echo "############################"
                echo ""
                cd /home/dz/Projects/Z.purs
                pwd
                ${pkgs.cloc}/bin/cloc HEAD \
                  --not-match-f='package-lock\.json$|yarn\.nix$' \
                  | tail -n +2
              '')
            ];
            shellHook = ''
              export NODE_OPTIONS="--enable-source-maps"
            '';
          };
       };
     }
  );
}
