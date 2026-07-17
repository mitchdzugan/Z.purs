{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.graalvmPackages.graaljs
    pkgs.graalvmPackages.graalnodejs
    pkgs.jdk
  ];

  shellHook = ''
    echo "GraalJS environment ready"
  '';
}
