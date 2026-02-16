{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/master.tar.gz";
  }) {}
}:

pkgs.mkShell {
  name = "symex-dev-shell";
  dontDetectOcamlConflicts = true;
  nativeBuildInputs = with pkgs.ocamlPackages; [
    dune_3
    findlib
    alcotest
    bisect_ppx
    #landmarks
    #landmarks-ppx
    #mdx
    merlin
    ocaml
    ocamlformat
    ocp-browser
    ocp-index
    ocb
    odoc
  ];
  buildInputs = with pkgs.ocamlPackages; [
    fmt
    prelude
    smtml
  ];
}
