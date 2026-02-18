{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/master.tar.gz";
  }) {}
}:

let
  smtml = pkgs.ocamlPackages.smtml.overrideAttrs (old: {
    src = pkgs.fetchFromGitHub {
      owner = "formalsec";
      repo = "smtml";
      rev = "a9dff52e7ef2215c786ee8ce2c24d716db0b5ace";
      hash = "sha256-TIOOE/bsis6oYV3Dt6TcI/r/aN3S1MQNtxDAnvBbVO0=";
    };
    doCheck = false;
  });
in

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
