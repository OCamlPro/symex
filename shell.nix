{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/master.tar.gz";
  }) {}
}:

let
  smtml = pkgs.ocamlPackages.smtml.overrideAttrs (old: {
    version = "0.31.0";
    src = pkgs.fetchFromGitHub {
      owner = "formalsec";
      repo = "smtml";
      rev = "v0.31.0";
      hash = "sha256-w/mEDIDydZ2xgTsMx/fbA+VxU8OlcAbOUMTogDQa5jo=";
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
    alcotest
  ];
  buildInputs = with pkgs.ocamlPackages; [
    fmt
    prelude
    smtml
  ];
}
