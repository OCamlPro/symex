(lang dune 3.7)

(map_workspace_root false)

(using mdx 0.2)

(generate_opam_files true)

(name symex)

(license AGPL-3.0-or-later)

(authors
 "redianthus <leo@ocaml.pro>"
 "Pierre Chambart <pierre.chambart@ocamlpro.com>"
 "Filipe Marques <filipe.s.marques@tecnico.ulisboa.pt>"
 "Arthur Carcano <arthur.carcano@ocamlpro.com>"
 "Basile Clément <basile.clement@ocamlpro.com")

(maintainers "redianthus <leo@ocaml.pro>")

(source
 (github ocamlpro/symex))

(package
 (name symex)
 (synopsis "Primitives to write symbolic execution engines")
 (description
  "Symex is a library providing efficient primitives to write symbolic execution engines. It provides an efficient path condition and a monad for exploring paths.")
 (tags
  (symbolic execution path condition monad solver))
 (depends
  ;; build
  (ocaml
   (>= 5.3))
  ;; runtime
  fmt
  (prelude
   (>= 0.5))
  (smtml
   (>= 0.20.0))
  ;; doc
  (odoc
   (and
    (>= 3.0.0)
    :with-doc))
  ;; test
  (mdx
   (and
    :with-test
    (>= 2.1)))
  ;; dev
  (bisect_ppx
   (and
    (>= 2.5)
    :with-dev-setup))
  (landmarks-ppx :with-dev-setup)
  (ocamlformat
   (and
    (>= 0.28)
    :with-dev-setup))
  (ocb
   (and
    (>= 0.1)
    :with-dev-setup))))
