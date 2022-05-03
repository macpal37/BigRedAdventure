opam init --bare -a -y
opam switch create cs3110-project ocaml-base-compiler.4.08.1
opam install -y utop odoc ounit2 qtest qcheck yojson lwt lwt_ppx menhir ansiterminal lambda-term merlin ocp-indent user-setup bisect_ppx-ocamlbuild ocaml-lsp-server ocamlformat ocamlformat-rpc imagelib
opam install -y graphics