# Installing and Building BigRedAdventures

## About the project

BigRedAdventures is a Creature-collecting RPG game created using OCaml!

## OPAM Package Installation

A critical package used for this project is the OCamlSDL2 library. However, there is still another way to run Graphics for both Windows and Linux, but it is more complicated.

### Mac Installation

```
opam init --bare -a -y
```

Create a new opam switch that uses OCaml 4.081

```
opam switch create cs3110-project ocaml-base-compiler.4.08.1
```

First install the following packages

```
opam install -y utop odoc ounit2 qtest qcheck yojson lwt lwt_ppx menhir ansiterminal lambda-term merlin ocp-indent user-setup bisect_ppx-ocamlbuild ocaml-lsp-server ocamlformat ocamlformat-rpc imagelib
```

Finally install graphics

```
opam install -y ocamlsdl2
```


### ** Most Importantly: Make sure to restart your Mac/PC afterwards to complete the installation

### Windows Installation

First, follow the instructions above in the Mac installation up to install SDL

Then run
```
sudo apt-get update
sudo apt-get install -y libsdl2-dev
opam install -y ocamlsdl2
```

Then, follow the instructions here to install X server for Windows:
<https://aalonso.dev/blog/how-to-use-gui-apps-in-wsl2-forwarding-x-server-cdj>

## Running the Game

Open the terminal and simply type `make play` to begin the game.

# ENJOY :)
