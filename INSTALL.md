# Installing and Building BigRedAdventures
## About the project
BigRedAdventures is a Creature-collecting RPG game created using OCaml, other external libraries, and some help from Java. All the graphics engine was built from the ground up using the OCaml graphics library. All images are represented as json files.

## OPAM Package Installation
The most critical package used for this project is the OCaml Graphics library, which uses XQuartz to run and display a window. Unfortunately, XQuartz can only be run in Mac OS devices. However, there is still another way to run Graphics for both Windows and Linux, but it is more complicated.

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
opam install -y utop odoc ounit2 qtest qcheck yojson lwt lwt_ppx menhir ansiterminal lambda-term merlin ocp-indent user-setup bisect_ppx-ocamlbuild ocaml-lsp-server ocamlformat ocamlformat-rpc
```
Finally install graphics
```
opam install -y graphics
```
* NOTE: will be prompted to install XQuartz, if you haven't already

###### ** Most Importantly: Make sure to restart your Mac afterwards to complete the installation

### Windows & Linux  Installation

#TODO:



## Running the Game

To run the game, simply type `make play` to begin the game.

# ENJOY :)
