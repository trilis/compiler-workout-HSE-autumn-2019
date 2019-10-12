# compiler-workout

Supplementary repository for compiler course.

Prerequisites:
* opam [http://opam.ocaml.org] (recommended version >= 2.0.1)
* ocaml [http://ocaml.org] (recommended version 4.07.1; ex: $ opam switch create 4.07.01)
* gcc-multilib

Building:

* opam pin add ppxlib 0.8.0
* opam pin add GT https://github.com/kakadu/GT.git#v0.0.1
  (error 57: Synopsis and description must not be both empty --- is OK)
* opam pin add ostap https://github.com/dboulytchev/ostap.git
* To build the sources: make from the top project directory
* To test: test.sh from regression subfolder

How to submit your HW:
* fork repo
* checkout branch hw<№>
* open a pull request from hw[№] branch of your repository into corresponding hw[№] branch of this repo
* NB: pull request title has to start with [hw№] then you have to specify your name, surname (both full and in russian), university, and group
* we will take a look on your pull request if and only if travis has successfully build and test your submission (a green mark has to appear next to your pull-request title)
* your pr has to contain only necessary changes (mostly only src / *. ml files are expected to change)