## Prerequisites

This version is known to compile with:

 - Coq 8.7.1
 - Ssreflect 1.6.4
 - Development version of [std++](https://gitlab.mpi-sws.org/robbertkrebbers/coq-stdpp)

The easiest way to install the correct versions of the dependencies is
through the [OCaml Package Manager](https://opam.ocaml.org/) (OPAM).
You will need the Coq and Iris OPAM repositories:

	opam repo add coq-released https://coq.inria.fr/opam/released
	opam repo add iris-dev https://gitlab.mpi-sws.org/FP/opam-dev.git

Once you set up OPAM, run `make build-dep` to install the right
versions of the dependencies.

## Updating

After doing `git pull`, the development may fail to compile because of
outdated dependencies. To fix that, please run `opam update` followed
by `make build-dep`.

## Build Instructions

Run `make -jN` to build the full development, where `N` is the number
of CPU cores you have.
