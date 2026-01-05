# fLisp

fLisp is a tiny yet practical interpreter for a dialect of the Lisp
programming language. It is designed to be embeddable in applications
as extension language.

fLisp is embedded into the
(Femto)[https://github.com/jorge-leon/femto] editor.

> A designer knows he has achieved perfection not when there is
> nothing left to add, but when there is nothing left to take away.
> -- <cite>Antoine de Saint-Exupery</cite>


## Why the name fLisp

Lower case "f" stands for the femto metric prefix which represents
10^-15 — a very small number.  fLisp is meant to be very small.

Since "femtolisp" is already taken by a not so "femto" Lisp
interpreter, the choice fell on "fLisp".


## Goals of fLisp

- To be the smallest embeddable Lisp interpreter yet powerful enough
  to drive real world applications.
- To be rock stable, predictable and consistent.
- To consume as little resources as possible.
- To be easy to understand without extensive study (to encourage further
  experimentation).

Size by version:

	Version	Binary	C-Lines/sloc/Files	Lisp-Lines/sloc/Files
	0.13	85584	3.6k/2.4k/6			373/272/3
	0.14    82984   3.7k/2.4k/7			610/355/5


## Building

fLisp only depends on the standard C libraries.

The default Makefile target:

    make all

creates:
- flisp .. The flisp command line utility.
- flispd .. The command line utility with floating point (double) support.
- libflisp.a and libflispd.a .. The libraries for embedding fLisp in other applications.

    make install

Installs the command line utilities, documentation and Lisp libraries.

    make install-dev

Installs the libraries, header files and pkg-config files.

	make deb

Builds the Debian packages `flisp` and `flisp-dev`.  The first
installs the command line utilities, the Lisp libraries and the documentation, the second
only the files required for development.

# Documentation

Extensive documentation is available in the `doc` directory in POSHdoc/HTML and Markdown format.

- (Manual)[doc/flisp.html] ((Markdown)[doc/flisp.md]).
- (Development)[doc/develop.html] of and with fLisp ((Markdown)[doc/develop.md]).
- (Implementation)[doc/implementation.html] Details ((Markdown)[doc/implementation.md]).
- (History)[doc/history.html] Details ((Markdown)[doc/history.md]).
