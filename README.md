# fLisp

fLisp is a tiny yet practical interpreter for a dialect of the Lisp
programming language. It is designed to be embeddable in applications
as extension language.

fLisp is embedded into the
(Femto)[https://github.com/hughbarney/femto] editor.

> A designer knows he has achieved perfection not when there is
> nothing left to add, but when there is nothing left to take away.
> -- <cite>Antoine de Saint-Exupery</cite>


## Why the name fLisp

The choice fell on "fLisp", since femtolisp is already taken (by a not
so "femto" Lisp interpreter). Lower case "f" stands for the femto
metric prefix which represents 10^-15 — a very small number.  fLisp is
meant to be very small.


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
	0.14    83088   3.7k/2.4k/7			607/354/5


## Building

fLisp only depends on the standard C libraries.

The default Makefile target:

	make all

Creates:
- flisp .. The flisp command line utility.
- flispd .. The command line utility with floating point (double) support.
- libflisp.a and libflispd.a .. The libraries for embedding fLisp in other applications.

	make install
	make install-dev

Install the command line utilities, documentation and Lisp libraries
in the first case, or the libraries, header files and pkg-config files
in the second case.

	make dep

Builds the Debian packages `flisp` and `flisp-dev`.  The first
installs the command line utilities, the Lisp libraries and the documentation, the second
only the files required for development.

# References

- [1] Emacs Lisp - https://www.gnu.org/software/emacs/manual/html_mono/elisp.html
- [2] Common Lisp - https://www.lispworks.com/documentation/HyperSpec/Front/
- [32] let - https://blog.veitheller.de/Scheme_Macros_III:_Defining_let.html
- [4] curry - https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Towards_a_Standard_Library
- [5] mal - quasiquote https://github.com/kanaka/mal
- [6] Scheme  https://www.scheme.org/
- [7] Scheme v7 Standard https://standards.scheme.org/official/r7rs.pdf
- [8] TSPL2d https://www.scheme.com/tspl2d/
