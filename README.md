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
	0.14    85744   3.6k/2.4k/6			428/300/2


## Building

fLisp should be buildable with only the standard C libraries.

    make flisp

# TODO

## Future

- Implement the backtick and comma reader macros. See [5]
- Adapt build system to be able to un/install `flisp` binary. Includes  
  preparing a Lisp library and a startup file.
- Extend file extension to be usable for Lisp programs.
- Refactor complete core to be able to run any number of interpreters.
- Tap the potential of the in code documentation via Doxygen.


# References

- [1] Emacs Lisp - https://www.gnu.org/software/emacs/manual/html_mono/elisp.html
- [2] Common Lisp - https://www.lispworks.com/documentation/HyperSpec/Front/
- [32] let - https://blog.veitheller.de/Scheme_Macros_III:_Defining_let.html
- [4] curry - https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Towards_a_Standard_Library
- [5] mal - quasiquote https://github.com/kanaka/mal
- [6] Scheme  https://www.scheme.org/
- [7] Scheme v7 Standard https://standards.scheme.org/official/r7rs.pdf
- [8] TSPL2d https://www.scheme.com/tspl2d/
