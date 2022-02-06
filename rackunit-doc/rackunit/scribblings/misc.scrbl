#lang scribble/doc
@(require "base.rkt")

@title{Miscellaneous Utilities}

The @racket[require/expose] macro allows you to access
bindings that a module does not provide.  It is useful for
testing the private functions of modules.

@defform[(require/expose module (id ...))]{

Requires @racket[id] from @racket[module] into the current module.  It
doesn't matter if the source module provides the bindings or not;
@racket[require/expose] can still get at them.

Note that @racket[require/expose] can be a bit fragile,
especially when mixed with compiled code.  Use at your own risk!
}

This example gets @racket[make-failure-test], which is defined in a RackUnit test:

@racketblock[
(require/expose rackunit/private/check-test (make-failure-test))
]

@defproc[(dynamic-require/expose [mod (or/c module-path?
                                            module-path-index?
                                            resolved-module-path?)]
                                 [name symbol?])
         any]{

Like @racket[dynamic-require], but gets internal bindings like
@racket[require/expose].
}

Checks defined with @racket[define-check] provide a
compile-time API to access information associated
with the check.

@defproc[(check-transformer? [v any/c]) boolean?]{
Determines if @racket[_v] is a syntax transformer
defined with @racket[define-check]. Typically, this
is used on the result of @racket[syntax-local-value].

Provided by @racketmodname[rackunit] at phase 1.
}

@defproc[(check-transformer-impl-name [ct check-transformer?]) identifier?]{
Given a transformer @racket[_ct] defined with @racket[define-check],
produces an identifier which names the procedure implementing the
check. This procedure takes the same arguments as the check form, as
well as two mandatory keyword arguments: @racket[#:location] whose argument
must be a list representing a source location as in the third argument of
@racket[datum->syntax], and @racket[#:exp], whose argument is an s-expression
representing the original syntax of the check for printing.

Provided by @racketmodname[rackunit] at phase 1.
}
