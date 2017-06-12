#lang scribble/doc
@require["base.rkt" racket/runtime-path]
@define-runtime-path[screenshot]{rackunit-screen-shot.png}


@title[#:tag  "ui"]{User Interfaces}

RackUnit provides a textual and a graphical user interface

@section{Textual User Interface}

@defmodule[rackunit/text-ui]

The textual UI is in the @racketmodname[rackunit/text-ui] module.
It is run via the @racket[run-tests] function.

@defproc[(run-tests (test (or/c test-case? test-suite?))
                    (verbosity (symbols 'quiet 'normal 'verbose) 'normal))
         natural-number/c]{

The given @racket[test] is run and the result of running it
output to the @racket[current-output-port].  The output is
compatable with the (X)Emacs next-error command (as used,
for example, by (X)Emacs's compile function)

The optional @racket[verbosity] is one of @racket['quiet],
@racket['normal], or @racket['verbose].  Quiet output
displays only the number of successes, failures, and errors.
Normal reporting suppresses some extraneous check
information (such as the expression).  Verbose reports all
information.

@racket[run-tests] returns the number of unsuccessful tests.}


@section{Graphical User Interface}

@defmodule[rackunit/gui]

RackUnit also provides a GUI test runner, available from the
@racketmodname[rackunit/gui] module.

@defproc[(test/gui [test (or/c test-case? test-suite?)] ...
                   [#:wait? wait? boolean? #f])
         void?]{

Creates a new RackUnit GUI window and runs each @racket[test]. The
GUI is updated as tests complete.

When @racket[wait?] is true, @racket[test/gui] does not return until
the test runner window has been closed.

 Given the following program, the RackUnit GUI will look as shown below:

 @racketblock[
 #,(hash-lang) racket
 (require rackunit rackunit/gui)
 (test/gui
  (test-suite
   "all tests"
   (test-suite
    "math tests"
    (test-case "addition" (check-equal? (+ 1 1) 2))
    (test-case "subtraction" (check-equal? (- 0 0) 0))
    (test-case "multiplication" (check-equal? (* 2 2) 5)))
   (test-suite
    "string tests"
    (test-case "append" (check-equal? (string-append "a" "b") "ab"))
    (test-case "ref" (check-equal? (string-ref "abc" 1) #\b)))))]

 @image[screenshot]{Screenshot of the RackUnit
  window. It features a tree representing the nested test suites (with test
  cases as leaves) on the left pane, and information about the selected test
  failure in the right pane.}
}

@defproc[(make-gui-runner)
         (-> (or/c test-case? test-suite?) ... any)]{

Creates a new RackUnit GUI window and returns a procedure that, when
applied, runs the given tests and displays the results in the GUI.

}
