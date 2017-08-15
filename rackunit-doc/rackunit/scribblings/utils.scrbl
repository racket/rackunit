#lang scribble/doc

@(require scribble/manual
          (for-label racket
                     rackunit/log
                     rackunit/docs-complete))

@title{Testing Utilities} 

@section{Checking documentation completeness}
@defmodule[rackunit/docs-complete]

@defproc[(check-docs [lib symbol?]
                     [#:skip skip 
                             (or/c regexp? 
                                   symbol?
                                   (listof (or/c regexp? symbol?))
                                   (-> symbol? any)
                                   #f)
                             #f])
         any]{
              
Checks to see if the module path named by @racket[lib] (e.g. @racket['racket/list])
has documented all of its exports and prints an error message to
@racket[(current-error-port)] if not.

If @racket[skip] is a regexp, then exporting matching that regexp
are ignored. If it is a symbol, then that export is ignored. If
it is a list of symbols and regexps, then any exporting matching any of the 
symbols or regexps are ignored. If it is a function, the function is treated
as a predicate and passed each export of the module. If @racket[skip] is
@racket[#f], no exports are skipped.
}

@section{Logging Test Results}
@defmodule[rackunit/log]

Rackunit provides a general purpose library for tracking test results
and displaying a summary message.

@defproc[(test-log! [result any/c]) void?]{
 Adds a test result to the running log. If @racket[result] is false,
 then the test is considered a failure.}

@defproc[(test-log [#:display? display? boolean? #f]
                   [#:exit? exit? boolean? #f])
         (cons/c exact-nonnegative-integer?
                 exact-nonnegative-integer?)]{
 Processes the running test log. The first integer is the failed tests, the
 second is the total tests. If @racket[display?] is true, then a message is
 displayed. If there were failures, the message is printed on
 @racket[(current-error-port)]. If @racket[exit?] is true, then if there were
 failures, calls @racket[(exit 1)].}

@defparam[test-log-enabled? enabled? boolean? #:value #t]{
 When set to @racket[#f], @racket[test-log!] is a no-op. This is useful to
 dynamically disable certain tests whose failures are expected and shouldn't be
 counted in the test log, such as when testing a custom check's failure
 behavior.
 @history[#:added "1.1"]}
