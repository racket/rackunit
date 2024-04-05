#lang scribble/doc

@(require scribble/manual
          "utils-label.rkt"
          (for-label racket
                     rackunit/log
                     rackunit/docs-complete))

@title{Testing Utilities} 

@section{Checking documentation completeness}
@defmodule[rackunit/docs-complete]

@defproc[(check-docs [lib module-path?]
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

@history[#:changed "1.10" @elem{Changed @racket[lib] to accept any module path.}]}

@section{Logging Test Results}
@defmodule[rackunit/log]

@deprecated[@racketmodname[raco/testing]]

@defproc[(test-log! [result any/c]) void?]{
  Re-exports @raco-testing[test-log!] from @racketmodname[raco/testing].}

@defproc[(test-log [#:display? display? any/c #f]
                   [#:exit? exit? any/c #f])
         (cons/c exact-nonnegative-integer?
                 exact-nonnegative-integer?)]{
 Re-exports @raco-testing[test-report] from @racketmodname[raco/testing].
 @history[#:changed "1.11" @elem{Allow any value for the @racket[display?]
                                 and @racket[exit?] arguments, not just booleans.}]}

@defboolparam[test-log-enabled? enabled? #:value #t]{
 Re-exports @raco-testing[test-log-enabled?] from @racketmodname[raco/testing].
 @history[#:added "1.1"
          #:changed "1.11" @elem{Allow any value for the parameter and coerce it to a boolean.}]}
