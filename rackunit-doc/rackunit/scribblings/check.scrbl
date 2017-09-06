#lang scribble/doc
@(require "base.rkt")

@(require (for-label racket/match))

@(define rackunit-eval (make-base-eval))
@(interaction-eval #:eval rackunit-eval (require rackunit))
@(interaction-eval #:eval rackunit-eval (error-print-context-length 0))

@title{Checks}

Checks are the basic building block of RackUnit.  A check
checks some condition and always
evaluates to @racket[(void)].  If the condition doesn't hold, the
check will report the failure (see @racket[current-check-handler]
for customizing how failures are handled).

Although checks are implemented as macros, which is
necessary to grab source location, they are conceptually
functions (with the exception of @racket[check-match] below).
This means, for instance, checks always evaluate
their arguments.  You can use checks as first class
functions, though you will lose precision in the reported
source locations if you do so.

The following are the basic checks RackUnit provides.  You
can create your own checks using @racket[define-check].

@defproc*[([(check-eq? (v1 any) (v2 any) (message string? "")) void?]
           [(check-not-eq? (v1 any) (v2 any) (message string? "")) void?]
           [(check-eqv? (v1 any) (v2 any) (message string? "")) void?]
           [(check-not-eqv? (v1 any) (v2 any) (message string? "")) void?]
           [(check-equal? (v1 any) (v2 any) (message string? "")) void?]
           [(check-not-equal? (v1 any) (v2 any) (message string? "")) void?])]{

Checks that @racket[v1] is equal (or not equal) to @racket[v2], using
@racket[eq?], @racket[eqv?], or @racket[equal?], respectively. The
optional @racket[message] is included in the output if the check
fails.

For example, the following checks all fail:

@interaction[#:eval rackunit-eval
  (check-eq? (list 1) (list 1) "allocated data not eq?")
  (check-not-eq? 1 1 "fixnums are eq?")
  (check-eqv? 1 1.0 "not eqv?")
  (check-not-eqv? 1 1 "integers are eqv?")
  (check-equal? 1 1.0 "not equal?")
  (check-not-equal? (list 1) (list 1) "equal?")
]
}

@defproc[(check-pred (pred (-> any any)) (v any) (message string? ""))
         void?]{

Checks that @racket[pred] returns a value that is not @racket[#f] when
applied to @racket[v].  The optional @racket[message] is included in
the output if the check fails. The value returned by a successful
check is the value returned by @racket[pred].

For example, the following check passes:
@interaction[#:eval rackunit-eval
  (check-pred string? "I work")
]
The following check fails:
@interaction[#:eval rackunit-eval
  (check-pred number? "I fail")
]
}

@defproc[(check-= (v1 any) (v2 any) (epsilon number?) (message string? ""))
         void?]{

Checks that @racket[v1] and @racket[v2] are numbers within
@racket[epsilon] of one another.  The optional
@racket[message] is included in the output if the check
fails.

For example, the following check passes:

@interaction[#:eval rackunit-eval
  (check-= 1.0 1.01 0.02 "I work")
]
The following check fails:
@interaction[#:eval rackunit-eval
  (check-= 1.0 1.01 0.005 "I fail")
]
}

@defproc*[([(check-true (v any) (message string? "")) void?]
           [(check-false (v any) (message string? "")) void?]
           [(check-not-false (v any) (message string? "")) void?])]{

Checks that @racket[v] is @racket[#t], is @racket[#f], or is not
@racket[#f], respectively.  The optional @racket[message] is included
in the output if the check fails.

For example, the following checks all fail:

@interaction[#:eval rackunit-eval
  (check-true 1)
  (check-false 1)
  (check-not-false #f)
]
}

@defproc[(check-exn (exn-predicate (or/c (-> any any/c) regexp?))
                    (thunk (-> any)) (message string? ""))
         void?]{

Checks that @racket[thunk] raises an exception and that either
@racket[exn-predicate] returns a true value if it is a function, or
that it matches the message in the exception if @racket[exn-predicate]
is a regexp. In the latter case, the exception raised must be an
@racket[exn:fail?].  The optional @racket[message] is included in the
output if the check fails.  A common error is to use an expression
instead of a function of no arguments for @racket[thunk].  Remember
that checks are conceptually functions.

For example, the following checks succeed:

@interaction[#:eval rackunit-eval
  (check-exn
   exn:fail?
   (lambda ()
     (raise (make-exn:fail "Hi there"
                           (current-continuation-marks)))))
  (check-exn
   exn:fail?
   (lambda ()
     (error 'hi "there")))
]

The following check fails:

@interaction[#:eval rackunit-eval
  (check-exn exn:fail?
             (lambda ()
               (break-thread (current-thread))))
]

The following example is a common mistake. The call to @racket[error]
is not within a @racket[lambda], so it bypasses @racket[check-exn]
entirely.

@interaction[#:eval rackunit-eval
  (code:comment "Forgot to wrap the expression in a thunk.  Don't do this!")
  (check-exn exn:fail?
             (error 'hi "there"))
]
}

@defproc[(check-not-exn (thunk (-> any)) (message string? "")) void?]{

Checks that @racket[thunk] does not raise any exceptions.
The optional @racket[message] is included in the output if
the check fails.

@interaction[#:eval rackunit-eval
                    (check-not-exn (λ () 1))
                    (check-not-exn (λ () (car '())))
                    (check-not-exn (λ () (/ 1 0)) "don't divide by 0")]

}

@defproc[(check-regexp-match (regexp regexp?)
                             (string string?))
         void?]{

Checks that @racket[regexp] matches the @racket[string].


For example, the following check succeeds:

@interaction[#:eval rackunit-eval
  (check-regexp-match "a+bba" "aaaaaabba")
]

The following check fails:

@interaction[#:eval rackunit-eval
  (check-regexp-match "a+bba" "aaaabbba")
]
}

@defform*[((check-match v pattern)
           (check-match v pattern pred))]{

A check that pattern matches on the test value.  Matches the test value
@racket[v] against @racket[pattern] as a @racket[match] clause.  If no
@racket[pred] is provided, then if the match succeeds, the entire check
succeeds.  For example, this use succeeds:

@interaction[#:eval rackunit-eval
  (check-match (list 1 2 3) (list _ _ 3))
]

This check fails to match:

@interaction[#:eval rackunit-eval
  (check-match (list 1 2 3) (list _ _ 4))
]

If @racket[pred] is provided, it is evaluated with the bindings from the
match pattern.  If it produces @racket[#t], the entire check succeeds,
otherwise it fails.  For example, this use succeeds, binding @racket[x]
in the predicate:

@interaction[#:eval rackunit-eval
  (check-match (list 1 (list 3)) (list x (list _)) (odd? x))
]

This check fails because the @racket[pred] fails:

@interaction[#:eval rackunit-eval
  (check-match 6 x (odd? x))
]

This check fails because of a failure to match:

@interaction[#:eval rackunit-eval
  (check-match (list 1 2) (list x) (odd? x))
]

}


@defproc[(check (op (-> any any any))
                (v1 any)
                (v2 any)
                (message string? ""))
         void?]{

The most generic check.  Succeeds if @racket[op] applied to
@racket[v1] and @racket[v2] is not @racket[#f], otherwise raises an
exception of type @racket[exn:test:check].  The optional
@racket[message] is included in the output if the check fails.

For example, the following check succeeds:

@interaction[#:eval rackunit-eval
  (check < 2 3)
]

The following check fails:

@interaction[#:eval rackunit-eval
  (check memq 'pine '(apple orange pear))
]
}

@defproc[(fail (message string? ""))
         void?]{

This check fails unconditionally.  Good for creating test stubs that
you intend to fill out later.  The optional @racket[message] is
included in the output.
}

@section{Augmenting Information on Check Failure}

When a check fails it stores @tech[#:key "check-info"]{information} including the name
of the check, the location and message (if available), the
expression the check is called with, and the parameters to
the check.  Additional information can be @tech[#:key "check-info stack"]{stored} by using
the @racket[with-check-info*] function, and the
@racket[with-check-info] macro.

@defstruct[check-info ([name symbol?] [value any]) #:transparent]{
 A @deftech[#:key "check-info"]{check-info structure} stores information
 associated with the context of the execution of a check. The @racket[value]
 is normally written in a check failure message using @racket[write], but the
 @racketmodname[rackunit] library provides several special formatting wrappers
 that can influence how the check info value is printed.

 @history[#:changed "1.6" "Changed from opaque to transparent"]}

@defstruct*[string-info ([value string?]) #:transparent]{
 A special wrapper around a string for use as a @tech{check-info} value. When
 displayed in a check failure message, @racket[value] is displayed without
 quotes. Used to print messages instead of writing values.
 
 @(interaction
   #:eval rackunit-eval
   (define-check (string-info-check)
     (with-check-info (['value "hello world"]
                       ['message (string-info "hello world")])
       (fail-check)))
   (string-info-check))

 @history[#:added "1.2"]}

@defstruct*[nested-info ([values (listof check-info?)]) #:transparent]{
 A special wrapper around a list of @tech{check-infos} for use as a
 @racket[check-info] value. A check info whose value is a nested info is
 displayed as an indented subsequence of infos. Nested infos can be placed
 inside nested infos, yielding greater indentation.
 
 @(interaction
   #:eval rackunit-eval
   (define-check (nested-info-check)
     (define infos
       (list (make-check-info 'foo "foo") (make-check-info 'bar "bar")))
     (with-check-info (['nested (nested-info infos)]) (fail-check)))
   (nested-info-check))
 
 @history[#:added "1.7"]}

@defstruct*[dynamic-info ([proc (-> any/c)]) #:transparent]{
 A special wrapper around a procedure that produces a value for a
 @tech{check-info}. When a @racket[dynamic-info] is displayed in a check info
 stack, @racket[proc] is called to determine what value to display.

 @(interaction
   #:eval rackunit-eval
   (with-check-info (['current-dir (dynamic-info current-directory)])
     (check-equal? 1 2)
     (parameterize ([current-directory (find-system-path 'temp-dir)])
       (check-equal? 1 2))))

 The value returned by @racket[proc] may itself be a special formatting value
 such as @racket[nested-info] (or even another @racket[dynamic-info]), in which
 case that value is rendered as it would be if it had not been wrapped in
 @racket[dynamic-info].

 @(interaction
   #:eval rackunit-eval
   (define current-foo (make-parameter #f))
   (with-check-info (['foo (dynamic-info current-foo)])
     (check-equal? 1 2)
     (parameterize ([current-foo
                     (nested-info (list (make-check-info 'nested 'foo)))])
       (check-equal? 1 2))))

 @history[#:added "1.9"]}

The are several predefined functions that create @tech{check-info}
structures with predefined names.  This avoids
misspelling errors:

@defproc*[([(make-check-name (name string?)) check-info?]
           [(make-check-params (params (listof any))) check-info?]
           [(make-check-location (loc (list/c any (or/c number? #f) (or/c number? #f)
                                                  (or/c number? #f) (or/c number? #f))))
            check-info?]
           [(make-check-expression (msg any)) check-info?]
           [(make-check-message (msg string?)) check-info?]
           [(make-check-actual (param any)) check-info?]
           [(make-check-expected (param any)) check-info?])]{}

@defproc[(with-check-info* (info (listof check-info?)) (thunk (-> any))) any]{

Pushes the given @racket[info] on the @tech{check-info stack} for
the duration (the dynamic extent) of the execution of
@racket[thunk]}

@interaction[#:eval rackunit-eval
  (with-check-info*
   (list (make-check-info 'time (current-seconds)))
   (lambda () (check = 1 2)))
]

When this check fails the message

@verbatim{time: <current-seconds-at-time-of-running-check>}

is printed along with the usual information on an check failure.

@defform[(with-check-info ((name val) ...) body ...)]{

The @racket[with-check-info] macro pushes the given
information onto the @tech{check-info stack} for the duration
of the execution of the body expressions. Each @racket[name] must be
a quoted symbol and each @racket[val] must be a value.}

@interaction[#:eval rackunit-eval
 (for-each
  (lambda (elt)
    (with-check-info
     (('current-element elt))
     (check-pred odd? elt)))
  (list 1 3 5 7 8))
]

When this test fails the message

@verbatim{current-element: 8}

is displayed along with the usual information on an check failure.



@section{Custom Checks}

Custom checks can be defined using @racket[define-check] and
its variants.  To effectively use these macros it is useful
to understand a few details about a check's evaluation
model.

Firstly, a check should be considered a function, even
though most uses are actually macros.  In particular, checks
always evaluate their arguments exactly once before
executing any expressions in the body of the checks.  Hence
if you wish to write checks that evaluate user defined code
that code must be wrapped in a thunk (a function of no
arguments) by the user.  The predefined @racket[check-exn]
is an example of this type of check.

It is also useful to understand how the @deftech{check-info stack}
operates.  The stack contains a list of @tech{check-info} structures;
when a check fails, RackUnit interprets these structures to print an error
message. The @tech{check-info stack} is stored in a parameter and the
@racket[with-check-info] forms evaluate to
@racket[parameterize] forms.  For this reason simple checks (see below)
cannot usefully contain calls to @racket[with-check-info] to report
additional information.  All checks created using
@racket[define-simple-check] or @racket[define-check] grab some
information by default, for example the name of the checks and the values of the
parameters.  Additionally, the macro forms of checks grab location
information and the expressions passed as parameters.

@defform[(define-simple-check (name param ...) body ...)]{

The @racket[define-simple-check] macro constructs a check
called @racket[name] that takes the params and an optional
message as arguments and evaluates the @racket[body]s.  The
check fails if the result of the last @racket[body] is
@racket[#f].  Otherwise the check succeeds.  Note that
simple checks cannot report extra information using
@racket[with-check-info].}

For example, the following code defines a check @racket[check-odd?]

@interaction[#:eval rackunit-eval
  (define-simple-check (check-odd? number)
    (odd? number))
]

We can use these checks in the usual way:

@interaction[#:eval rackunit-eval
  (check-odd? 3)
  (check-odd? 2)
]

@defform*[[(define-binary-check (name pred actual expected))
          (define-binary-check (name actual expected) body ...)]]{

The @racket[define-binary-check] macro constructs a check
that tests a binary predicate.  Compared to
@racket[define-simple-check], this macro does a better job reporting check
failures.  The first form of @racket[define-binary-check] accepts a binary
predicate and tests if the predicate holds for the given
values.  The second form tests if the last @racket[body]
evaluates to a non-false value.
}

Here's the first form, where we use a predefined predicate
to construct a binary check:

@interaction[#:eval rackunit-eval
  (define-binary-check (check-char=? char=? actual expected))
]

In use:

@interaction[#:eval rackunit-eval
  (check-char=? (read-char (open-input-string "a")) #\a)
]

If the expression is more complicated, the second form should
be used.  For example, below we define a binary check that
tests whether a number is within 0.01 of the expected value:

@interaction[#:eval rackunit-eval
  (define-binary-check (check-in-tolerance actual expected)
    (< (abs (- actual expected)) 0.01))
]

@defform[(define-check (name param ...) body ...)]{

The @racket[define-check] macro acts in exactly the same way
as @racket[define-simple-check], except the check only fails
if @racket[fail-check] is called in the body of the check.
This allows more flexible checks, and in particular more flexible
reporting options.}

@defproc[(fail-check [message string?]) void?]{
Raises an @racket[exn:test:check] with the contents of the @tech{check-info stack}.
The optional message is used as the exception's
message.}

@close-eval[rackunit-eval]
