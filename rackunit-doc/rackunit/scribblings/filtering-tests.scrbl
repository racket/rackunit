#lang scribble/manual

@(require (for-label racket/base syntax/srcloc)
          scribble/example
          racket/sandbox)

@(define e (make-base-eval '(require rackunit)))

@title[#:tag "filtering-tests"]{Filtering Tests with Command-line Arguments}

RackUnit supports test filtering so that one may run one test or a handful of
tests out of a given set. This can be accomplished by using command-line
arguments. Before each check is run, RackUnit will use the value of
@racket[current-command-line-arguments] to construct a list of names, files,
and lines to run.

@examples[#:eval e
          (parameterize ([current-command-line-arguments (vector "foo")])
            (with-check-info (['name 'foo])
              (check-equal? 1 2))
            (check-equal? 2 3))]

In the above example, the former test runs because its @racket['name] field
matches with the value specified on the command line. The latter test is
skipped.

Multiple names can also be specified. Names are treated as regular expressions,
and tests will be run if they match any of the names specified on the command
line.

@examples[#:eval e
          (parameterize ([current-command-line-arguments (vector "foo" "bar")])
            (with-check-info (['name 'foo])
              (check-equal? 1 2))
            (with-check-info (['name 'bar])
              (check-equal? 2 3)))]

Filtering by file or line number works similarly, and uses the syntax
file:@italic{<file>} or line:@italic{<line>}.

@examples[#:eval e
          (parameterize ([current-command-line-arguments (vector "line:2")])
            (check-equal? 1 2))]

@examples[#:eval e
          (parameterize ([current-command-line-arguments (vector "line:1")])
            (check-equal? 2 3))]

Name, file, and line specifiers can be combined to create more specific filters.

@racketblock[(define loc (list (string->path "baz.rkt") 5 0 #f #f))
          (parameterize ([current-command-line-arguments (vector "foo" "file:bar.rkt" "line:5")])
            (with-check-info (['name 'foo]
                              ['location (location-info loc)])
              (check-equal? 1 2)))]

The above example is not run because, even though the test name and line number
are correct, the file names do not match.
