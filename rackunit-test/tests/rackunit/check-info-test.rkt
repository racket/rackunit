;;;
;;; <check-util-test.rkt> ---- Tests for check-util
;;; Time-stamp: <2009-06-11 17:03:21 noel>
;;;
;;; Copyright (C) 2003 by Noel Welsh.
;;;
;;; This file is part of RackUnit.

;;; RackUnit is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.

;;; RackUnit is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.

;;; You should have received a copy of the GNU Lesser General Public
;;; License along with RackUnit; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA

;;; Author: Noel Welsh <noelwelsh@yahoo.com>
;;
;;
;; Commentary:

#lang racket/base

(require racket/function
         rackunit
         rackunit/private/check-info
         (submod rackunit/private/check-info for-test))

(module+ test
  (test-case "with-check-info stores value in lexical order"
    (define stack
      (with-check-info (['a 1] ['b 2] ['c 3]) (current-check-info)))
    (for ([actual (in-list stack)]
          [expected (in-list (list 'a 'b 'c))])
      (check-eq? (check-info-name actual) expected)))

  (test-case "Nested uses of with-check-info store values in lexical order"
    (define stack
      (with-check-info (['a 1] ['b 2] ['c 3])
        (with-check-info (['d 4] ['e 5] ['f 6])
          (current-check-info))))
    (for ([actual (in-list stack)]
          [expected (in-list (list 'a 'b 'c 'd 'e 'f))])
      (check-eq? (check-info-name actual) expected)))

  (test-case
      "later with-check-info values override earlier values with same name"
    (define stack (with-check-info (['a 1] ['a 2]) (current-check-info)))
    (check-equal? stack (list (make-check-info 'a 2))))

  (test-case "nested uses with-check-info override outer values with same name"
    (define stack
      (with-check-info (['a 1])
        (with-check-info (['a 2])
          (current-check-info))))
    (check-equal? stack (list (make-check-info 'a 2))))

  (test-case "check-actual? and check-expected? work"
    (check-true (check-actual? (make-check-actual 1)))
    (check-true (check-expected? (make-check-expected 1)))
    (check-false (check-expected? (make-check-actual 1)))
    (check-false (check-expected? (make-check-actual 1))))

  (test-case "make-check-actual and make-check-expected store param (prettified)"
    (check-equal? (check-info-value (make-check-actual 1)) (pretty-info 1))
    (check-equal? (check-info-value (make-check-expected 2)) (pretty-info 2)))

  ;; Utilities for collecting the info present in a check
  
  (define current-info-box (make-parameter #f))

  (define-check (check-foo arg1 arg2 arg3)
    (set-box! (current-info-box) (current-check-info)))

  (define (call/info-box thnk)
    (parameterize ([current-info-box (box 'uninitialized)])
      (thnk)
      (map check-info-name (unbox (current-info-box)))))

  (test-case "define-check adds certain infos automatically in a specific order"
    (define expected-info-names (list 'name 'location 'expression 'params))
    (check-equal? (call/info-box (thunk (check-foo 'arg1 'arg2 'arg3)))
                  expected-info-names))

  (test-case "define-check infos are added before custom infos"
    (define-check (check-foo/custom-info arg1 arg2 arg3)
      (with-check-info (['custom1 'foo] ['custom2 'bar])
        (set-box! (current-info-box) (current-check-info))))
    (define expected-info-names
      (list 'name 'location 'expression 'params 'custom1 'custom2))
    (check-equal? (call/info-box
                   (thunk (check-foo/custom-info 'arg1 'arg2 'arg3)))
                  expected-info-names))

  (test-case "define-check infos are added before calling current-check-around"
    ;; The check infos added by define-check are not considered part of the
    ;; "check body": the expressions given to define-check that implement the
    ;; check. The current-check-around param is called with the check body only,
    ;; not the info-adding expressions. This lets rackunit clients use
    ;; current-check-around to automatically add infos to certain uses of checks
    ;; that appear after the default infos, or even override them while still
    ;; preserving their position in the stack (the way a nested use of
    ;; with-check-info would).
    (define (call-check-foo/extra-infos)
      (define old-around (current-check-around))
      (define (new-around chk)
        (with-check-info (['custom 'custom]) (old-around chk)))
      (parameterize ([current-check-around new-around])
        (check-foo 'arg1 'arg2 'arg3)))
    (check-equal? (call/info-box call-check-foo/extra-infos)
                  (list 'name 'location 'expression 'params 'custom)))

  (test-case "check-info-ref / check-info-contains-key"
    (define info0 (list (make-check-name 'my-name)))
    (define info1 (list (make-check-message 'my-message)))

    (parameterize ([current-check-info info0])
      (check-not-false (check-info-ref 'name))
      (check-false (check-info-ref 'message))

      (check-not-false (check-info-ref info1 'message))
      (check-false (check-info-ref info1 'name))

      (check-true (check-info-contains-key? 'name))
      (check-false (check-info-contains-key? 'message))
      (check-true (check-info-contains-key? info1 'message))
      (check-false (check-info-contains-key? info1 'name))))

  (test-case "All tests for trim-current-directory"
    (test-case "trim-current-directory leaves directories outside the current directory alone"
      (check-equal? (trim-current-directory "/foo/bar/") "/foo/bar/"))
    (test-equal?
     "trim-current-directory strips directory from files in current directory"
     (trim-current-directory
      (path->string (build-path (current-directory) "foo.rkt")))
     "foo.rkt")
    (test-equal?
     "trim-current-directory leaves subdirectories alone"
     (trim-current-directory
      (path->string (build-path (current-directory) "foo" "bar.rkt")))
     "foo/bar.rkt")))
