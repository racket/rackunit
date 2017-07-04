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

(require rackunit
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

  (test-case "check-actual? and check-expected? work"
    (check-true (check-actual? (make-check-actual 1)))
    (check-true (check-expected? (make-check-expected 1)))
    (check-false (check-expected? (make-check-actual 1)))
    (check-false (check-expected? (make-check-actual 1))))

  (test-case "make-check-actual and make-check-expected store param (prettified)"
    (check-equal? (check-info-value (make-check-actual 1)) (pretty-info 1))
    (check-equal? (check-info-value (make-check-expected 2)) (pretty-info 2)))

  (test-case "define-check adds certain infos automatically in a specific order"
    (define current-info-box (make-parameter #f))
    (define-check (check-foo arg1 arg2 arg3)
      (set-box! (current-info-box) (current-check-info)))
    (define (get-foo-info-names)
      (parameterize ([current-info-box (box 'uninitialized)])
        (check-foo 'arg1 'arg2 'arg3)
        (map check-info-name (unbox (current-info-box)))))
    (define expected-info-names
      (list 'name 'location 'expression 'params))
    (check-equal? (get-foo-info-names) expected-info-names))

  (test-case "define-check infos are added before custom infos"
    (define current-info-box (make-parameter #f))
    (define-check (check-foo/custom-info arg1 arg2 arg3)
      (with-check-info (['custom1 'foo] ['custom2 'bar])
        (set-box! (current-info-box) (current-check-info))))
    (define (get-foo-info-names)
      (parameterize ([current-info-box (box 'uninitialized)])
        (check-foo/custom-info 'arg1 'arg2 'arg3)
        (map check-info-name (unbox (current-info-box)))))
    (define expected-info-names
      (list 'name 'location 'expression 'params 'custom1 'custom2))
    (check-equal? (get-foo-info-names) expected-info-names))

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
