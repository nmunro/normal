(defpackage normal/tests/main
  (:use :cl
        :normal
        :rove))
(in-package :normal/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :normal)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
  (format t "Testing~%")
    (ok (= 1 1))))