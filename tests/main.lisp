(defpackage normal/tests/main
  (:use :cl
        :normal
        :rove))
(in-package :normal/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :normal)' in your Lisp.

(defmodel user (base-model)
    ((name :accessor name :initarg :name)
     (age :accessor age :initarg :age)))

(deftest test-target-1
  (testing "should (= 1 1) to be true"
  (format t "Testing~%")
    (ok (= 1 1))))

(deftest test-no-objects-1
  (testing "No objects returned with :none"
    (format t "Testing~%")
    (ok (eq nil (user :none)))))

(deftest test-name-1
  (testing "Smoke test for name"
    (format t "Testing~%")
    (let ((u (user :create :name "Bob" :age 24)))
      (ok (string= (name u) "Bob"))
      (ok (= (age u) 24)))))
