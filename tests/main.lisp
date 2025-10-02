(defpackage normal/tests/main
  (:use :cl
        :normal
        :rove))
(in-package :normal/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :normal)' in your Lisp.

(defmodel user (base-model)
    ((name :accessor name :initarg :name :type string)
     (age :accessor age :initarg :age :type integer)))

(deftest test-create-object
  (testing "Create an object with :create"
    (ng (eq nil (user :create :name "Bob" :age 18)))))

(deftest test-no-objects
  (testing "No objects returned with :none"
    (ok (eq nil (user :none)))))

(deftest test-class-properties
  (testing "Smoke test for class properties")
  (let ((cls (find-class 'user)))
    (ok (equal (list 'PK 'CREATED-AT 'UPDATED-AT 'NAME 'AGE) (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots cls))))
    (ok (equal (closer-mop:slot-definition-type PK) integer))))


(deftest test-instance-properties
  (testing "Smoke test for instance properties"
    (let ((u (user :create :name "Bob" :age 24)))
      (ok (equal (list 'PK 'CREATED-AT 'UPDATED-AT 'NAME 'AGE) (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots (class-of u)))))
      (ok (string= (name u) "Bob"))
      (ok (= (age u) 24))
      (ok (= (pk u) 0))
      (ok (eq (created-at u) nil))
      (ok (eq (updated-at u) nil)))))
