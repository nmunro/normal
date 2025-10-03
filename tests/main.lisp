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
  (let* ((cls (find-class 'user))
         (slots (closer-mop:class-slots cls))
         (slot-names (mapcar #'closer-mop:slot-definition-name slots)))
    (ok (equal (list 'PK 'CREATED-AT 'UPDATED-AT 'NAME 'AGE) slot-names))
    (let ((pk-slot (find 'pk slots :key #'closer-mop:slot-definition-name))
          (created-at-slot (find 'created-at slots :key #'closer-mop:slot-definition-name))
          (updated-at-slot (find 'updated-at slots :key #'closer-mop:slot-definition-name))
          (name-slot (find 'name slots :key #'closer-mop:slot-definition-name))
          (age-slot (find 'age slots :key #'closer-mop:slot-definition-name)))
      (ok (equal (closer-mop:slot-definition-type pk-slot) 'integer))
      (ok (equal (closer-mop:slot-definition-type created-at-slot) '(or local-time:timestamp null)))
      (ok (equal (closer-mop:slot-definition-type updated-at-slot) '(or local-time:timestamp null)))
      (ok (equal (closer-mop:slot-definition-type name-slot) 'string))
      (ok (equal (closer-mop:slot-definition-type age-slot) 'integer)))))


(deftest test-instance-properties
  (testing "Smoke test for instance properties"
    (let ((u (user :create :name "Bob" :age 24)))
      (ok (equal (list 'PK 'CREATED-AT 'UPDATED-AT 'NAME 'AGE) (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots (class-of u)))))
      (ok (string= (name u) "Bob"))
      (ok (= (age u) 24))
      (ok (= (pk u) 0))
      (ok (eq (created-at u) nil))
      (ok (eq (updated-at u) nil)))))
