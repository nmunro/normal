(defpackage :normal/conditions
  (:use :cl)
  (:export #:no-object-returned
           #:multiple-objects-returned))

(in-package :normal/conditions)

(define-condition db-error (error)
  ((model :initarg :model :initform nil :reader model)
   (query :initarg :query :initform ""  :reader query))
  (:documentation "General DB error")
  (:report (lambda (c s) (format s "DB Error: ~A" (query c)))))

(define-condition multiple-objects-returned (db-error)
  ((row-count :initarg :row-count :initform 0 :reader row-count))
  (:documentation "Multiple objects returned")
  (:report (lambda (c s) (format s "1 Object expected, ~A objects returned" (row-count c)))))

(define-condition no-object-returned (db-error)
  ()
  (:documentation "No records returned")
  (:report (lambda (c s) (format s "No records match query: ~A" (query c)))))

;; Examples
(error 'multiple-objects-returned :row-count 5)
(error 'no-object-returned :query "SELECT * FROM user WHERE id = 4")
