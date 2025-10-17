(defpackage :normal
  (:use :cl)
  (:export #:defmodel
           #:base-model
           #:created-at
           #:updated-at
           #:pk
           #:create-normal-object
           #:get-normal-object
           #:all-normal-objects
           #:save-normal-object
           #:save-normal-objects
           #:no-normal-objects
           #:filter-normal-objects
           #:delete-normal-object
           #:bulk-delete-normal-objects))

(in-package :normal)

(defclass base-model ()
  ((pk         :initform 0   :accessor pk :type integer)
   (created-at :initform nil :accessor created-at :type (or local-time:timestamp null))
   (updated-at :initform nil :accessor updated-at :type (or local-time:timestamp null)))
  (:documentation "Base class for all models"))

(define-condition multiple-objects-returned (error)
  ((model :initarg :model :initform nil :reader model)))

(define-condition no-objects-returned (error)
  ((model :initarg :model :initform nil :reader model)))

;; generic functions operate on model instances/classes directly
(defgeneric create-migrate-normal-object (class &rest initargs)
  (:documentation "Create a model migrations"))

(defgeneric perform-migrate-normal-object (class &rest initargs)
  (:documentation "Perform a model migrations"))

(defgeneric create-normal-object (class &rest initargs)
  (:documentation "Create an instance of CLASS with INITARGS."))

(defgeneric get-normal-object (class &rest conditions)
  (:documentation "Get a single object."))

(defgeneric get-or-create-normal-object (class &rest conditions)
  (:documentation "Get/create a single object."))

(defgeneric save-normal-object (class)
  (:documentation "Save a single object."))

(defgeneric save-normal-objects (class)
  (:documentation "Save a group of objects."))

(defgeneric all-normal-objects (class)
  (:documentation "All objects."))

(defgeneric no-normal-objects (class)
  (:documentation "Return NIL to represent no objects."))

(defgeneric filter-normal-objects (class &rest conditions)
  (:documentation "Filter objects."))

(defgeneric delete-normal-object (obj &rest conditions)
  (:documentation "Delete object."))

(defgeneric bulk-delete-normal-objects (class &rest conditions)
  (:documentation "Delete objects."))

(defmacro defmodel (name supers &rest slots)
  `(progn
     (defclass ,name ,supers
       ,@slots)

     ;; default methods for this model
     (defmethod create-normal-object ((class (eql ',name)) &rest initargs)
       (apply #'make-instance class initargs))

    (defmethod get-normal-object ((class (eql ',name)) &rest conditions)
        (let* ((table (string-downcase (symbol-name ',name)))
               (clauses (loop for (k v) on conditions by #'cddr collect (format nil "~(~a~) = ~a" k v))))
            (if clauses
                (format nil "SELECT * FROM ~A WHERE ~{~A~^ AND ~}" table clauses)
                (format nil "SELECT * FROM ~A" table))))

     (defmethod get-or-create-normal-object ((class (eql ',name)) &rest conditions)
       (format nil "Get/create NORMAL: '~A' (~A)~%" class conditions))

     (defmethod all-normal-objects ((class (eql ',name)))
        (sxql:yield (sxql:select :* (sxql:from ,(intern (symbol-name name) :keyword)))))

     (defmethod save-normal-object ((class (eql ',name)))
       (format nil "Save NORMAL: '~A'~%" class))

     (defmethod save-normal-objects ((class (eql ',name)))
       (format nil "Save many NORMAL: '~A'~%" class))

     (defmethod no-normal-objects ((class (eql ',name)))
       nil)

     (defmethod filter-normal-objects ((class (eql ',name)) &rest conditions)
       (format nil "Filter NORMAL: '~A' with ~A" class conditions))

     (defmethod delete-normal-objects ((class (eql ',name)) &rest conditions)
       (format nil "Delete single NORMAL instance of ~A (~A)" class conditions))

     (defmethod bulk-delete-normal-objects ((class (eql ',name)) &rest conditions)
       (format nil "Delete many NORMAL: '~A' with ~A" class conditions))

     ;; dispatcher function
     (defun ,name (&rest args)
       (destructuring-bind (op &rest params) args
         (ecase op
           (:create        (apply #'create-normal-object ',name params))
           (:get           (apply #'get-normal-object ',name params))
           (:get-or-create (apply #'get-normal-object ',name params))
           (:all           (all-normal-objects ',name))
           (:none          (no-normal-objects ',name))
           (:filter        (apply #'filter-normal-objects ',name params))
           (:delete        (apply #'delete-normal-object params))
           (:bulk-delete   (apply #'bulk-delete-normal-objects ',name params)))))))
