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

;; generic functions operate on model instances/classes directly
(defgeneric create-migrate-normal-object (class &rest initargs)
  (:documentation "Create a model migrations"))

(defgeneric perform-migrate-normal-object (class &rest initargs)
  (:documentation "Perform a model migrations"))

(defgeneric create-normal-object (class &rest initargs)
  (:documentation "Create an instance of CLASS with INITARGS."))

(defgeneric get-normal-object (class)
  (:documentation "Get a single object."))

(defgeneric get-or-create-normal-object (class)
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

(defgeneric delete-normal-object (obj)
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

     (defmethod get-normal-object ((class (eql ',name)))
       (format nil "Get NORMAL: '~A'~%" class))

     (defmethod get-or-create-normal-object ((class (eql ',name)))
       (format nil "Get/create NORMAL: '~A'~%" class))

     (defmethod all-normal-objects ((class (eql ',name)))
       (format nil "All NORMAL: '~A'~%" class))

     (defmethod save-normal-object ((class (eql ',name)))
       (format nil "Save NORMAL: '~A'~%" class))

     (defmethod save-normal-objects ((class (eql ',name)))
       (format nil "Save NORMAL: '~A'~%" class))

     (defmethod no-normal-objects ((class (eql ',name)))
       nil)

     (defmethod filter-normal-objects ((class (eql ',name)) &rest conditions)
       (format nil "Filter NORMAL: '~A' with ~A" class conditions))

     (defmethod delete-normal-object ((obj ,name))
       (format nil "Delete single NORMAL instance of ~A" ',name))

     (defmethod bulk-delete-normal-objects ((class (eql ',name)) &rest conditions)
       (format nil "Delete many NORMAL: '~A' with ~A" class conditions))

     ;; dispatcher function
     (defun ,name (&rest args)
       (destructuring-bind (op &rest params) args
         (ecase op
           (:create        (apply #'create-normal-object ',name params))
           (:get           (get-normal-object ',name))
           (:get-or-create (get-normal-object ',name))
           (:all           (all-normal-objects ',name))
           (:none          (no-normal-objects ',name))
           (:filter        (apply #'filter-normal-objects ',name params))
           (:delete        (apply #'delete-normal-object params))
           (:bulk-delete   (apply #'bulk-delete-normal-objects ',name params)))))))
