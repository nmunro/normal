(defpackage normal
  (:use :cl))

(in-package normal)

(defclass base-model ()
  ((pk         :initform 0   :accessor pk)
   (created-at :initform nil :accessor created-at)
   (updated-at :initform nil :accessor updated-at))
  (:documentation "Base class for all models"))

(defclass base-manager ()
  ()
  (:documentation "Base class for all managers"))

(defgeneric create-object (manager)
  (:documentation "Create object"))

(defgeneric all-objects (manager)
  (:documentation "All objects"))

(defgeneric no-objects (manager)
  (:documentation "A helper method to return a usable, but empty object"))

(defgeneric filter-objects (manager)
  (:documentation "Filter objects"))

(defgeneric delete-object (manager)
  (:documentation "Delete object"))

(defgeneric delete-objects (manager)
  (:documentation "Delete objects"))

(defmacro defmanager (name)
  `(defclass ,name (base-manager)
     ()))

(defmacro defmodel (name supers &rest slots)
  `(defclass ,name ,supers
     ,@slots))

(defun manage (model &key with)
  (format t "Managing ~A with ~A~%" model with))

(macroexpand-1 '(defmodel user (base-model)
               ((name :accessor name :initarg :name)
                (age :accessor age :initarg arg))))

(defmodel user (base-model)
    ((name :accessor name :initarg :name)
     (age :accessor age :initarg :age)))

(let ((u (make-instance 'user :name "Bob" :age 24)))
  (created-at u))

;;; Example usage
;; (defmodel user ()
;;   ((name :accessor name :initarg :name)
;;    (age  :accessor age  :initarg :age)))

;; (user :create :name "Alice")     ; expands to user-objects
;; (user :all)                      ; same
;; (user :using :admin :all)        ; will use user-admin-objects if defined

;; Create instance
;; (let ((u (user :create :name "Fred" :age 27)))
;;   (format t "Name: ~A, Age: ~A~%" (name u) (age u))) ;; @NOTE: This doesn't work yet because instances aren't ACTUALLY being created (yet)

;; Manager call
;; (user :all)        ;; => calls user-objects
;; (user-objects :all)        ;; => calls user-objects
;; (user-admin-objects :all)        ;; => calls user-objects
;; (user-objects)     ;; => also directly accessible
;; (user-admin-objects) ;; => developer-defined alt manager

(manage :user :with :user-admin)
