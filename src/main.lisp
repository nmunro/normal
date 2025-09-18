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

(defun manage (model &key with)
  (format t "Managing ~A with ~A~%" model with))

(defgeneric create-norma-object (manager)
  (:documentation "Create object"))

(defgeneric all-normal-objects (manager)
  (:documentation "All objects"))

(defgeneric no-normal-objects (manager)
  (:documentation "A helper method to return a usable, but empty object"))

(defgeneric filter-normal-objects (manager)
  (:documentation "Filter objects"))

(defgeneric delete-normal-object (manager)
  (:documentation "Delete object"))

(defgeneric delete-normal-objects (manager)
  (:documentation "Delete objects"))

(defmacro defmanager (name)
  `(defclass ,name (base-manager)
     ()))

(defmacro defmodel (name supers &rest slots)
    (let* ((pkg (or *package* (find-package :cl-user)))
           (manager-name (intern (string-upcase (format nil "~A-OBJECTS" name)) pkg)))
        `(progn
            (defclass ,manager-name (base-manager) ())

            (defun ,name (&rest rest)
              (format nil "~A" rest))

            (defclass ,name ,supers
                ,@slots)

            (defmethod no-normal-objects ((manager ,name))
              (format nil "none"))

            (manage ',name :with ',manager-name))))

(macroexpand-1 '(defmodel user (base-model)
               ((name :accessor name :initarg :name)
                (age :accessor age :initarg arg))))

;;; Example usage
(defmodel user (base-model)
    ((name :accessor name :initarg :name)
     (age :accessor age :initarg :age)))

(let ((u (make-instance 'user :name "Bob" :age 24)))
  (created-at u))

(let ((user (make-instance 'user :name "Fred" :age 23)))
  (no-normal-objects user))

(user :create :name "Alice" :age 23)     ; expands to user-objects
(user :all)                      ; same
;; (user :filter (:and (:> :age 18) (:<= :age 65)))
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
