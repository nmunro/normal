(defpackage normal
  (:use :cl)
  (:export #:main))

(in-package normal)

(defclass base-model ()
  ()
  (:documentation "Base class for all models"))

(defmacro defmodel (name supers &rest slots)
  (let* ((managers-sym (intern (format nil "~A--MANAGERS" name)))
         (default-mgr-sym (intern (format nil "~A-OBJECTS" name))))
    `(progn
       ;; define the model
       (defclass ,name ,(if supers supers '(base-model)) ,slots)

       ;; define a registry of managers
       (defparameter ,managers-sym (make-hash-table))

       ;; define default manager
       (defun ,default-mgr-sym (&rest args)
         (apply #'some-default-manager-fn ',name args))

       ;; register default manager
       (setf (gethash :default ,managers-sym) #',default-mgr-sym)

       ;; "simple-name" dispatch → defaults to user-objects
       (defun ,name (op &rest args &key (using :default))
         (declare (ignore using))
         (apply (gethash using ,managers-sym) op args)))))

;;; Example usage
(defmodel user ()
  ((name :accessor name :initarg :name)
   (age  :accessor age  :initarg :age)))

(user :create :name "Alice")     ; expands to user-objects
(user :all)                      ; same
(user :all :using :admin)        ; will use user-admin-objects if defined

;; Create instance
;; (let ((u (user :create :name "Fred" :age 27)))
;;   (format t "Name: ~A, Age: ~A~%" (name u) (age u))) ;; @NOTE: This doesn't work yet because instances aren't ACTUALLY being created (yet)

;; Manager call
(user :all)        ;; => calls user-objects
(user-objects :all)        ;; => calls user-objects
(user-admin-objects :all)        ;; => calls user-objects
;; (user-objects)     ;; => also directly accessible
;; (user-admin-objects) ;; => developer-defined alt manager
