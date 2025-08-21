(defpackage normal
  (:use :cl)
  (:export #:main))

(in-package normal)

(defclass base-model ()
  ()
  (:documentation "Base class for all models"))

(defmacro defmodel (name supers slots &key (managers '(objects)))
  "Define a model with NAME, SUPERS, and SLOTS.
   Also defines default manager functions like NAME-OBJECTS."
  (let* ((class-name name)
         (default-supers (if (null supers) '(base-model) supers))
         (manager-symbols (mapcar (lambda (m) (intern (format nil "~A-~A" name m))) managers)))
    `(progn
       ;; Define the class
       (defclass ,class-name ,default-supers ,slots)

       ;; Define default manager(s)
       ,@(mapcar (lambda (mgr)
                   `(defun ,mgr ()
                      (format t "Called manager ~A for class ~A~%"
                              ',mgr ',class-name)))
                 manager-symbols)

       ;; Define dispatcher (keyword API)
       (defun ,class-name (keyword &rest args)
         (ecase keyword
           (:create
            (apply #'make-instance ',class-name args))

           (:all
            (funcall ,(car manager-symbols)))

           ;; extendable dispatch
           (otherwise
            (error "Unknown keyword ~S for model ~S"
                   keyword ',class-name)))))))

;;; Example usage
(defmodel user ()
  ((name :accessor name :initarg :name)
   (age  :accessor age  :initarg :age))
  :managers (objects admin-objects))

;; Create instance
(let ((u (user :create :name "Fred" :age 27)))
  (format t "Name: ~A, Age: ~A~%" (name u) (age u)))

;; Manager call
(user :all)        ;; => calls user-objects
;; (user-objects)     ;; => also directly accessible
;; (user-admin-objects) ;; => developer-defined alt manager
