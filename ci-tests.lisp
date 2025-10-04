;;; ci-tests.lisp - Test runner for CI

(require :asdf)

;; Ensure Quicklisp is loaded
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))

;; Load the system under test
(ql:quickload :rove)
(ql:quickload :normal)

;; Run tests, exit 0 if they pass, 1 otherwise
(handler-case
  (progn
    (asdf:test-system :normal)
    (format t "~%All tests passed.~%")
    (sb-ext:quit :unix-status 0))

  (error (c)
    (format *error-output* "~%Test failure: ~A~%" c)
    (sb-ext:quit :unix-status 1)))
