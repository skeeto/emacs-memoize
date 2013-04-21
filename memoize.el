;;; memoize.el --- Memoization functions -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <mosquitopsu@gmail.com>
;; URL: https://github.com/skeeto/emacs-memoize
;; Version: 1.0.1

;;; Commentary:

;; `memoize' accepts a symbol or a function. When given a symbol, the
;; symbol's function definition is memoized and installed overtop of
;; the original function definition. When given a function, it returns
;; a memoized version of that function.

;;     (memoize 'my-expensive-function)

;; `defmemoize' defines a memoized function directly, behaving just
;; like `defun'.

;;     (defmemoize my-expensive-function (x)
;;       (if (zerop n)
;;           1
;;         (* n (my-expensive-function (1- n)))))

;; Memoizing an interactive function will render that function
;; non-interactive. It would be easy to fix this problem when it comes
;; to non-byte-compiled functions, but recovering the interactive
;; definition from a byte-compiled function is more complex than I
;; care to deal with. Besides, interactive functions are always used
;; for their side effects anyway.

;; There's no way to memoize nil returns, but why would your expensive
;; functions do all that work just to return nil? :-)

;; Memoization takes up memory, which should be freed at some point.
;; Because of this, all memoization has a timeout from when the last
;; access was. The default timeout is set by
;; `memoize-default-timeout'.  It can be overriden by using the
;; `memoize' function, but the `defmemoize' macro will always just use
;; the default timeout.

;; If you wait to byte-compile the function until *after* it is
;; memoized then the function and memoization wrapper both get
;; compiled at once, so there's no special reason to do them
;; separately. But there really isn't much advantage to compiling the
;; memoization wrapper anyway.

;;; Code:

(eval-when-compile (require 'cl))

(defvar memoize-default-timeout "2 hours"
  "The amount of time after which to remove a memoization.
This represents the time after last use of the memoization after
which the value is expired. Setting this to nil means to never
expire, which will cause a memory leak, but may be acceptable for
very careful uses.")

(defun memoize (func &optional timeout)
  "Memoize the given function. If argument is a symbol then
install the memoized function over the original function. The
TIMEOUT value, a timeout string as used by `run-at-time' will
determine when the value expires, and will apply after the last
access (unless another access happens)."
  (typecase func
    (symbol
     (put func 'function-documentation
          (concat (documentation func) " (memoized)"))
     (fset func (memoize--wrap (symbol-function func) timeout))
     func)
    (function (memoize--wrap func timeout))))

;; ID: 83bae208-da65-3e26-2ecb-4941fb310848
(defun memoize--wrap (func timeout)
  "Return the memoized version of FUNC.
TIMEOUT specifies how long the values last from last access. A
nil timeout will cause the values to never expire, which will
cause a memory leak as memoize is use, so use the nil value with
care."
  (let ((table (make-hash-table :test 'equal))
        (timeouts (make-hash-table :test 'equal)))
    (lambda (&rest args)
      (let ((value (gethash args table)))
        (unwind-protect
            (or value (puthash args (apply func args) table))
          (let ((existing-timer (gethash args timeouts))
                (timeout-to-use (or timeout memoize-default-timeout)))
            (when existing-timer
              (cancel-timer existing-timer))
            (when timeout-to-use
              (puthash args
                       (run-at-time timeout-to-use nil
                                    (lambda ()
                                      (remhash args table))) timeouts))))))))

(defmacro defmemoize (name arglist &rest body)
  "Create a memoize'd function. NAME, ARGLIST, DOCSTRING and BODY
have the same meaning as in `defun'."
  (declare (indent defun))
  `(progn
     (defun ,name ,arglist
       ,@body)
     (memoize (quote ,name))))

(provide 'memoize)

;;; memoize.el ends here
