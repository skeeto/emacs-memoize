;;; Tests for memoize.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'memoize)

(defvar memoize-numcalls 0)

(defun memoize-testfunc1 (_)
    (cl-incf memoize-numcalls))

(ert-deftest memoize ()
  ;; Have to defun each time since we don't want to keep re-memoizing
  ;; the same function.

  (setq memoize-numcalls 0)
  (let ((run-at-time-timeout)
        (run-at-time-func)
        (timer-canceled))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (timeout repeat func)
                 (setq run-at-time-timeout timeout)
                 (should (null repeat))
                 (setq run-at-time-func func)))
              ((symbol-function 'cancel-timer)
               (lambda (_)
                 (setq timer-canceled t))))
      (memoize #'memoize-testfunc1 "10 seconds")
      (should (eq 0 memoize-numcalls))
      (memoize-testfunc1 1)
      (should run-at-time-func)
      (should (eq 1 memoize-numcalls))
      ;; Timer should be called now
      (should (equal "10 seconds" run-at-time-timeout))
      (memoize-testfunc1 1)
      ;; This should be cached
      (should (eq 1 memoize-numcalls))
      (funcall run-at-time-func)
      ;; Now the cache should be gone
      (memoize-testfunc1 1)
      (message "Finished running memoize-testfunc1")
      (should (eq 2 memoize-numcalls))
      ;; Another arg is another call
      (memoize-testfunc1 2)
      (should (eq 3 memoize-numcalls))
      (should timer-canceled))))

(defun memoize-testfunc2 (_a _b)
    (cl-incf memoize-numcalls))

(ert-deftest memoize-by-buffer-contents ()
  (let ((f (memoize-by-buffer-contents--wrap #'memoize-testfunc2)))
    (setq memoize-numcalls 0)
    (with-temp-buffer
      (funcall f 0 0)
      (should (eq  1 memoize-numcalls))
      (funcall f 0 1)
      (should (eq 2 memoize-numcalls))
      (funcall f 0 0)
      (should (eq 2 memoize-numcalls))
      (insert "hello world")
      (funcall f 0 0)
      (should (eq 3 memoize-numcalls)))))
