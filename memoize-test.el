;;; Tests for memoize.el -*- lexical-binding: t; -*-

(require 'ert)

(defvar numcalls 0)

(ert-deftest memoize ()
  ;; Have to defun each time since we don't want to keep re-memoizing
  ;; the same function.
  (defun testfunc (a)
    (incf numcalls))
  (setq numcalls 0)
  (let ((run-at-time-timeout)
        (run-at-time-func)
        (timer-canceled))
    (flet ((run-at-time (timeout repeat func)
                        (setq run-at-time-timeout timeout)
                        (should (null repeat))
                        (setq run-at-time-func func))
           (cancel-timer (timer)
                         (setq timer-canceled t)))
      (memoize #'testfunc "10 seconds")
      (should (eq 0 numcalls))
      (testfunc 1)
      (should run-at-time-func)
      (should (eq 1 numcalls))
      ;; Timer should be called now
      (should (equal "10 seconds" run-at-time-timeout))
      (testfunc 1)
      ;; This should be cached
      (should (eq 1 numcalls))
      (funcall run-at-time-func)
      ;; Now the cache should be gone
      (testfunc 1)
      (message "Finished running testfunc")
      (should (eq 2 numcalls))
      ;; Another arg is another call
      (testfunc 2)
      (should (eq 3 numcalls)))))


(ert-deftest memoize-by-buffer-contents ()
  (defun testfunc (arg1 arg2) (incf numcalls))
  (let ((f (memoize-by-buffer-contents--wrap #'testfunc)))
    (setq numcalls 0)
    (with-temp-buffer
      (funcall f 0 0)
      (should (eq  1 numcalls))
      (funcall f 0 1)
      (should (eq 2 numcalls))
      (funcall f 0 0)
      (should (eq 2 numcalls))
      (insert "hello world")
      (funcall f 0 0)
      (should (eq 3 numcalls)))))

