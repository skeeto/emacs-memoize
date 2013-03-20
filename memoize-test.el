;;; Tests for memoize.el -*- lexical-binding: t; -*-

(defun test-func (arg1 arg2) (incf called-count))

(ert-deftest memoize-by-buffer-contents ()
  (let ((f (memoize-by-buffer-contents--wrap #'test-func))
        (called-count 0))
    (with-temp-buffer
      (funcall f 0 0)
      (should (eq  1 called-count))
      (funcall f 0 1)
      (should (eq 2 called-count))
      (funcall f 0 0)
      (should (eq 2 called-count))
      (insert "hello world")
      (funcall f 0 0)
      (should (eq 3 called-count)))))
