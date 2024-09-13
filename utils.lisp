(in-package #:navi/utils)

(defun system-path (path)
  (asdf:system-relative-pathname 'personal-site path))
