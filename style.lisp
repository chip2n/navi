(in-package #:navi/style)

(defvar *style-definitions* nil)

(defun get-style-definition (name)
  (cdr (assoc name *style-definitions*)))

(defun set-style-definition (name style)
  (setf (a:assoc-value *style-definitions* name) style))

(eval-when (:compile-toplevel :load-toplevel)
  (defun strip-docstring (body)
    (if (typep (car body) 'string)
        (cdr body)
        body)))

(defmacro define-style (name &body body)
  `(let ((current-style (get-style-definition ',name))
         (new-style (lass:compile-and-write
                     ,@(loop for form in (strip-docstring body)
                             collect `(quote ,form)))))
     (unless (string-equal current-style new-style)
       (set-style-definition ',name new-style))))

(defun compile-styles (output-path)
  (ensure-directories-exist output-path)
  (with-open-file (s output-path :direction :output :if-exists :supersede)
    (loop for (name . style) in (reverse *style-definitions*) do
      (fresh-line s)
      (princ style s))))
