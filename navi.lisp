(in-package #:navi)

(defvar *output-dir* (fad:merge-pathnames-as-file (uiop:getcwd) "out/"))
(defvar *pages* (make-hash-table))
(defvar *assets* nil)
(defvar *pages-dirty-p* nil)

;; These settings are ensuring that we don't mess with spaces when displaying inlined links.
;; More info: https://github.com/ruricolist/spinneret/issues/37
(setf spinneret:*html-style* :human)
(setf spinneret:*suppress-inserted-spaces* t)
(setf spinneret:*fill-column* 999)

(defclass page ()
  ((path :initarg :path :accessor page-path)
   (body :initarg :body :accessor page-body)
   (builder :initarg :builder :accessor page-builder)))

(defmacro define-page (name (&key path) &body body)
  `(add-page ',name ,path ',body (lambda () (spinneret:with-html (:doctype) ,@body))))

(defun add-page (name path body builder)
  (setf (gethash name *pages*)
        (make-instance 'page
          :path path
          :body body
          :builder builder)))

(defmacro define-tag (name (body attrs-var &rest ll) &body tag)
  `(progn
     (spinneret:deftag ,name (,body ,attrs-var ,@ll) ,@tag)
     ;;TODO do this only if tree is actually changed
     (setf *pages-dirty-p* t)))

(defun recompile-page (page)
  (format t "Recompiling page... ~A~%" (page-path page))
  (setf (page-builder page)
        (lambda () (eval `(spinneret:with-html
                       (:doctype)
                       ,@(page-body page))))))

(defun add-asset (path)
  (pushnew path *assets*))

(defun build-pages (&optional (output-dir *output-dir*))
  (ensure-directories-exist output-dir)

  (when *pages-dirty-p*
    (loop for page being the hash-value of *pages*
          do (recompile-page page))
    (setf *pages-dirty-p* nil))

  (loop for page being the hash-value of *pages*
        for path = (fad:merge-pathnames-as-file output-dir (page-path page))
        do (format t "Writing ~A~%" path)
           (with-open-file (spinneret:*html* path :direction :output :if-exists :supersede)
             (funcall (page-builder page))))

  (navi/style:compile-styles (fad:merge-pathnames-as-file output-dir "style.css"))

  (loop for path in *assets*
        do (uiop:copy-file path (fad:merge-pathnames-as-file output-dir (path:basename path)))))

(defun start ()
  (build-pages)
  (navi/server:start *output-dir*))

(defun stop ()
  (navi/server:stop))
