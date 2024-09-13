(in-package #:navi/org)

(defclass document ()
  ((nodes :initarg :nodes
          :accessor document-nodes)
   (properties :initarg :properties)))

;; TODO indentation level?
(defclass section-node ()
  ((headline :initarg :headline
             :accessor section-headline)
   (properties :initarg :properties)
   (child-nodes :initarg :child-nodes
                :accessor section-children)))

(defclass text-node ()
  ((text :initarg :text
         :accessor text-text)
   (style :initarg :style
          :initform nil
          :accessor text-style)))

(defclass paragraph-node ()
  ((child-nodes :initarg :child-nodes
                :accessor paragraph-nodes)))

(defclass link-node ()
  ((url :initarg :url
        :accessor link-url)
   (description :initarg :description
                :accessor link-description)))

(defclass table-node ()
  ((rows :initarg :rows
         :accessor table-rows)))

(defclass source-node ()
  ((language :initarg :language
             :accessor source-language)
   (code :initarg :code
         :accessor source-code)))

;; * Printing nodes

(defmethod print-object ((obj text-node) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (princ (text-text obj) stream)))

(defmethod print-object ((obj link-node) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (princ (link-url obj) stream)))

(defmethod print-object ((obj section-node) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (princ (section-headline obj) stream)))

;; * Properties

(defun make-property-table ()
  (make-hash-table :test 'equal))

(defun put-prop (table key value)
  (setf (gethash key table) value))

(defmethod get-prop (key node)
  ;; (when *current-document*
  ;;   (gethash key (slot-value *current-document* 'properties)))
  )

(defmethod get-prop (key (doc document))
  (or
   (gethash key (slot-value doc 'properties))
   (call-next-method)))

(defmethod get-prop (key (section section-node))
  (or
   (gethash key (slot-value section 'properties))
   (call-next-method)))

;; * Rest

(defun string-emptyp (s)
  (string-equal "" (string-trim '(#\Space #\Newline #\Tab) s)))

(defun string-indent (string)
  (loop with num = 0
        for char across string
        while (char-equal char #\Space)
        do (incf num)
        finally (return num)))

(defun strip-indent (indent s)
  (subseq s (min indent (length s))))

(defun lines (string)
  (uiop:split-string string :separator '(#\Newline)))

(defun unlines (string-list)
  (str:join #\Newline string-list))

(defmethod walk-sections (node fn)
  nil)

(defmethod walk-sections ((doc document) fn)
  (loop for n in (document-nodes doc) do
    (walk-sections n fn)))

(defmethod walk-sections ((section section-node) fn)
  (funcall fn section)
  (loop for n in (section-children section) do
        (walk-sections n fn)))

(defmethod nodes (x)
  nil)

(defmethod nodes ((doc document))
  (document-nodes doc))

(defmethod nodes ((section section-node))
  (section-children section))

(defmethod nodes ((p paragraph-node))
  (paragraph-nodes p))

(defun find-nodes (predicate x)
  (remove-if (compose #'not predicate) (nodes x)))

(let ((begin-pattern (ppcre:create-scanner " *\\#\\+begin_src (.*)"))
      (end-pattern (ppcre:create-scanner " *\\#\\+end_src")))
  (defun extract-source-blocks (s)
    (let ((starts nil)
          (ends nil))

      (ppcre:do-scans (ms me rs re begin-pattern s)
        (push (list ms me rs re) starts))
      (ppcre:do-scans (ms me rs re end-pattern s)
        (push (list ms me rs re) ends))

      (when (/= (length starts) (length ends))
        (error "Non-matching source begin/end lines"))

      (mapcar (lambda (start end)
                (destructuring-bind (start-ms start-me start-rs start-re) start
                  (destructuring-bind (end-ms end-me end-rs end-re) end
                    (declare (ignore end-rs end-re))
                    (let* ((indent (string-indent (subseq s start-ms start-me)))
                           (lang (subseq s (aref start-rs 0) (aref start-re 0)))
                           (src (subseq s (1+ start-me) (1- end-ms)))
                           (trimmed-src (->> src
                                             (lines)
                                             (mapcar (curry #'strip-indent indent))
                                             (unlines))))
                      (list start-ms end-me (cons lang trimmed-src))))))
              (nreverse starts)
              (nreverse ends)))))

(let ((pattern (ppcre:create-scanner "\\[\\[([^\\]]+)\\](?:\\[(.+)\\])?\\]")))
  (defun extract-links (s)
    (let ((result nil))
      (ppcre:do-scans (ms me rs re pattern s)
        (let ((url (subseq s (aref rs 0) (aref re 0)))
              (description (when (aref rs 1) (subseq s (aref rs 1) (aref re 1)))))
          (push (list ms me (cons url description)) result)))
      (nreverse result))))

(let ((pattern (ppcre:create-scanner "=([^ \\n=][^\\n=]*[^ \\n=])=")))
  (defun extract-bolds (s)
    (let ((result nil))
      (ppcre:do-scans (ms me rs re pattern s)
        (let ((text (subseq s (aref rs 0) (aref re 0))))
          (push (list ms me text) result)))
      (nreverse result))))

(let ((pattern (ppcre:create-scanner "~([^ \\n~][^\\n~]*[^ \\n~])~")))
  (defun extract-inline-code (s)
    (let ((result nil))
      (ppcre:do-scans (ms me rs re pattern s)
        (let ((text (subseq s (aref rs 0) (aref re 0))))
          (push (list ms me text) result)))
      (nreverse result))))

;; Must have trimmed indendation before
(let ((pattern (ppcre:create-scanner "(?:\\|(?:.*)\\|(?: )*\\n)+(?!\\|)")))
  (defun extract-tables (s)
    ;; Add a newline to make regex work when table ends at end of string
    (let ((s (concatenate 'string s "
"))
          (tables nil))
      (ppcre:do-scans (ms me rs re pattern s)
        (let ((table (->> (uiop:split-string (subseq s ms me) :separator (list #\Newline))
                          (remove-if #'string-emptyp)
                          (mapcar (lambda (x)
                                    (->>
                                     (uiop:split-string x :separator "|")
                                     (remove-if #'string-emptyp)
                                     (mapcar (curry #'string-trim '(#\Space)))
                                     (funcall (lambda (x)
                                                (if (and (car x)
                                                         (char-equal (char (car x) 1) #\-))
                                                    (list :line)
                                                    (list :row x))))))))))
          (push (list ms (1- me) table) tables)))
      (nreverse tables))))


(defun split-paragraphs (s)
  (let* ((result nil)
         (result2 nil)
         (lines (lines (string-trim '(#\Newline) s)))
         (min-indent (apply #'min (mapcar #'string-indent (remove-if #'string-emptyp lines)))))
    (loop for x in lines do
      (let ((trimmed-x (strip-indent min-indent x)))
        (if (string-emptyp trimmed-x)
            (progn
              (push (reverse result2) result)
              (setf result2 nil))
            (push trimmed-x result2))))
    (push (nreverse result2) result)
    (mapcar #'unlines (nreverse result))))

(defmethod convert (node)
  (warn "Unknown node: ~a" node)
  (list node))

(defmethod convert ((node cl-org-mode::outline-node))
  (let* ((children (cl-org-mode::node.children node))
         (non-prop-children (remove-if (lambda (x) (typep x 'cl-org-mode::properties-node)) children))
         (prop-nodes (->> children
                          (remove-if (lambda (x) (not (typep x 'cl-org-mode::properties-node))))
                          (mappend 'cl-org-mode::node.children)
                          (remove-if (lambda (x) (not (typep x 'cl-org-mode::property-node))))))
         (properties (make-property-table)))

    (loop for n in prop-nodes do
      (setf (gethash (cl-org-mode::property-node.property n) properties)
            (string-trim '(#\Space) (cl-org-mode::property-node.value n))))

    (list
     (make-instance 'section-node
                    :headline (cl-org-mode::node.heading node)
                    :properties properties
                    :child-nodes (mappend #'convert non-prop-children)))))

(defun chunk-nodes (string locations node-generator)
  (let ((result nil)
        (current 0))
    (if locations
        (loop for (start end data) in locations
              for i from 0 do
                (push (make-instance 'text-node :text (subseq string current start)) result)
                (push (funcall node-generator data) result)
                (when (= i (1- (length locations)))
                  (push (make-instance 'text-node :text (subseq string end)) result))
                (setf current end))
        (push (make-instance 'text-node :text string) result))

    (nreverse result)))

(defmethod splice-source-blocks (node)
  (list node))

(defmethod splice-source-blocks ((node text-node))
  (let ((text (text-text node)))
    (chunk-nodes text (extract-source-blocks text)
                 (lambda (x) (make-instance 'source-node
                                            :language (car x)
                                            :code (cdr x))))))

(defmethod splice-links (node)
  (list node))

(defmethod splice-links ((node text-node))
  (let ((text (text-text node)))
    (chunk-nodes text (extract-links text)
                 (lambda (x) (make-instance 'link-node
                                            :url (car x)
                                            :description (cdr x))))))

(defmethod splice-links ((node table-node))
  (list
   (make-instance 'table-node
                  :rows
                  (mapcar (lambda (x)
                            (if (eq (car x) :row)
                                (cons :row (mapcar (lambda (col)
                                                     ;; TODO dont create nodes here
                                                     (splice-links (make-instance 'text-node :text col))) (cadr x)))

                                ;; TODO wrong probably (handle horizontal lines)
                                x)) (table-rows node)))))

(defmethod splice-bolds (node)
  (list node))

(defmethod splice-bolds ((node text-node))
  (if (text-style node)
      (list node)
      (let ((text (text-text node)))
        (chunk-nodes text (extract-bolds text)
                     (lambda (x) (make-instance 'text-node :text x :style :bold))))))

(defmethod splice-inline-code (node)
  (list node))

(defmethod splice-inline-code ((node text-node))
  (if (text-style node)
      (list node)
      (let ((text (text-text node)))
        (chunk-nodes text (extract-inline-code text)
                     (lambda (x) (make-instance 'text-node :text x :style :bold))))))

(defmethod convert ((node cl-org-mode::text-node))
  (unless (string-emptyp (cl-org-mode::node.text node))

    (let ((nodes (chunk-nodes (cl-org-mode::node.text node) (extract-source-blocks (cl-org-mode::node.text node))
                              (lambda (x) (make-instance 'source-node
                                                         :language (car x)
                                                         :code (cdr x))))))

      (flet ((chunk-rest (text)
               (let ((tables (extract-tables text)))
                 (let ((nodes (chunk-nodes text tables (lambda (x) (make-instance 'table-node :rows x)))))
                   (make-instance 'paragraph-node
                                  :child-nodes
                                  (->> nodes
                                       (mappend #'splice-source-blocks)
                                       (mappend #'splice-links)
                                       (mappend #'splice-bolds)
                                       (mappend #'splice-inline-code)
                                       ))))))

        (mappend (lambda (node)
                   (if (and (typep node 'text-node)
                            (not (string-emptyp (text-text node))))
                       (mapcar #'chunk-rest (split-paragraphs (text-text node)))
                       (list node)))
                 nodes)))))

(defun parse-file (path)
  (flet ((property? (x)
           (if (and (typep x 'cl-org-mode::text-node)
                    (str:starts-with? "#+" (cl-org-mode::node.text x)))
               (let ((s (str:split ":" (subseq (cl-org-mode::node.text x) 2))))
                 (cons (str:trim (car s))
                       (str:trim (cadr s)))))))
    (let* ((file (cl-org-mode::read-org-file path))
           (children (cl-org-mode::node.children file))
           (props (make-property-table)))
      (loop for child in children
            for p = (property? child)
            while p
            do (put-prop props (car p) (cdr p))
               (pop children))
      (values
       (make-instance 'document
                      :nodes (mappend #'convert children)
                      :properties props)
       file))))

(defgeneric get-property (node key)
  (:documentation "Get property `KEY' of `NODE'."))

(defmethod get-property (node key) nil)

(defmethod get-property ((node document) key)
  (gethash key (slot-value node 'properties)))

(defmethod get-property ((node section-node) key)
  (gethash key (slot-value node 'properties)))
