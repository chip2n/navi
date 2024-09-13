(defpackage #:navi/utils
  (:use #:cl)
  (:export
   #:system-path))

(defpackage #:navi/server
  (:use #:cl)
  (:export
   #:start
   #:stop))

(defpackage #:navi/style
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:define-style
   #:compile-styles))

(defpackage #:navi/org
  (:use #:cl #:alexandria #:arrows)
  (:export
   #:parse-file
   #:document-nodes
   #:document
   #:section-node
   #:section-headline
   #:section-children
   #:text-node
   #:text-text
   #:text-style
   #:paragraph-node
   #:paragraph-nodes
   #:link-node
   #:link-url
   #:link-description
   #:table-node
   #:table-rows
   #:source-node
   #:source-language
   #:source-code
   #:get-prop
   #:nodes
   #:get-property))

(defpackage #:navi
  (:use #:cl)
  (:export
   #:define-page
   #:define-tag
   #:add-asset
   #:build-pages
   #:stop
   #:start
   #:add-page
   #:*output-dir*))
