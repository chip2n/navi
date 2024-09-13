(asdf:defsystem #:navi
  :description "A static site generator"
  :author "Andreas Arvidsson <andreas@arvidsson.io>"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on ("alexandria"
               "trivial-file-watch"
               "clack"
               "websocket-driver"
               "spinneret"
               "lass"
               "arrows"
               "cl-org-mode"
               "cl-ppcre"
               "cl-fad"
               "str")
  :components ((:file "package")
               (:file "style")
               (:file "server")
               (:file "org")
               (:file "navi")))
