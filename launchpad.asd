;;;; -*- Mode: LISP; -*-
(asdf:defsystem :launchpad
  :version "0.2"
  :serial t
  :author "Plato Wu <gtalk000@gmail.com>"
  :license "MIT"
  :components ((:file "packages")
               (:file "oauth")
	       (:file "launchpad"))
  :depends-on (drakma cl-ppcre cl-json))
