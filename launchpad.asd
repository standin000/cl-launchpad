;;;; -*- Mode: LISP; -*-
(asdf:defsystem :launchpad
  :version "0.1"
  :serial t
  :author "Plato Wu <standin-000@tianya.cn>"
  :license "GPL"
  :components ((:file "packages")
               (:file "oauth")
	       (:file "launchpad"))
  :depends-on (drakma cl-ppcre cl-json))
