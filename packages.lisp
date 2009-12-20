(defpackage :launchpad
    (:documentation "A Common LISP library for lauchpad.net") 
    (:use :cl :drakma :cl-ppcre :json)
    (:export :get-token-and-login :get-all-bugs :get-bug :open-bug :update-bug :add-comment))
  
