(defpackage :launchpad
    (:use :cl :drakma :cl-ppcre :json)
    (:export :get-token-and-login :get-all-bugs :open-a-bug))
  
