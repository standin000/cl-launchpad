(in-package :launchpad)

(defun get-entry (entry &key (debug nil) (parameters nil))
 (let ((response (drakma:http-request entry 
			       :method :get
			       :parameters parameters
			       :additional-headers (get-additional-headers))))
  (if debug
      response
      (with-input-from-string 
	  (s (sb-ext:octets-to-string response))
	(json:decode-json s)))))

;; Plato Wu,2009/11/12: cl-json will convert false and null in json into nil in lisp,
;; so it can not fully recover them to json from lisp, but launchpad.net think false
;; and null are different in json.

(defun put-entry (entry document)
  (http-request entry
		:method :put
		:additional-headers (get-additional-headers)
		:content (encode-json-alist-to-string document)
                :content-type "application/json"))

(defun modify-entry (entry field)
  ;; Plato Wu,2009/11/13: it need my patch for drakma
  (http-request entry
		:method :patch
		:additional-headers (get-additional-headers)
		:content (encode-json-alist-to-string field)
                :content-type "application/json"))

(defun post-entry (entry parameters)
  (http-request entry
		:method :post
		:additional-headers (get-additional-headers)
		:parameters parameters))

(defun get-all-bugs (project)
  (get-entry (format nil "https://api.edge.launchpad.net/beta/~a" project)
	      :parameters '(("ws.op" . "searchTasks"))))

(defun open-a-bug (project title description)
  (post-entry "https://api.edge.launchpad.net/beta/bugs" 
	    `(("ws.op" . "createBug")
	      ("description" . ,description)
	      ("target" . ,(format nil "https://api.edge.launchpad.net/beta/~a" project))
	      ("title" . ,title))))