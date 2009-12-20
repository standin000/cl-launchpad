(in-package :launchpad)

(defun http-request-for-launchpad (&rest args)
  (multiple-value-bind (body status header)
      (apply #'http-request args)
    (if (not (member status '(200 201 209)))
        (cerror (format nil "It returns HTTP ~A" status) "Ignore it"))
    (if body
        (setf body
              (with-input-from-string 
                  (s (sb-ext:octets-to-string body))
                (json:decode-json s))))
    (values body status header)))

(defun get-entry (entry &key (parameters nil))
  (http-request-for-launchpad entry 
                :method :get
                :parameters parameters
                :additional-headers (get-additional-headers)))

;; Plato Wu,2009/11/12: cl-json will convert false and null in json into nil in lisp,
;; so it can not fully recover them to json from lisp, but launchpad.net think false
;; and null are different in json.

(defun put-entry (entry document)
  (http-request-for-launchpad entry
		:method :put
		:additional-headers (get-additional-headers)
		:content (encode-json-alist-to-string document)
                :content-type "application/json"))

(defun modify-entry (entry field)
  (http-request-for-launchpad entry
		:method :patch
		:additional-headers (get-additional-headers)
		:content (encode-json-alist-to-string field)
                :content-type "application/json"))

(defun delete-entry (entry)
  (http-request-for-launchpad entry
		:method :delete
		:additional-headers (get-additional-headers)))


(defun post-entry (entry parameters)
  (http-request-for-launchpad entry
		:method :post
		:additional-headers (get-additional-headers)
		:parameters parameters))

(defun get-all-bugs (project)
  (get-entry (format nil "https://api.edge.launchpad.net/beta/~a" project)
	      :parameters '(("ws.op" . "searchTasks"))))

(defun open-bug (project title description)
  (multiple-value-bind (body status header)
   (post-entry "https://api.edge.launchpad.net/beta/bugs" 
               `(("ws.op" . "createBug")
                 ("description" . ,description)
                 ("target" . ,(format nil "https://api.edge.launchpad.net/beta/~a" project))
                 ("title" . ,title)))
    (declare (ignore body))
    (if (not (= status 201))
        (error "Create failed ~A!" header))
    (parse-integer (subseq (cdr (assoc :location header)) 41))))

(defun update-bug (bug-id &key title description)
  (multiple-value-bind (body status header)
   (modify-entry (format nil "https://api.edge.launchpad.net/beta/bugs/~a" bug-id)
                 (list (cons "title" title) (cons "description" description)))
    (if (not (= status 209))
        (cerror "update failed!" "Ignore it?"))))

(defun get-bug (bug-id)
  (get-entry (format nil "https://api.edge.launchpad.net/beta/bugs/~a" bug-id)))

(defun add-comment (bug-id comment)
  (post-entry (format nil "https://api.edge.launchpad.net/beta/bugs/~a" bug-id)
	    `(("ws.op" . "newMessage")
	      ("content" . ,comment))))