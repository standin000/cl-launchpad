(in-package :launchpad)

(let ((%unix-epoch% (encode-universal-time 0 0 0 1 1 1970 0)))
  (defun unix-to-lisp-time (unix-time)
    "Lisp and Unix have differing notions of time:
Universal time is an absolute time represented as a single
non-negative integer---the number of seconds since midnight,
January 1, 1900 GMT (ignoring leap seconds).
This is unlike Unix's gettimeofday() that is expressed in seconds
and microseconds since midnight (0 hour), January 1, 1970.
See HyperSpec/Body/25_adb.htm"
    (+ unix-time %unix-epoch%))

  (defun lisp-to-unix-time (&optional lisp-time)
    (- (or lisp-time
       (get-universal-time))
       %unix-epoch%)))

(defvar weak-oauth-token nil)
(defvar weak-oauth-token-secret nil)
(defvar oauth-token nil)
(defvar oauth-token-secret nil)
(defvar consumer-key nil)


(defun get-request-token ()
    (let ((responses 
	   (cl-ppcre:split "[&=]" 
			(drakma:http-request 
			 "https://edge.launchpad.net/+request-token" 
			 :method :post 
			 :parameters (list (cons "oauth_consumer_key" consumer-key)
					   (cons "oauth_signature_method" "PLAINTEXT")
					   (cons "oauth_signature" "&"))))))
   (setf weak-oauth-token (second responses))
   (setf weak-oauth-token-secret (fourth responses))))

(defun get-token-and-login (user-name)
  (setf consumer-key user-name)
  (get-request-token)
  (format t "The authorization page(https://edge.launchpad.net/+authorize-token?oauth_token=~a) should be opening in your browser. After you have authorized this program to access Launchpad on your behalf you should come back here and press <Enter> to finish the authentication process." weak-oauth-token)
  (read-line)
  (exchange-request-token))

(defun exchange-request-token ()
    (let ((responses 
	   (cl-ppcre:split "[&=]"
			   (drakma:http-request 
			    "https://edge.launchpad.net/+access-token" 
			    :method :post 
			    :parameters (list (cons "oauth_token" weak-oauth-token )
					      (cons "oauth_consumer_key" consumer-key)
					      (cons "oauth_signature_method" "PLAINTEXT")
					      (cons "oauth_signature" (format nil "&~a" weak-oauth-token-secret)))))))
   (setf oauth-token (second responses))
   (setf oauth-token-secret (fourth responses))))

(defun get-additional-headers ()
  (list (cons 
	 "Authorization" 
	 ;; Plato Wu,2009/11/13: it need my patch for drakma, or don't use
	 ;; lambda form.
	 (lambda () 
	   (format nil 
		  "OAuth realm=\"https://api.launchpad.net/ \",
                   oauth_consumer_key=\"~a\",
                   oauth_token=\"~a\",
                   oauth_signature_method=\"PLAINTEXT\",
                   oauth_signature=\"&~a\",
                   oauth_timestamp=\"~a\",
                   oauth_nonce=\"~a\",
                   oauth_version=\"1.0\"" 
		   consumer-key
		   oauth-token
		   oauth-token-secret
		   (lisp-to-unix-time)
		   (random 100000))))))