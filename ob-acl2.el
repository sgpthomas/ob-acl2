(require 'ob)
(require 'comint)
(require 'dash)
(provide 'ob-acl2)

;; Set up variables for ACL2
(defgroup ob-acl2 nil
  "Customization of org babel acl2"
  :group 'ob-acl2)

(defcustom org-babel-acl2-command "acl2"
  "Name of the command for executing the Acl2 shell."
  :group 'ob-acl2
  :type 'string)

;; Tell org babel about acl2
(add-to-list 'org-babel-tangle-lang-exts
	     '("acl2" "lisp"))

(defun org-babel-execute:acl2 (body params)
  (let ((session (org-babel-acl2-initiate-session
		  (cdr (assq :session params)))))
    (org-babel-acl2-execute session body)))

(defun org-babel-acl2-initiate-session (session)
  (unless (string= session "none")
    (let* ((buf-name (format "*acl2-session:%s*" session))
	   (proc-name (org-babel-acl2--proc-name session)))
      (make-comint-in-buffer proc-name buf-name org-babel-acl2-command)
      (message "words: %s" (org-babel-acl2--send-string buf-name ":help"))
      buf-name)))

(defun org-babel-acl2--proc-name (session)
  (format "acl2<%s>" session))

(defun org-babel-acl2--send-string (session body)
  (org-babel-comint-in-buffer session
    (let* ((string-buffer "")
	   (comint-output-filter-functions
	    (cons (lambda (text)
		    (setq string-buffer
				 (concat string-buffer text)))
		  comint-output-filter-functions)))
      (insert body)
      (comint-send-input)
      (while (not (string-match "ACL2 !>" string-buffer))
	(accept-process-output (get-buffer-process (current-buffer))))
      (--> (substring string-buffer 0 (match-beginning 0))
	   (org-babel-chomp it)
	   (org-babel-chomp it)
	   (org-babel-chomp it)))))

(defun org-babel-acl2-execute (session body)
  (org-babel-acl2--send-string session body))

;;;###autoload
(define-derived-mode acl2-mode lisp-mode "ACL2 Mode"
  "Major Mode for dealing with ACL2 files.")
