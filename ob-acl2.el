(require 'ob)
(require 's)

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
	   (proc-name (org-babel-acl2--proc-name session))
	   (buf (get-buffer-create buf-name)))
      (unless (get-buffer-process buf)
	(start-process-shell-command proc-name buf org-babel-acl2-command))
      session)
    ;; (save-window-excursion
    ;;   (message "%s" "asdf")
    ;;   )
    ))

(defun org-babel-acl2--proc-name (session)
  (format "acl2<%s>" session)
  )

(defun org-babel-acl2--send-string (session body)
  (process-send-string (org-babel-acl2--proc-name session)
		       body))

(defvar org-babel-acl2-output "new test")
(defvar org-babel-acl2-output-initialized 'nil)
(defun org-babel-acl2-filter (proc string)
  (message "%s" string)
  (if (or org-babel-acl2-output-initialized (s-contains? "ACL2 !>" string))
      (progn
	(setq org-babel-acl2-output-initialized t)
	(setq org-babel-acl2-output (cadr (s-split "ACL2 !>" string))))
    (setq org-babel-acl2-output string)))

(defun org-babel-acl2-execute (session body)
  (let* ((proc (get-process (org-babel-acl2--proc-name session))))
    (set-process-filter proc 'org-babel-acl2-filter)
    (org-babel-acl2--send-string session body)
    org-babel-acl2-output))

;;;###autoload
(define-derived-mode acl2-mode lisp-mode "ACL2 Mode"
  "Major Mode for dealing with ACL2 files.")

;; (save-window-excursion
;;   (with-current-buffer (get-buffer-create "*acl2-session:test*")
;;     ;; (erase-buffer)
;;     ;; (insert "(+ 1 1)")
;;     (process-send-string "acl2" "(+ 1 1)\n")
;;     (goto-char (point-max))
;;     )
;;   )

;; (start-process-shell-command "acl2" "*acl2-session:test*" "acl2")
;; (process-send-eof "acl2")

(kill-process "acl2<nat>")


;; (org-babel-acl2-initiate-session "test")
