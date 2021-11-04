(require 'org-babel)
(provide 'ob-acl2)

;; Set up variables for ACL2
(defgroup ob-acl2 nil
  "Customization of org babel acl2"
  :group 'ob-acl2)

(defcustom org-babel-acl2-command "acl2"
  "Name of the command for executing the Acl2 shell."
  :group 'ob-acl2
  :type string)

;; Tell org babel about acl2
(org-babel-add-interpreter "acl2")
(add-to-list 'org-babel-tangle-lang-exts
	     '("acl2" "lisp"))

(defun org-babel-execute:acl2 (body params)
  (let ((session (org-babel-acl2-initiate-session
		  (cdr (assq :session params)))))
    (org-babel-eval "echo 'hi'")))

;; (defun org-babel-acl2-initiate-session (session)
;;   (unless (string= session "none")
;;     (save-window-excursion
;;       ;; (let* ((session)))
;;       )
;;     )
;;   )

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

;; (kill-process "acl2")


;; (org-babel-acl2-initiate-session "test")
