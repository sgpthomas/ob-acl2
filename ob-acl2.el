(provide 'ob-acl2)

(defgroup ob-acl2 nil
  "Customization of org babel acl2"
  :group 'ob-acl2)

(defcustom org-babel-acl2-command "acl2"
  "Name of the command for executing the Acl2 shell."
  :group 'ob-acl2
  :type string)

(defun org-babel-execute:acl2 (body params)
  (let (()))
  (org-babel-eval)
  )
