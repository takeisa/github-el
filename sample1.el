(load-file "./github.el")

(defun create-authorization (user-name password)
  (interactive (list
		(read-string "User name:")
		(read-passwd "Password:") ))
  (let* ((auth (github-create-new-authorization user-name password))
	 (token (cdr (assoc 'token auth)))
	 (id (cdr (assoc 'id auth))))
    (list token id)))

(defun gists-description-list (token)
  (interactive "sToken:")
  (let ((gists (github-gists-list-gists token)))
    (map 'list #'(lambda (gist) (cdr (assoc 'description gist))) gists)))

(defun delete-authorization (token id)
  (interactive "sToken:\nnId:")
  (github-delete-authorization token id))
