;;; github.el --- github api library

;;; The MIT License (MIT)
;;; Copyright (c) 2012 Satoshi Takei <s.takei.dev@gmail.com>
;;; 
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(require 'json)

(defvar *curl-path* "curl")
(defvar *curl-output-buffer-name* "*curl output*")
(defvar *github-api-host-name* "api.github.com")

(defun curl-command-to-buffer (&rest args)
  (save-excursion
    (set-buffer *curl-output-buffer-name*)
    (erase-buffer)
    (apply #'call-process
	   (append (list *curl-path* nil *curl-output-buffer-name* nil "-s") args))))

(defun curl-command-to-string (&rest args)
  (save-excursion
    (apply #'curl-command-to-buffer args)
    (set-buffer *curl-output-buffer-name*)
    (buffer-string)))

(defun curl-command (&rest args)
  (json-read-from-string
   (apply #'curl-command-to-string args)))

(defun api-url (api &optional token)
  (let ((atmark (if token "@" "")))
    (setf token (or token ""))
    (format "https://%s%s%s%s"
	    token atmark *github-api-host-name* api)))

(defun github-create-new-authorization (user-name password)
  (curl-command
   "-u" (format "%s:%s" user-name password)
   "-d" "{\"scopes\":[\"gist\"]}"
   (api-url "/authorizations")))

(defun github-delete-authorization (token id)
  (curl-command
   "-x" "delete"
   (api-url (format "/authorizations/%d" id) token)))

(defun github-gists-list-gists (token)
  (curl-command (api-url "/gists" token)))

(provide 'github)

