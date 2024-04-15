;;; consult-web-wikipedia.el --- Consulting Wikipedia -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (consult "1.1"))
;; Homepage: https://github.com/armindarvish/consult-web
;; Keywords: convenience

;;; Commentary:

;;; Code:

(require 'consult-web)

(defvar consult-web-wikipedia-search-url "https://www.wikipedia.org/search-redirect.php")
(defvar consult-web-wikipedia-url "https://wikipedia.org/")
(defvar consult-web-wikipedia-api-url "https://wikipedia.org/w/api.php")

(cl-defun consult-web--wikipedia-fetch-results (input &rest args &key count page &allow-other-keys)
  "Retrieve search results from Wikipedia for INPUT.

COUNT is passed as srlimit in query parameters.
PAGEis passed as sroffset in query paramters."
  (let* ((count (or (and (integerp count) count)
                    (and count (string-to-number (format "%s" count)))
                    consult-web-default-count))
         (page (or (and (integerp page) page)
                   (and page (string-to-number (format "%s" page)))
                   consult-web-default-count))
         (params `(("action" . "query")
                   ("format" . "json")
                   ("list" . "search")
                   ("formatversion" . "2")
                   ("prop" . "info")
                   ("inprop" . "url")
                   ("srwhat" . "text")
                   ("srsearch" . ,(url-hexify-string input))
                   ("srlimit" . ,(format "%s" count))
                   ("sroffset" . ,(format "%s" page))))
         (headers '(("User-Agent" . "Emacs:consult-web/0.1 (https://github.com/armindarvish/consult-web);"))))
    (funcall consult-web-retrieve-backend
     consult-web-wikipedia-api-url
     :params params
     :headers headers
     :parser
     (lambda ()
       (goto-char (point-min))
       (let* ((results (gethash "search" (gethash "query" (json-parse-buffer))))
              (titles  (mapcar (lambda (item) (format "%s" (gethash "title" item))) results))
              (table (make-hash-table :test 'equal)))
         (cl-loop for a in titles
                  collect
                  (let ((table (make-hash-table :test 'equal)))
                    (puthash :url
                             (concat consult-web-wikipedia-url "wiki/" (string-replace " " "_" a)) table)
                    (puthash :search-url (concat  consult-web-wikipedia-search-url "?" "search=" input) table)
                    (puthash :title
                             a table)
                    (puthash :source "Wikipedia" table)
                    (puthash :query input
                             table)
                    table
                    )))))))

(consult-web-define-source "Wikipedia"
                           :narrow-char ?w
                           :face 'consult-web-engine-source-face
                           :request #'consult-web--wikipedia-fetch-results
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :dynamic 'both
                           )

;;; provide `consult-web-wikipedia' module

(provide 'consult-web-wikipedia)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-wikipedia)
;;; consult-web-wikipedia.el ends here
