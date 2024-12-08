;;; consult-web-stackoverflow.el --- Consulting StackOverflow -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: "0.1"
;; Package-Requires: ((emacs "28.1") (consult "1.1"))
;; Homepage: https://github.com/armindarvish/consult-web
;; Keywords: convenience

;;; Commentary:

;;; Code:

(require 'consult-web)

(defvar consult-web-stackoverflow-search-url "https://stackoverflow.com/search")
(defvar consult-web-stackoverflow-api-url "https://api.stackexchange.com/2.3/search/advanced")

(defcustom consult-web-stackexchange-api-key nil
  "Key for Stack Exchange API.

See URL `https://api.stackexchange.com/', and URL `https://stackapps.com/' for more info"
  :group 'consult-web
  :type '(choice (const :tag "API Key" string)
                 (function :tag "Custom Function")))

(cl-defun consult-web--stackoverflow-fetch-results (input &rest args &key count page order sort &allow-other-keys)
  "Fetch search results for INPUT from stackoverflow.

COUNT is passed as pagesize in query parameters.
PAGE is passed as page in query parameters.
ORDER is passed as order in query parameters.
SORT is passed as sort in query parameters.

See URL `https://api.stackexchange.com/' for more info.
"
  (let* ((count (or (and (integerp count) count)
                    (and count (string-to-number (format "%s" count)))
                    consult-web-default-count))
         (count (min count 25))
         (page (or (and (integerp page) page)
                   (and page (string-to-number (format "%s" page)))
                   consult-web-default-page))
         (page (max page 1))
         (order (if (and order (member (format "%s" order) '("desc" "asc"))) (format "%s" order)))
         (sort (if (and sort (member (format "%s" sort) '("activity" "votes" "creation" "relevance"))) (format "%s" sort)))
         (params `(("order" . ,(or order "desc"))
                   ("sort" . ,(or sort "relevance"))
                   ("site" . "stackoverflow")
                   ("q" . ,(replace-regexp-in-string " " "+" input))
                   ("pagesize" . ,(format "%s" count))
                   ("page" . ,(format "%s" page))
                   ("key" . ,(consult-web-expand-variable-function consult-web-stackexchange-api-key)))))
    (funcall consult-web-retrieve-backend
     consult-web-stackoverflow-api-url
     :params params
     :parser
     (lambda ()
       (goto-char (point-min))
       (let* ((results (gethash "items" (json-parse-buffer)))
              (data  (mapcar (lambda (item) `(,(format "%s" (gethash "title" item)) ,(format "%s" (gethash "link" item)))) results))
              (table (make-hash-table :test 'equal)))
         (cl-loop for a in data
                  collect
                  (let ((table (make-hash-table :test 'equal)))
                    (puthash :url
                             (cadr a) table)
                    (puthash :search-url (concat consult-web-stackoverflow-search-url "?q=" input)
                             table)
                    (puthash :title
                             (car a) table)
                    (puthash :source "StackOverflow"
                             table)
                    (puthash :query input
                             table)
                   table

                    )))
       ))))

(consult-web-define-source "StackOverflow"
                           :narrow-char ?s
                           :face 'consult-web-engine-source-face
                           :request #'consult-web--stackoverflow-fetch-results
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :dynamic 'both
                           )

;;; provide `consult-web-stackoverflow' module

(provide 'consult-web-stackoverflow)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-stackoverflow)
;;; consult-web-stackoverflow.el ends here
