;;; consult-web-brave.el --- Consulting Brave -*- lexical-binding: t -*-

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

(defvar consult-web-brave-search-url "https://search.brave.com/search")

(defvar consult-web-brave-url "https://api.search.brave.com/res/v1/web/search")

(defcustom consult-web-brave-api-key nil
  "Key for Brave API.

See URL `https://brave.com/search/api/' for more info"
  :group 'consult-web
  :type '(choice (const :tag "Brave API Key" string)
                 (function :tag "Custom Function")))


(cl-defun consult-web--brave-fetch-results (input &rest args &key count page &allow-other-keys)
  "Retrieve search results from Brave for INPUT.

COUNT is passed as count in query parameters.
PAGE is passed as page in query paramters.
"
  (let* ((count (or (and (integerp count) count)
                    (and count (string-to-number (format "%s" count)))
                    consult-web-default-count))
         (page (or (and (integerp page) page)
                   (and page (string-to-number (format "%s" page)))
                   consult-web-default-count))
         (count (min count 20))
         (params `(("q" . ,(url-hexify-string input))
                   ("count" . ,(format "%s" count))
                   ("page" . ,(format "%s" page))))
         (headers `(("User-Agent" . "Emacs:consult-web/0.1 (Emacs consult-web package; https://github.com/armindarvish/consult-web)")
                    ("Accept" . "application/json")
                    ("Accept-Encoding" . "gzip")
                    ("X-Subscription-Token" . ,(consult-web-expand-variable-function consult-web-brave-api-key))
                    )))
    (funcall consult-web-retrieve-backend
     consult-web-brave-url
     :params params
     :headers headers
     :parser
     (lambda ()
       (goto-char (point-min))
       (let* ((results (gethash "results" (gethash "web" (json-parse-buffer))))
              (items  (mapcar (lambda (item) `(:url ,(format "%s" (gethash "url" item)) :title ,(format "%s" (gethash "title" item)))) results))
              )
         (cl-loop for a in items
                  collect
                  (let ((table (make-hash-table :test 'equal)))
                    (puthash :url
                             (plist-get a :url) table)
                    (puthash :search-url (consult-web--make-url-string consult-web-brave-search-url params) table)
                    (puthash :title
                             (plist-get a :title) table)
                    (puthash :source "Brave"
                             table)
                    (puthash :query input
                             table)
                    table
                    ))))

     )))

(consult-web-define-source "Brave"
                           :narrow-char ?b
                           :face 'consult-web-engine-source-face
                           :request #'consult-web--brave-fetch-results
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :dynamic 'both
                           )

;;; provide `consult-web-brave' module

(provide 'consult-web-brave)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-brave)
;;; consult-web-brave.el ends here
