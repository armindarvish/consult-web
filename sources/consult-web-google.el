;;; consult-web-google.el --- Consulting Google -*- lexical-binding: t -*-

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

(defvar consult-web-google-search-url "https://www.google.com/search")

(defvar consult-web-google-customsearch-api-url "https://www.googleapis.com/customsearch/v1")

(defcustom consult-web-google-customsearch-key nil
"Key for Google custom search API

See URL `https://developers.google.com/custom-search/' and URL `https://developers.google.com/custom-search/v1/introduction' for details"
:group 'consult-web
:type '(choice (const :tag "API Key" string)
               (function :tag "Custom Function")))

(defcustom consult-web-google-customsearch-cx nil
"CX for Google custom search API

See URL `https://developers.google.com/custom-search/' and URL `https://developers.google.com/custom-search/v1/introduction' for details"
:group 'consult-web
:type '(choice (const :tag "CX String" string)
               (function :tag "Custom Function")))

(cl-defun consult-web--google-fetch-results (input &rest args &key count page filter &allow-other-keys)
  "Fetches search results for INPUT from “Google custom search” service.

COUNT is passed as num in query parameters.
(* PAGE COUNT) is passed as start in query paramters.

Refer to URL `https://programmablesearchengine.google.com/about/' and `https://developers.google.com/custom-search/' for more info.
"
  (let* ((count (or (and (integerp count) count)
                    (and count (string-to-number (format "%s" count)))
                    consult-web-default-count))
         (page (or (and (integerp page) page)
                     (and page (string-to-number (format "%s" page)))
                     consult-web-default-page))
         (filter (or (and (integerp filter) filter)
                     (and filter (string-to-number (format "%s" filter)))
                     1))
         (filter (if (member filter '(0 1)) filter 1))
         (count (min count 10))
         (page (+ (* page count) 1))
         (params `(("q" . ,(replace-regexp-in-string " " "+" input))
                   ("key" . ,(consult-web-expand-variable-function consult-web-google-customsearch-key))
                   ("cx" . ,(consult-web-expand-variable-function consult-web-google-customsearch-cx))
                   ("gl" . "en")
                   ("filter" . ,(format "%s" filter))
                   ("num" . ,(format "%s" count))
                   ("start" . ,(format "%s" page))))
         (headers '(("Accept" . "application/json")
                    ("Accept-Encoding" . "gzip")
                    ("User-Agent" . "consult-web (gzip)"))))
    (funcall consult-web-retrieve-backend
     consult-web-google-customsearch-api-url
     :params params
     :headers headers
     :parser
     (lambda ()
       (goto-char (point-min))
       (let* ((results (gethash "items" (json-parse-buffer)))
              (items  (mapcar (lambda (item) `(:url ,(format "%s" (gethash "link" item)) :title ,(format "%s" (gethash "title" item)) :snippet ,(string-trim (format "%s" (gethash "snippet" item))))) results)))
         (cl-loop for a in items
                  collect
                  (let ((table (make-hash-table :test 'equal)))
                    (puthash :url
                             (plist-get a :url) table)
                    (puthash :search-url (consult-web--make-url-string consult-web-google-search-url params '("key" "cx" "gl"))
                             table)
                    (puthash :title
                             (plist-get a :title) table)
                    (puthash :source "Google"
                             table)
                    (puthash :query input
                             table)
                    (puthash :snippet (plist-get a :snippet) table)
                    table
                    )
                  ))))))

(consult-web-define-source "Google"
                           :narrow-char ?g
                           :face 'consult-web-engine-source-face
                           :request #'consult-web--google-fetch-results
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :dynamic 'both
                           )

;;; provide `consult-web-google' module

(provide 'consult-web-google)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-google)
;;; consult-web-google.el ends here
