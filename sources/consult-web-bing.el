;;; consult-web-bing.el --- Consulting Bing -*- lexical-binding: t -*-

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

(defvar consult-web-bing-search-api-url "https://api.bing.microsoft.com/v7.0/search")

(defcustom consult-web-bing-search-api-key nil
"Key for Bing (Microsoft Azure) search API

See URL `https://www.microsoft.com/en-us/bing/apis/bing-web-search-api' and URL `https://learn.microsoft.com/en-us/bing/search-apis/bing-web-search/search-the-web' for details"
:group 'consult-web
:type '(choice (const :tag "API Key" string)
               (function :tag "Custom Function")))


(cl-defun consult-web--bing-fetch-results (input &rest args &key count page &allow-other-keys)
  "Fetches search results for INPUT from Bing web search api.

COUNT is passed as count in query parameters.
(* PAGE COUNT) is passed as offset in query paramters.

Refer to URL `https://programmablesearchengine.google.com/about/' and `https://developers.google.com/custom-search/' for more info.
"
  (let* ((count (or (and (integerp count) count)
                    (and count (string-to-number (format "%s" count)))
                    consult-web-default-count))
         (page (or (and (integerp page) page)
                     (and page (string-to-number (format "%s" page)))
                     consult-web-default-count))
         (count (max count 1))
         (page (* page count))
         (params `(("q" . ,(replace-regexp-in-string " " "+" input))
                   ("count" . ,(format "%s" count))
                   ("offset" . ,(format "%s" page))))
         (headers `(("Ocp-Apim-Subscription-Key" . ,(consult-web-expand-variable-function consult-web-bing-search-api-key)))))
    (funcall consult-web-retrieve-backend
     consult-web-bing-search-api-url
     :params params
     :headers headers
     :parser
     (lambda ()
       (goto-char (point-min))
        (let* ((results (json-parse-buffer))
               (webpages (gethash "webPages" results))
               (search-url (gethash "webSearchUrl" webpages))
               (items (gethash "value" webpages)))
         (cl-loop for a across items
                  collect
                  (let ((table (make-hash-table :test 'equal))
                        (title (gethash "name" a))
                        (url (gethash "url" a))
                        (snippet (gethash "snippet" a)))
                    (puthash :url url
                             table)
                    (puthash :search-url search-url
                             table)
                    (puthash :title title
                             table)
                    (puthash :source "Bing"
                             table)
                    (puthash :query input
                             table)
                    (puthash :snippet snippet
                             table)
                    table
                    )
                  ))
))))

(consult-web-define-source "Bing"
                           :narrow-char ?m
                           :face 'consult-web-engine-source-face
                           :request #'consult-web--bing-fetch-results
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :dynamic 'both
                           )

;;; provide `consult-web-bing' module

(provide 'consult-web-bing)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-bing)
;;; consult-web-bing.el ends here
