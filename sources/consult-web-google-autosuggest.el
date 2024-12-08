;;; consult-web-google-autosuggest.el --- Consulting Google Autosuggest -*- lexical-binding: t -*-

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

(defvar consult-web-google-autosuggest-api-url "http://suggestqueries.google.com/complete/search")

(cl-defun consult-web--google-autosuggest-fetch-results (input &rest args &key count page &allow-other-keys)
  "Fetch search results for INPUT from Google Autosuggest.

Uses `consult-web-google-autosuggest-api-url' as autosuggest api url."
  (let* ((count (or (and (integerp count) count)
                    (and count (string-to-number (format "%s" count)))
                    consult-web-default-count))
         (page (or (and (integerp page) page)
                    (and page (string-to-number (format "%s" page)))
                    consult-web-default-count))
         (params `(("q" . ,input)
                   ("client" . "chrome")))
         (headers `(("Accept" . "application/json"))))
  (funcall consult-web-retrieve-backend
   consult-web-google-autosuggest-api-url
   :params params
   :headers headers
   :parser
   (lambda ()
     (goto-char (point-min))
     (let* ((results (json-parse-buffer))
            (cands (vconcat (vector (elt results 0)) (aref (cl-subseq results 1) 0))))
       (cl-loop for a across cands
                collect
                (let ((table (make-hash-table :test 'equal)))
                  (puthash :url
                           (concat "https://www.google.com/search?q=" (url-hexify-string a)) table)
                  (puthash :title
                           a table)
                  (puthash :search-url nil table)
                  (puthash :source
                           "Google AutoSuggest" table)
                  (puthash :query input
                          table)
                  table
                  ))

       ))

   )))

(consult-web-define-source "Google AutoSuggest"
                           :narrow-char ?G
                           :face 'consult-web-engine-source-face
                           :request #'consult-web--google-autosuggest-fetch-results
                           :on-preview #'ignore
                           :on-return #'identity
                           :on-callback #'string-trim
                           :search-history 'consult-web--search-history
                           :selection-history t
                           :dynamic t
                           )

;;; provide `consult-web-google-autosuggest' module

(provide 'consult-web-google-autosuggest)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-google-autosuggest)
;;; consult-web-google-autosuggest.el ends here
