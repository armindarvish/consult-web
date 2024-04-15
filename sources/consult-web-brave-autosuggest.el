;;; consult-web-brave-autosuggest.el --- Consulting Brave Autosuggest -*- lexical-binding: t -*-

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

(defvar consult-web-brave-autosuggest-api-url "https://api.search.brave.com/res/v1/suggest/search")


(defcustom consult-web-brave-autosuggest-api-key nil
  "Key for Brave Autosuggest API.

See URL `https://brave.com/search/api/' for more info"
  :group 'consult-web
  :type '(choice (const :tag "Brave Autosuggest API Key" string)
                 (function :tag "Custom Function")))

(cl-defun consult-web--brave-autosuggest-fetch-results (input &rest args &key count page &allow-other-keys)
  "Fetch search results for INPUT from `consult-web-brave-autosuggest-api-url'.
"
  (let* ((count (or (and (integerp count) count)
                    (and count (string-to-number (format "%s" count)))
                    consult-web-default-count))
         (page (or (and (integerp page) page)
                   (and page (string-to-number (format "%s" page)))
                   consult-web-default-page))
         (params  `(("q" . ,input)
                    ("count" . ,(format "%s" count))
                    ("page" . ,(format "%s" page))
                    ("country" . "US")))
         (headers `(("User-Agent" . "Emacs:consult-web/0.1 (Emacs consult-web package; https://github.com/armindarvish/consult-web)")
                    ("Accept" . "application/json")
                    ("Accept-Encoding" . "gzip")
                    ("X-Subscription-Token" . ,(consult-web-expand-variable-function consult-web-brave-autosuggest-api-key))
                    )))
    (funcall consult-web-retrieve-backend
     consult-web-brave-autosuggest-api-url
     :params params
     :headers headers
     :parser
     (lambda ()
       (goto-char (point-min))
       (buffer-substring (point-min) (point-max))
       (let* ((content (json-parse-buffer))
              (original (make-hash-table :test 'equal))
              (_ (puthash "query" (gethash "original" (gethash "query" content)) original))
              (suggestions (gethash "results" content)))
         (cl-loop for a across (vconcat suggestions (vector original))
                  collect
                  (let ((table (make-hash-table :test 'equal))
                        (word (gethash "query" a)))
                    (puthash :url
                             (concat "https://search.brave.com/search?q=" (url-hexify-string word)) table)
                    (puthash :search-url nil
                             table)
                    (puthash :title
                             word table)
                    (puthash :source
                             "Brave AutoSuggest" table)
                    (puthash :query input
                             table)
                    table
                    ))

         )
       ))))

(consult-web-define-source "Brave AutoSuggest"
                           :narrow-char ?B
                           :face 'consult-web-engine-source-face
                           :request #'consult-web--brave-autosuggest-fetch-results
                           :on-preview #'ignore
                           :on-return #'identity
                           :on-callback #'string-trim
                           :search-history 'consult-web--search-history
                           :selection-history t
                           :dynamic t
                           )

;;; provide `consult-web-brave-autosuggest' module

(provide 'consult-web-brave-autosuggest)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-brave-autosuggest)
;;; consult-web-brave-autosuggest.el ends here
