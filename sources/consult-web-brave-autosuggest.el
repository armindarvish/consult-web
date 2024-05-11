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

(cl-defun consult-web--brave-autosuggest-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetch search results for INPUT from Brave Autosuggest API.
"
  (pcase-let* ((`(,query . ,opts) (consult-web--split-command input))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (page (plist-get opts :page))
               (count (or (and (integerp count) count)
                          (and count (string-to-number (format "%s" count)))
                          consult-web-default-count))
               (page (or (and (integerp page) page)
                         (and page (string-to-number (format "%s" page)))
                         consult-web-default-page))
               (count (min (max count 1) 20))
               (params  `(("q" . ,query)
                          ("count" . ,(format "%s" count))
                          ("page" . ,(format "%s" page))
                          ("country" . "US")))
               (headers `(("User-Agent" . "Emacs:consult-web/0.1 (Emacs consult-web package; https://github.com/armindarvish/consult-web)")
                          ("Accept" . "application/json")
                          ("Accept-Encoding" . "gzip")
                          ("X-Subscription-Token" . ,(consult-web-expand-variable-function consult-web-brave-autosuggest-api-key))
                          )))
    (consult-web--fetch-url consult-web-brave-autosuggest-api-url consult-web-http-retrieve-backend
                                  :params params
                                  :headers headers
                                  :parser #'consult-web--json-parse-buffer
                                  :callback
                                  (lambda (attrs)
                                    (when-let* ((original (make-hash-table :test 'equal))
                                                (_ (puthash "query" (gethash "original" (gethash "query" attrs)) original))
                                                (raw-results  (append (map-nested-elt attrs '("results")) (list original)))
                                                (annotated-results
                                                 (mapcar (lambda (item)
                                                           (let* ((source "Brave AutoSuggest")
                                                                  (word (gethash "query" item))
                                                                  (url (concat "https://search.brave.com/search?q="  (replace-regexp-in-string " " "+" word)))
                                                                  (urlobj (and url (url-generic-parse-url url)))
                                                                  (domain (and (url-p urlobj) (url-domain urlobj)))
                                                                  (domain (and (stringp domain)
                                                                               (propertize domain 'face 'font-lock-variable-name-face)))
                                                                  (path (and (url-p urlobj) (url-filename urlobj)))
                                                                  (path (and (stringp path)
                                                                             (propertize path 'face 'font-lock-warning-face)))
                                                                  (search-url nil)
                                                                  (decorated (propertize word 'face 'consult-web-default-face)))
                                                             (propertize decorated
                                                                         :source source
                                                                         :title word
                                                                         :url url
                                                                         :search-url search-url
                                                                         :query query)))

                                                         raw-results)))
                                      (funcall callback annotated-results)
                                      annotated-results)))))

(consult-web-define-source "Brave AutoSuggest"
                           :narrow-char ?B
                           :type 'async
                           :face 'consult-web-engine-source-face
                           :request #'consult-web--brave-autosuggest-fetch-results
                           :group #'consult-web--group-function
                           :on-preview #'ignore
                           :on-return #'string-trim
                           :on-callback #'string-trim
                           :search-history 'consult-web--search-history
                           :selection-history t
                           :enabled (lambda () (bound-and-true-p consult-web-brave-autosuggest-api-key))
                           :sort t
                           :dynamic t
                           )

;;; provide `consult-web-brave-autosuggest' module

(provide 'consult-web-brave-autosuggest)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-brave-autosuggest)
;;; consult-web-brave-autosuggest.el ends here
