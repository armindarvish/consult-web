;;; consult-web-google-autosuggest.el --- Consulting Google Autosuggest -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.2
;; Package-Requires: ((emacs "28.1") (consult "1.1") (consult-web "0.2"))
;; Homepage: https://github.com/armindarvish/consult-web
;; Keywords: convenience

;;; Commentary:

;;; Code:

(require 'consult-web)

(defvar consult-web-google-autosuggest-api-url "http://suggestqueries.google.com/complete/search")

(cl-defun consult-web--google-autosuggest-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetch search results for INPUT from Google Autosuggest.

Uses `consult-web-google-autosuggest-api-url' as autosuggest api url."
  (pcase-let* ((`(,query . ,opts) (consult-web--split-command input args))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (page (plist-get opts :page))
               (count (or (and (integerp count) count)
                          (and count (string-to-number (format "%s" count)))
                          consult-web-default-count))
               (page (or (and (integerp page) page)
                         (and page (string-to-number (format "%s" page)))
                         consult-web-default-count))
               (params `(("q" . ,query)
                         ("client" . "chrome")))
               (headers `(("Accept" . "application/json"))))
    (consult-web--fetch-url consult-web-google-autosuggest-api-url consult-web-http-retrieve-backend
                            :params params
                            :headers headers
                            :parser #'consult-web--json-parse-buffer
                            :callback
                            (lambda (attrs)
                              (when-let* ((raw-results (append (list (car-safe attrs)) (car-safe (cdr-safe attrs))))
                                          (annotated-results
                                           (mapcar (lambda (item)
                                                     (let* ((source "Google AutoSuggest")
                                                            (word item)
                                                            (url                                  (concat "https://www.google.com/search?q="  (replace-regexp-in-string " " "+" word)))
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

(consult-web-define-source "Google AutoSuggest"
                           :narrow-char ?G
                           :type 'dynamic
                           :face 'consult-web-engine-source-face
                           :request #'consult-web--google-autosuggest-fetch-results
                           :on-preview #'ignore
                           :on-return #'identity
                           :on-callback #'string-trim
                           :search-history 'consult-web--search-history
                           :selection-history t
                           :group #'consult-web--group-function
                           :enabled (lambda () (boundp consult-web-google-autosuggest-api-url))
                           :sort t
                           :static t
                           )

;;; provide `consult-web-google-autosuggest' module

(provide 'consult-web-google-autosuggest)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-google-autosuggest)
;;; consult-web-google-autosuggest.el ends here
