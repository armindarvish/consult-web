;;; consult-web-doi.el --- Consulting DOI.org -*- lexical-binding: t -*-

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

(defvar consult-web-doiorg-api-url "https://doi.org/api/handles/")

(defvar consult-web-doiorg-search-url "https://doi.org/")

(defun consult-web--doi-to-url (doi)
  "Converts DOI value to target url"
  (let ((out))
    (let* ((doi (if doi (format "%s" doi)))
           (url (concat consult-web-doiorg-api-url doi)))
      (consult-web--url-response-body
       (consult-web--fetch-url url consult-web-http-retrieve-backend
                               :sync t
                               :encoding 'utf-8
                               :parser #'consult-web--default-url-parse-buffer
                               :callback
                               (lambda (attrs)
                                 (when-let* ((raw-results (map-nested-elt attrs '("values")))
                                             (result (car-safe (remove nil (mapcar (lambda (item)
                                                                                     (if-let* ((type (gethash "type" item))                                                                                                        (link (if (equal type "URL") (map-nested-elt item '("data" "value")))))
                                                                                         link))
                                                                                   raw-results)))))
                                   result)))))))


(cl-defun consult-web--doiorg-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetch target url of DOI.
"
  (pcase-let* ((`(,query . ,opts) (consult-web--split-command input))
               (opts (car-safe opts))
               (url (consult-web--doi-to-url query)))
    (if url
        (let* ((source "doiorg")
               (title (format "%s" query))
               (title (and (stringp title)
                           (propertize title 'face 'consult-web-engine-source-face)))
               (urlobj (and url (url-generic-parse-url url)))
               (domain (and (url-p urlobj) (url-domain urlobj)))
               (domain (and (stringp domain)
                            (propertize domain 'face 'font-lock-variable-name-face)))
               (path (and (url-p urlobj) (url-filename urlobj)))
               (path (and (stringp path)
                          (propertize path 'face 'font-lock-warning-face)))
               (search-url (concat consult-web-doiorg-search-url query))
               (decorated (concat title "\t"
                                             (propertize " " 'display '(space :align-to center))
                                             domain path " "
                                             ))
               (annotated-results (propertize decorated
                                              :source source
                                              :title title
                                              :url url
                                              :search-url search-url
                                              :query query)))

          (funcall callback (list annotated-results))
          annotated-results
          ))))


(defvar consult-web--doi-search-history (list)
  "History variables for search terms when using
`consult-web-doi' commands.")

(defvar consult-web--doi-selection-history (list)
  "History variables for selected items when using
`consult-web-doi' commands.")


(consult-web-define-source "doiorg"
                           :narrow-char ?d
                           :type 'async
                           :face 'link
                           :request #'consult-web--doiorg-fetch-results
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--doi-search-history
                           :selection-history 'consult-web--doi-selection-history
                           :enabled (lambda () (bound-and-true-p consult-web-doiorg-search-url))
                           :group #'consult-web--group-function
                           :sort t
                           :dynamic 'both
                           )

;;; provide `consult-web-doi' module

(provide 'consult-web-doi)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-doi)
;;; consult-web-doi.el ends here
