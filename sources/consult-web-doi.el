;;; consult-web-doi.el --- Consulting DOI.org -*- lexical-binding: t -*-

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

(defvar consult-web-doiorg-api-url "https://doi.org/api/handles/")

(defvar consult-web-doiorg-search-url "https://doi.org/")

(defun consult-web--doi-to-url (doi)
  "Converts DOI value to target url"
  (let* ((doi (if doi (format "%s" doi)))
         (url (concat consult-web-doiorg-api-url doi)))
    (funcall consult-web-retrieve-backend
             url
             :parser
             (lambda ()
               (goto-char (point-min))
               (let* ((content (json-parse-buffer))
                      (items (gethash "values" content)))
                 (car (mapcar (lambda (item)
                                (if-let* ((type (gethash "type" item))
                                          (url (if (equal type "URL") (gethash "value" (gethash "data" item)))))
                                    url
                                  nil)) items)))))))


(cl-defun consult-web--doiorg-fetch-results (doi &rest args)
  "Fetch target url of DOI.
"
  (let* ((table (make-hash-table :test 'equal))
         (url (consult-web--doi-to-url doi)))
    (if url
        (progn
          (puthash :url url
                   table)
          (puthash :title doi
                   table)
          (puthash :source "doiorg"
                   table)
          (puthash :query doi
                   table)
          ))
    (list table)))

(defvar consult-web--doi-search-history (list)
  "History variables for search terms when using
`consult-web-doi' commands.")

(defvar consult-web--doi-selection-history (list)
  "History variables for selected items when using
`consult-web-doi' commands.")


(consult-web-define-source "doiorg"
                           :narrow-char ?d
                           :face 'link
                           :request #'consult-web--doiorg-fetch-results
                           :search-history 'consult-web--doi-search-history
                           :selection-history 'consult-web--doi-selection-history
                           :dynamic t
                           )

;;; provide `consult-web-doi' module

(provide 'consult-web-doi)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-doi)
;;; consult-web-doi.el ends here
