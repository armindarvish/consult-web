;;; consult-web-scopus.el --- Consulting Scopus -*- lexical-binding: t -*-

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

(defun consult-web--scopus-format-candidate (table &optional face &rest args)
  "Returns a formatted string for candidates of `consult-web-scopus'.

TABLE is a hashtable from `consult-web--scopus-fetch-results'."
  (let* ((pl (consult-web-hashtable-to-plist table))
         (title (format "%s" (gethash :title table)))
         (source (gethash :source table))
         (source (if (stringp source) (propertize source 'face 'consult-web-source-face)))
         (query (gethash :query table))
         (date (gethash :date table))
         (date (if (stringp date) (propertize date 'face 'consult-web-path-face)))
         (journal (gethash :journal table))
         (journal (if (stringp journal) (propertize journal 'face 'consult-web-domain-face)))
         (authors (gethash :authors table))
         (authors (cond
                   ((and authors (listp authors))
                    (concat (first authors) ",..., " (car (last authors))))
                   ((stringp authors)
                    authors)
                   ))
         (authors (if (and authors (stringp authors)) (propertize authors 'face 'consult-web-source-face)))
         (doi (gethash :doi table))
         (doi (if (stringp doi) (propertize doi 'face 'link)))
         (match-str (if (stringp query) (consult--split-escaped (car (consult--command-split query))) nil))
         (title-str (consult-web--set-string-width title (floor (* (frame-width) 0.4))))
         (title-str (propertize title-str 'face (or 'consult-web-scholar-source-face)))
         (extra-args (consult-web-hashtable-to-plist table '(:title :url :search-url :query :source :journal :date :volume :pages :authors :doi :pmid :eid)))
         (str (concat title-str
                      (if journal (format "\t%s" journal))
                      (if date (format "\s\s%s" date))
                      (if authors (format "\s\s%s" authors))
                      (if source (concat "\t" source))
                      (if extra-args (format "\t%s" extra-args))))
         (str (apply #'propertize str pl))
         )
    (if consult-web-highlight-matches
        (cond
         ((listp match-str)
          (mapcar (lambda (match) (setq str (consult-web--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-web--highlight-match match-str str t)))))
    str))

(defun consult-web--scopus-callback (cand)
  "Callback function for `consult-web-scopus'."
  (let* ((doi (get-text-property 0 :doi cand))
         (url (if doi (consult-web--doi-to-url doi)
                (get-text-property 0 :url cand))))
         (funcall consult-web-default-browse-function url)))

(defun consult-web--scopus-preview (cand)
   "Preview function for `consult-web-scopus'."
  (let* ((doi (get-text-property 0 :doi cand))
         (url (if doi (consult-web--doi-to-url doi)
                (get-text-property 0 :url cand))))
         (funcall consult-web-default-preview-function url)))

(defvar consult-web-scopus-search-url "https://www.scopus.com/record/display.uri?")

(defvar consult-web-scopus-api-url "https://api.elsevier.com/content/search/scopus")

(defcustom consult-web-scopus-api-key nil
  "Key for Scopus API.

See URL `https://dev.elsevier.com/documentation/SCOPUSSearchAPI.wadl' for more info"
  :group 'consult-web
  :type '(choice (const :tag "Scopus API Key" string)
                 (function :tag "Custom Function")))


(cl-defun consult-web--scopus-fetch-results (input &rest args &key count page &allow-other-keys)
  "Retrieve search results from SCOPUS for INPUT.

COUNT is passed as count in query parameters.
(* PAGE COUNT) is passed as start in query paramters.
"
  (let* ((count (or (and (integerp count) count)
                    (and count (string-to-number (format "%s" count)))
                    consult-web-default-count))
         (page (or (and (integerp page) page)
                   (and page (string-to-number (format "%s" page)))
                   consult-web-default-count))
         (count (max count 1))
         (page (* count page))
         (params `(("query" . ,(replace-regexp-in-string " " "+" input))
                   ("count" . ,(format "%s" count))
                   ("start" . ,(format "%s" page))
                   ("apiKey" . ,(consult-web-expand-variable-function consult-web-scopus-api-key))))
         (headers `(("Accept" . "application/json")
                    )))
    (funcall consult-web-retrieve-backend
     consult-web-scopus-api-url
     :params params
     :headers headers
     :parser
     (lambda ()
       (goto-char (point-min))
       ;; (buffer-substring (point-min) (point-max))
       (let* ((content (json-parse-buffer))
              (results (gethash "search-results" content))
              (items  (gethash "entry" results)))
         (cl-loop for a across items
                  collect
                  (let* ((table (make-hash-table :test 'equal))
                        (title (gethash "dc:title" a))
                        (journal (gethash "prism:publicationName" a))
                        (volume (gethash "prism:volume" a))
                        (pages (gethash "prism:pageRange" a))
                        (authors (gethash "dc:creator" a))
                        (authors (cond
                                  ((stringp authors) (list authors))
                                  (t authors)))
                        (date (gethash "prism:coverDate" a))
                        (eid (gethash "eid" a))
                        (doi (gethash "prism:doi" a))
                        (url (concat consult-web-scopus-search-url "&eid=" eid "&origin=inward"))

                        (search-url (concat consult-web-scopus-search-url "&eid=" eid "&origin=inward"))
                        )
                    (puthash :url url
                             table)
                    (puthash :search-url search-url
                             table)
                    (puthash :title title
                             table)
                    (puthash :journal journal
                             table)
                    (puthash :volume volume
                             table)
                    (puthash :pages pages
                             table)
                    (puthash :date date
                             table)
                    (puthash :authors authors
                             table)
                    (puthash :doi doi
                             table)
                    (puthash :eid eid
                             table)
                    (puthash :source "Scopus"
                             table)
                    (puthash :query input
                             table)
                    table
                    )))
       )

     )))


(consult-web-define-source "Scopus"
                           :narrow-char ?s
                           :face 'consult-web-scholar-source-face
                           :request #'consult-web--scopus-fetch-results
                           :format #'consult-web--scopus-format-candidate
                           :preview-key consult-web-preview-key
                           :on-preview #'consult-web--scopus-preview
                           :on-return #'identity
                           :on-callback #'consult-web--scopus-callback
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :dynamic 'both)

;;; provide `consult-web-scopus' module

(provide 'consult-web-scopus)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-scopus)
;;; consult-web-scopus.el ends here
