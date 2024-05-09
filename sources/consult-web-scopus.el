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

(defun consult-web--scopus-format-candidate (source query url search-url title authors date journal doi)
  "Returns a formatted string for candidates of `consult-web-scopus'.

TABLE is a hashtable from `consult-web--scopus-fetch-results'."
  (let* ((source (if (stringp source) (propertize source 'face 'consult-web-source-face) nil))
         (date (if (stringp date) (propertize date 'face 'consult-web-date-face) nil))
         (journal (if (stringp journal) (propertize journal 'face 'consult-web-domain-face) nil))
         (authors (cond
                   ((and authors (listp authors))
                    (concat (first authors) ",..., " (car (last authors))))
                   ((stringp authors)
                    authors)
                   (t nil)
                   ))
         (authors (if (and authors (stringp authors)) (propertize authors 'face 'consult-web-source-face)))
         (doi (if (stringp doi) (propertize doi 'face 'link) nil))
         (match-str (if (stringp query) (consult--split-escaped query) nil))
         (title-str (consult-web--set-string-width title (floor (* (frame-width) 0.4))))
         (title-str (propertize title-str 'face (or 'consult-web-scholar-source-face)))
         (str (concat title-str
                      (if journal (format "\t%s" journal))
                      (if date (format "\s\s%s" date))
                      (if authors (format "\s\s%s" authors))
                      (if source (concat "\t" source)))))
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

(cl-defun consult-web--scopus-fetch-results (input &rest args &key callback &allow-other-keys)
  "Retrieve search results from SCOPUS for INPUT.
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
               (count (min (max count 1) 25))
               (page (* count page))
               (params `(("query" . ,(replace-regexp-in-string " " "+" query))
                         ("count" . ,(format "%s" count))
                         ("start" . ,(format "%s" page))
                         ("apiKey" . ,(consult-web-expand-variable-function consult-web-scopus-api-key))))
               (headers `(("Accept" . "application/json")
                          )))
    (consult-web--fetch-url consult-web-scopus-api-url consult-web-http-retrieve-backend
                            :encoding 'utf-8
                            :params params
                            :headers headers
                            :parser #'consult-web--default-url-parse-buffer
                            :callback
                            (lambda (attrs)
                              (when-let* ((raw-results (map-nested-elt attrs '("search-results" "entry")))
                                          (annotated-results
                                           (mapcar (lambda (item)
                                                     (let*
                                                         ((source "Scopus")
                                                          (title (gethash "dc:title" item))
                                                          (journal (gethash "prism:publicationName" item))
                                                          (volume (gethash "prism:volume" item))
                                                          (pages (gethash "prism:pageRange" item))
                                                          (authors (gethash "dc:creator" item))
                                                          (authors (cond
                                                                    ((stringp authors) (list authors))
                                                                    (t authors)))
                                                          (date (gethash "prism:coverDate" item))
                                                          (eid (gethash "eid" item))
                                                          (doi (gethash "prism:doi" item))
                                                          (url (concat consult-web-scopus-search-url "&eid=" eid "&origin=inward"))

                                                          (search-url (concat consult-web-scopus-search-url "&eid=" eid "&origin=inward"))

                                                          (decorated (consult-web--scopus-format-candidate source query url search-url title authors date journal doi)))
                                                       (propertize decorated
                                                                   :source source
                                                                   :url url
                                                                   :title title
                                                                   :search-url search-url
                                                                   :query query
                                                                   :journal journal
                                                                   :volume volume
                                                                   :pages pages
                                                                   :authors authors
                                                                   :date date
                                                                   :doi doi
                                                                   :eid eid)))

                                                   raw-results)))
                                (funcall callback annotated-results))))))


(consult-web-define-source "Scopus"
                           :narrow-char ?s
                           :face 'consult-web-scholar-source-face
                           :type 'async
                           :request #'consult-web--scopus-fetch-results
                           :preview-key consult-web-preview-key
                           :on-preview #'consult-web--scopus-preview
                           :on-return #'identity
                           :on-callback #'consult-web--scopus-callback
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :enabled (lambda () (bound-and-true-p consult-web-scopus-api-key))
                           :group #'consult-web--group-function
                           :sort t
                           :dynamic 'both
                           :annotate nil
                           )

;;; provide `consult-web-scopus' module

(provide 'consult-web-scopus)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-scopus)
;;; consult-web-scopus.el ends here
