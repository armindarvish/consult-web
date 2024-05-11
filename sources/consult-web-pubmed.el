;;; consult-web-pubmed.el --- Consulting PubMed -*- lexical-binding: t -*-

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

(defcustom consult-web-pubmed-api-key nil
  "Key for Pubmed Entrez API.

See URL `https://www.ncbi.nlm.nih.gov/books/NBK25501/' for more info"
  :group 'consult-web
  :type '(choice (const :tag "API Key" string)
                 (function :tag "Custom Function")))

(defvar consult-web-pubmed-search-url "https://pubmed.ncbi.nlm.nih.gov/")

(defvar  consult-web-pubmed-esearch-api-url "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi")


(cl-defun consult-web--pubmed-esearch-fetch-results (input &rest args &key db &allow-other-keys)
  "Fetches “esearch” results for INPUT from PubMed Entrez Utilities service.

DB is passed as db in query parameters. (This is the databes to search.)

Refer to URL `https://www.ncbi.nlm.nih.gov/books/NBK25501/'
for more info."

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
               (count (min count 20))
               (page (* page count))
               (db (if db (format "%s" db) "pubmed"))
               (params `(("db" . ,db)
                         ("term" . ,(replace-regexp-in-string " " "+" query))
                         ("usehistory" . "y")
                         ("retmax" . ,(format "%s" count))
                         ("retstart" . ,(format "%s" page))
                         ("retmode" . "json")
                         ))
               (headers `(("tool" . "consult-web")
                          ("email" . "contact@armindarvish.com")
                          ("api_key" . ,(consult-web-expand-variable-function consult-web-pubmed-api-key)))))
    (consult-web--fetch-url
     consult-web-pubmed-esearch-api-url consult-web-http-retrieve-backend
     :sync t
     :params params
     :headers headers
     :parser #'consult-web--json-parse-buffer
     :callback
     (lambda (attrs)
       (let* ((results (gethash "esearchresult" attrs))
              (webenv (gethash "webenv" results))
              (qk (gethash "querykey" results))
              (idlist (gethash "idlist" results)))
         `(:webenv ,webenv :qk ,qk :idlist ,idlist)
         )))
    ))

(defvar consult-web-pubmed-esummary-api-url "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi")

(cl-defun consult-web--pubmed-esummary-fetch-results (input &rest args &key callback webenv qk db &allow-other-keys)
  "Fetches “esummary” results for INPUT from PubMed Entrez Utilities service.

WEBENV is passed as webenv in query parameters

qk is passed as qk in query parameters

DB is passed as db in query parameters. (This is the databes to search.)

Refer to URL `https://www.ncbi.nlm.nih.gov/books/NBK25501/'
for more info."

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
               (page (* page count))
               (webenv (if webenv (format "%s" webenv)))
               (qk (if qk (format "%s" qk)))
               (retmax (min count 500))
               (retstart (max 0 page))
               (db (if db (format "%s" db) "pubmed"))
               (params `(("db" . ,db)
                         ("query_key" . ,qk)
                         ("WebEnv" . ,webenv)
                         ("retmax" . ,(format "%s" retmax))
                         ("retstart" . ,(format "%s" retstart))
                         ("retmode" . "json")
                         ))
               (headers `(("tool" . "consult-web")
                          ("email" . "contact@armindarvish.com")
                          ("api_key" . ,(consult-web-expand-variable-function consult-web-pubmed-api-key)))))
    (consult-web--fetch-url consult-web-pubmed-esummary-api-url consult-web-http-retrieve-backend
                            :encoding 'utf-8
                            :params params
                            :headers headers
                            :parser #'consult-web--json-parse-buffer
                            :callback
                            (lambda (attrs)
                              (let* ((results (gethash "result" attrs))
                                     (uids (gethash "uids" results))
                                     (annotated-results
                                           (mapcar (lambda (uid)
                                                     (let*
                                                         ((source "PubMed")
                                                          (url (url-unhex-string (concat consult-web-pubmed-search-url (format "%s" uid))))
                                                          (search-url (consult-web--make-url-string consult-web-pubmed-search-url `(("term" . ,(replace-regexp-in-string " " "+" query)))))
                                                          (data (gethash uid results))
                                                          (title (gethash "title" data))
                                                          (pubdate (date-to-time (gethash "pubdate" data)))
                                                          (date (format-time-string "%Y-%m-%d" pubdate))
                                                          (journal (gethash "fulljournalname" data))
                                                          (authors (mapcar (lambda (item) (gethash "name" item)) (gethash "authors" data)))
                                                          (ids (gethash "articleids" data))
                                                          (doi (car (remove nil (mapcar (lambda (item) (if (equal (gethash "idtype" item) "doi") (gethash "value" item))) ids))))

                                                          (decorated (consult-web-dynamic--pubmed-format-candidate :source source :query query :url url :search-url search-url :title title :authors authors :date date :journal journal :doi doi)))
                                                       (propertize decorated
                                                                   :source source
                                                                   :url url
                                                                   :title title
                                                                   :search-url search-url
                                                                   :query query
                                                                   :journal journal
                                                                   :authors authors
                                                                   :date date
                                                                   :doi doi)))
                                                   uids)))
                                (when annotated-results
                                  (funcall callback annotated-results)
                                    )
                                annotated-results)))))

(cl-defun consult-web-dynamic--pubmed-format-candidate (&rest args &key source query url search-url title authors date journal doi face &allow-other-keys)
  "Returns a formatted string for candidates of `consult-web-pubmed'.

SOURCE is the name to use (e.g. “PubMed”)

QUERY is the query input from the user

URL is the url of  candidate

SEARCH-URL is the web search url
(e.g. https://pubmed.ncbi.nlm.nih.gov/?term=QUERY)

TITLE is the title of the result/paper (e.g. title of paper)

AUTHORS are authors of the result/paper

DATE is the publish date of the result/paper

JOURNAL is the journal that the result/paper is published in

DOI is doi of the result/paper

FACE is the face to apply to TITLE
"
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (source (if (stringp source) (propertize source 'face 'consult-web-source-face) nil))
         (date (if (stringp date) (propertize date 'face 'consult-web-date-face) nil))
         (journal (if (stringp journal) (propertize journal 'face 'consult-web-domain-face) nil))
         (authors (cond
                   ((and authors (listp authors))
                    (concat (first authors) ",..., " (car (last authors))))
                   ((stringp authors)
                    authors)
                   (t nil)))
         (authors (if (and authors (stringp authors)) (propertize authors 'face 'consult-web-source-face)))
         (doi (if (stringp doi) (propertize doi 'face 'link)))
         (match-str (if (stringp query) (consult--split-escaped query) nil))
         (face (or (consult-web--get-source-prop source :face) face 'consult-web-default-face))
         (title-str (propertize title 'face face))
         (title-str (consult-web--set-string-width title-str (* 5 frame-width-percent)))
         (str (concat title-str
                      (if journal (format "\t%s" journal))
                      (if date (format "\s\s%s" date))
                      (if authors (format "\s\s%s" authors))
                      (if source (concat "\t" source))))
         )
    (if consult-web-highlight-matches
        (cond
         ((listp match-str)
          (mapcar (lambda (match) (setq str (consult-web--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-web--highlight-match match-str str t)))))
    str))

(cl-defun consult-web--pubmed-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetches results for INPUT from PubMed using Entrez Utilities service.
"
(let* ((esearch (consult-web--pubmed-esearch-fetch-results input))
       (webenv (plist-get esearch :webenv))
       (qk (plist-get esearch :qk)))
  (consult-web--pubmed-esummary-fetch-results input :callback callback :webenv webenv :qk qk)
))


(consult-web-define-source "PubMed"
                           :narrow-char ?p
                           :type 'async
                           :category 'consult-web-scholar
                           :face 'consult-web-scholar-source-face
                           :request #'consult-web--pubmed-fetch-results
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :enabled (lambda () (bound-and-true-p consult-web-pubmed-api-key))
                           :group #'consult-web--group-function
                           :sort t
                           :dynamic 'both
                           :annotate nil
                           )

;;; provide `consult-web-pubmed' module

(provide 'consult-web-pubmed)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-pubmed)
;;; consult-web-pubmed.el ends here
