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

(cl-defun consult-web--pubmed-esearch-fetch-results (input &rest args &key db count page &allow-other-keys)
  "Fetches “esearch” results for INPUT from PubMed Entrez Utilities service.

COUNT is passed as retmax in query parameters.
(* PAGE COUNT) is passed as retstart in query paramters.
DB is passed as db in query parameters. (This is the databes to search.)

Refer to URL `https://www.ncbi.nlm.nih.gov/books/NBK25501/'
for more info."
  (let* ((count (or (and (integerp count) count)
                    (and count (string-to-number (format "%s" count)))
                    consult-web-default-count))
         (page (or (and (integerp page) page)
                     (and page (string-to-number (format "%s" page)))
                     consult-web-default-page))
         (count (min count 20))
         (page (* page count))
         (db (if db (format "%s" db) "pubmed"))
         (params `(("db" . ,db)
                   ("term" . ,(replace-regexp-in-string " " "+" input))
                   ("usehistory" . "y")
                   ("retmax" . ,(format "%s" count))
                   ("retstart" . ,(format "%s" page))
                   ("retmode" . "json")
                   ))
         (headers `(("tool" . "consult-web")
                    ("email" . "contact@armindarvish.com")
                    ("api_key" . ,(consult-web-expand-variable-function consult-web-pubmed-api-key)))))
    (funcall consult-web-retrieve-backend
     consult-web-pubmed-esearch-api-url
     :params params
     :headers headers
     :parser
     (lambda ()
       (goto-char (point-min))
     (let* ((results (gethash "esearchresult" (json-parse-buffer)))
            (webenv (gethash "webenv" results))
            (qk (gethash "querykey" results))
            (idlist (gethash "idlist" results)))
                    `(:webenv ,webenv :qk ,qk :idlist ,idlist)
                    )))))

(defvar consult-web-pubmed-esummary-api-url "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi")

(cl-defun consult-web--pubmed-esummary-fetch-results (input &rest args &key db qk webenv count page &allow-other-keys)
  "Fetches “esummary” results for INPUT from PubMed Entrez Utilities
service.

COUNT is passed as retmax in query parameters.
(* PAGE COUNT) is passed as retstart in query paramters.
DB is passed as db in query parameters. (This is the database to search.)

Refer to URL `https://www.ncbi.nlm.nih.gov/books/NBK25501/'
for more info.
"
  (let* ((count (or (and (integerp count) count)
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
    (funcall consult-web-retrieve-backend
     consult-web-pubmed-esummary-api-url
     :params params
     :headers headers
     :parser
     (lambda ()
       (goto-char (point-min))
       (let* ((response (json-parse-buffer))
              (results (gethash "result" response))
              (uids (gethash "uids" results))
              )
         (cl-loop for uid across uids
                  collect
                  (let* ((table (make-hash-table :test 'equal))
                         (url (concat consult-web-pubmed-search-url (format "%s" uid)))
                         (data (gethash uid results))
                         (title (gethash "title" data))
                         (pubdate (date-to-time (gethash "pubdate" data)))
                         (date (format-time-string "%Y-%m-%d" pubdate))
                         (journal (gethash "fulljournalname" data))
                         (authors (mapcar (lambda (item) (gethash "name" item)) (gethash "authors" data)))
                         (ids (gethash "articleids" data))
                         (doi (car (remove nil (mapcar (lambda (item) (if (equal (gethash "idtype" item) "doi") (gethash "value" item))) ids))))
                         )
                    (puthash :url (url-unhex-string url)
                             table)
                    (puthash :search-url (consult-web--make-url-string consult-web-pubmed-search-url `(("term" . ,(replace-regexp-in-string " " "+" input))))
                             table)
                    (puthash :title title
                             table)
                    (puthash :pmid uid
                             table)
                    (puthash :date date
                             table)
                    (puthash :journal journal
                             table)
                    (puthash :authors authors
                             table)
                    (puthash :doi doi table)
                    (puthash :source "PubMed"
                             table)
                    (puthash :query input
                             table)
                    table))
         )))))

(defun consult-web-dynamic--pubmed-format-candidate (table &optional face &rest args)
  "Returns a formatted string for candidates of `consult-web-pubmed'.

TABLE is a hashtable from `consult-web--pubmed-fetch-results'."
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
                    authors)))
         (authors (if (and authors (stringp authors)) (propertize authors 'face 'consult-web-source-face)))
         (doi (gethash :doi table))
         (doi (if (stringp doi) (propertize doi 'face 'link)))
         (match-str (if (stringp query) (consult--split-escaped (car (consult--command-split query))) nil))
         (title-str (consult-web--set-string-width title (floor (* (frame-width) 0.5))))
         (title-str (propertize title-str 'face (or face 'consult-web-scholar-source-face)))
         (extra-args (consult-web-hashtable-to-plist table '(:title :url :search-url :query :source :journal :date :authors :doi :pmid)))
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

(cl-defun consult-web--pubmed-fetch-results (input &rest args &key database count page &allow-other-keys)
  "Fetches results for INPUT from PubMed using Entrez Utilities
service.

COUNT and PAGE are passed to `consult-web--pubmed-esearch-fetch-results' and `consult-web--pubmed-esummary-fetch-results'.

DATABASE is passed as DB to `consult-web--pubmed-esearch-fetch-results' and `consult-web--pubmed-esummary-fetch-results'."
(let* ((esearch (consult-web--pubmed-esearch-fetch-results input args :db database :count count :page page))
       (webenv (plist-get esearch :webenv))
       (qk (plist-get esearch :qk)))
(consult-web--pubmed-esummary-fetch-results input :webenv webenv :qk qk :db database :count count :page page args)
))


(consult-web-define-source "PubMed"
                           :narrow-char ?p
                           :face 'consult-web-scholar-source-face
                           :request #'consult-web--pubmed-fetch-results
                           :format #'consult-web-dynamic--pubmed-format-candidate
                           :preview-key consult-web-preview-key
                           :category 'consult-web-scholar
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :dynamic 'both
                           )

;;; provide `consult-web-pubmed' module

(provide 'consult-web-pubmed)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-pubmed)
;;; consult-web-pubmed.el ends here
