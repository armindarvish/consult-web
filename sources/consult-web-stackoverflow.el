;;; consult-web-stackoverflow.el --- Consulting StackOverflow -*- lexical-binding: t -*-

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

(cl-defun consult-web--stackoverflow-format-candidate (&rest args &key source query url search-url title snippet date answered score face &allow-other-keys)
  "Returns a formatted string for Wikipedia's searches.

SOURCE is the name string of the source for candidate

QUERY is the query string used for searching

URL is a string pointing to url of the candidate

SEARCH-URL is a string pointing to the url for
the search results of QUERY on the SOURCE website

TITLE is the title of the candidate

SNIPPET is a string containing a snippet/description of candidate
"
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (source (and (stringp source) (propertize source 'face 'consult-web-source-face)))
         (date (and (stringp date) (propertize date 'face 'consult-web-date-face)))
         (answered (if answered (propertize consult-web-stackoverflow-answered-mark 'face 'consult-web-domain-face)
                     (propertize consult-web-stackoverflow-unanswered-mark 'face 'error)))
         (score (and score (propertize (format "%s" score) 'face 'consult-web-path-face)))
         (match-str (and (stringp query) (consult--split-escaped query) nil))
         (face (or (consult-web--get-source-prop source :face) face 'consult-web-default-face))
         (title-str (propertize title 'face face))
         (title-str (consult-web--set-string-width title-str (* 7 frame-width-percent)))
         (str (concat title-str
                      (when date (concat "\s" date))
                      (when answered (concat "\s" answered))
                      (when score (concat "\s" score))
                      (when source (concat "\t" source)))))
    (if consult-web-highlight-matches
        (cond
         ((listp match-str)
          (mapcar (lambda (match) (setq str (consult-web--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-web--highlight-match match-str str t)))))
    str))

(defvar consult-web-stackoverflow-search-url "https://stackoverflow.com/search")
(defvar consult-web-stackoverflow-api-url "https://api.stackexchange.com/2.3/search/advanced")
(defvar consult-web-stackoverflow-answered-mark "+")
(defvar consult-web-stackoverflow-unanswered-mark "x")

(defcustom consult-web-stackexchange-api-key nil
  "Key for Stack Exchange API.

See URL `https://api.stackexchange.com/', and URL `https://stackapps.com/' for more info"
  :group 'consult-web
  :type '(choice (const :tag "API Key" string)
                 (function :tag "Custom Function")))

(cl-defun consult-web--stackoverflow-fetch-results (input &rest args &key count page order sort &allow-other-keys)
  "Fetch search results for INPUT from StackOverflow.

COUNT is passed as pagesize in query parameters.
PAGE is passed as page in query parameters.
ORDER is passed as order in query parameters.
SORT is passed as sort in query parameters.

See URL `https://api.stackexchange.com/' for more info.
"
  (let* ((count (or (and (integerp count) count)
                    (and count (string-to-number (format "%s" count)))
                    consult-web-default-count))
         (count (min count 25))
         (page (or (and (integerp page) page)
                   (and page (string-to-number (format "%s" page)))
                   consult-web-default-page))
         (page (max page 1))
         (order (if (and order (member (format "%s" order) '("desc" "asc"))) (format "%s" order)))
         (sort (if (and sort (member (format "%s" sort) '("activity" "votes" "creation" "relevance"))) (format "%s" sort)))
         (params `(("order" . ,(or order "desc"))
                   ("sort" . ,(or sort "relevance"))
                   ("site" . "stackoverflow")
                   ("q" . ,(replace-regexp-in-string " " "+" input))
                   ("pagesize" . ,(format "%s" count))
                   ("page" . ,(format "%s" page))
                   ("key" . ,(consult-web-expand-variable-function consult-web-stackexchange-api-key)))))
    (funcall consult-web-retrieve-backend
             consult-web-stackoverflow-api-url
             :params params
             :parser
             (lambda ()
               (goto-char (point-min))
               (let* ((results (gethash "items" (json-parse-buffer)))
                      (data  (mapcar (lambda (item) `(,(format "%s" (gethash "title" item)) ,(format "%s" (gethash "link" item)))) results))
                      (table (make-hash-table :test 'equal)))
                 (cl-loop for a in data
                          collect
                          (let ((table (make-hash-table :test 'equal)))
                            (puthash :url
                                     (cadr a) table)
                            (puthash :search-url (concat consult-web-stackoverflow-search-url "?q=" input)
                                     table)
                            (puthash :title
                                     (car a) table)
                            (puthash :source "StackOverflow"
                                     table)
                            (puthash :query input
                                     table)
                            table

                            )))
               ))))

(cl-defun consult-web--stackoverflow-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetch search results for INPUT from StackOverflow.
See URL `https://api.stackexchange.com/' for more info.
"
  (pcase-let* ((`(,query . ,opts) (consult-web--split-command input))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (page (plist-get opts :page))
               (order (plist-get opts :order))
               (sort (plist-get opts :sort))
               (count (or (and (integerp count) count)
                          (and count (string-to-number (format "%s" count)))
                          consult-web-default-count))
               (page (or (and (integerp page) page)
                         (and page (string-to-number (format "%s" page)))
                         consult-web-default-page))
               (count (min count 25))
               (page (max page 1))
               (order (if (and order (member (format "%s" order) '("desc" "asc"))) (format "%s" order)))
               (sort (if (and sort (member (format "%s" sort) '("activity" "votes" "creation" "relevance"))) (format "%s" sort)))
               (params `(("order" . ,(or order "desc"))
                         ("sort" . ,(or sort "relevance"))
                         ("site" . "stackoverflow")
                         ("q" . ,(replace-regexp-in-string " " "+" query))
                         ("pagesize" . ,(format "%s" count))
                         ("page" . ,(format "%s" page))
                         ("key" . ,(consult-web-expand-variable-function consult-web-stackexchange-api-key))))
               (headers '(("Accept" . "application/json"))))
    (consult-web--fetch-url consult-web-stackoverflow-api-url consult-web-http-retrieve-backend
                            :encoding 'utf-8
                            :params params
                            :headers headers
                            :parser #'consult-web--default-url-parse-buffer
                            :callback
                            (lambda (attrs)
                              (let* ((raw-results (gethash "items" attrs))
                                     (annotated-results
                                      (mapcar (lambda (item)
                                                (let*
                                                    ((source "StackOverflow")
                                                     (url (format "%s" (gethash "link" item)))
                                                     (title (format "%s" (gethash "title" item)))
                                                     (date (gethash "last_edit_date" item))
                                                     (date (format-time-string "%Y-%m-%d" (seconds-to-time date)))
                                                     (answered (gethash "is_answered" item))
                                                     (score (gethash "score" item))
                                                     (search-url (concat consult-web-stackoverflow-search-url "?q=" input))
                                                     (decorated (consult-web--stackoverflow-format-candidate :source source :query query :url url :search-url search-url :title title :date date :answered answered :score score)))
                                                  (propertize decorated
                                                              :source source
                                                              :title title
                                                              :url url
                                                              :search-url search-url
                                                              :query query
                                                              :date date
                                                              :answered answered
                                                              :score score
                                                              )))

                                              raw-results)))
                                (when annotated-results
                                  (funcall callback annotated-results))
                                annotated-results)))))

(consult-web-define-source "StackOverflow"
                           :narrow-char ?s
                           :type 'async
                           :face 'consult-web-engine-source-face
                           :request #'consult-web--stackoverflow-fetch-results
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :dynamic 'both
                           :enabled (lambda () (bound-and-true-p consult-web-stackexchange-api-key))
                           :group #'consult-web--group-function
                           :sort t
                           :dynamic 'both
                           :annotate nil
                           )

;;; provide `consult-web-stackoverflow' module

(provide 'consult-web-stackoverflow)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-stackoverflow)
;;; consult-web-stackoverflow.el ends here
