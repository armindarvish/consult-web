;;; consult-web-wikipedia.el --- Consulting Wikipedia -*- lexical-binding: t -*-

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

(cl-defun consult-web--wikipedia-format-candidate (&rest args &key source query url search-url title snippet date face &allow-other-keys)
  "Returns a formatted string for Wikipedia's searches.

SOURCE is the name string of the source for candidate

QUERY is the query string used for searching

URL is a string pointing to url of the candidate

SEARCH-URL is the web search url e.g.:
https://www.wikipedia.org/search-redirect.php?search=query

TITLE is the title of the candidate

SNIPPET is a string containing a snippet/description of candidate

DATE is the date the article was last updated

FACE is the face to apply to TITLE
"
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (source (and (stringp source) (propertize source 'face 'consult-web-source-face)))
         (date (and (stringp date) (propertize date 'face 'consult-web-date-face)))
         (match-str (and (stringp query) (consult--split-escaped query) nil))
         (face (or (consult-web--get-source-prop source :face) face 'consult-web-default-face))
         (title-str (propertize title 'face face))
         (title-str (consult-web--set-string-width title-str (* 3 frame-width-percent)))
         (snippet (and (stringp snippet) (consult-web--set-string-width (string-trim snippet) (* 6 frame-width-percent))))
         (snippet (and (stringp snippet) (propertize snippet 'face 'consult-web-snippet-face)))
         (str (concat title-str
                      (when date (concat "\s" date))
                      (when snippet (concat "\s\s" snippet))
                      (when source (concat "\t" source)))))
    (if consult-web-highlight-matches
        (cond
         ((listp match-str)
          (mapcar (lambda (match) (setq str (consult-web--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-web--highlight-match match-str str t)))))
    str))

(defvar consult-web-wikipedia-search-url "https://www.wikipedia.org/search-redirect.php")
(defvar consult-web-wikipedia-url "https://wikipedia.org/")
(defvar consult-web-wikipedia-api-url "https://wikipedia.org/w/api.php")

(cl-defun consult-web--wikipedia-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetches search results from Wikipedia for INPUT.
"

  (pcase-let* ((`(,query . ,opts) (consult-web--split-command input args))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (page (plist-get opts :page))
               (count (or (and (integerp count) count)
                          (and count (string-to-number (format "%s" count)))
                          consult-web-default-count))
               (page (or (and (integerp page) page)
                         (and page (string-to-number (format "%s" page)))
                         consult-web-default-page))
               (count (max count 1))
               (params `(("action" . "query")
                 ("format" . "json")
                 ("list" . "search")
                 ("formatversion" . "2")
                 ("prop" . "info")
                 ("inprop" . "url")
                 ("srwhat" . "text")
                 ("srsearch" . ,(url-hexify-string query))
                 ("srlimit" . ,(format "%s" count))
                 ("sroffset" . ,(format "%s" page))))
               (headers '(("User-Agent" . "Emacs:consult-web/0.1 (https://github.com/armindarvish/consult-web);"))))
    (consult-web--fetch-url consult-web-wikipedia-api-url consult-web-http-retrieve-backend
      :encoding 'utf-8
      :params params
      :headers headers
      :parser #'consult-web--json-parse-buffer
      :callback
      (lambda (attrs)
        (when-let* ((raw-results (map-nested-elt attrs '("query" "search")))
                    (annotated-results
                     (mapcar (lambda (item)
                               (let*
                                   ((source "Wikipedia")
                                    (title (format "%s" (gethash "title" item)))
                                    (url (concat consult-web-wikipedia-url "wiki/" (string-replace " " "_" title)))
                                    (date (gethash "timestamp" item))
                                    (date (format-time-string "%Y-%m-%d" (date-to-time date)))
                                    (snippet (replace-regexp-in-string "<span.*?>\\|</span>\\|&quot;" "" (format "%s" (gethash "snippet" item))))
                                    (search-url (concat  consult-web-wikipedia-search-url "?" "search=" query))
                                    (decorated (consult-web--wikipedia-format-candidate :source source :query query :url url :search-url search-url :title title :snippet snippet :date date)))
                                 (propertize decorated
                                             :source source
                                             :title title
                                             :url url
                                             :search-url search-url
                                             :query query
                                             :date date)))

                             raw-results)))
          (funcall callback annotated-results)
          annotated-results)))))


(consult-web-define-source "Wikipedia"
                           :narrow-char ?w
                           :type 'dynamic
                           :face 'consult-web-engine-source-face
                           :request #'consult-web--wikipedia-fetch-results
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :enabled (lambda () (boundp  'consult-web-wikipedia-api-url))
                           :group #'consult-web--group-function
                           :sort t
                           :type 'dynamic
                           :static 'both
                            )

;;; provide `consult-web-wikipedia' module

(provide 'consult-web-wikipedia)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-wikipedia)
;;; consult-web-wikipedia.el ends here
