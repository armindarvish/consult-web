;;; consult-web-google.el --- Consulting Google -*- lexical-binding: t -*-

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

(defun consult-web-google-format-candidate (source query url search-url title snippet)
  "Returns a formatted string for candidates of `consult-web-scopus'.

TABLE is a hashtable from `consult-web--scopus-fetch-results'."
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (source (if (stringp source) (propertize source 'face 'consult-web-source-face) nil))
         (match-str (if (stringp query) (consult--split-escaped query) nil))
         (title-str (consult-web--set-string-width title (* 4 frame-width-percent)))
         (title-str (propertize title-str 'face 'consult-web-engine-source-face))
         (snippet (if (stringp snippet) (consult-web--set-string-width snippet (* 2 frame-width-percent))))
         (snippet (and (stringp snippet) (propertize snippet 'face 'consult-web-snippet-face)))
         (urlobj (and url (url-generic-parse-url url)))
         (domain (and (url-p urlobj) (url-domain urlobj)))
         (domain (and (stringp domain) (propertize domain 'face 'consult-web-domain-face)))
         (path (and (url-p urlobj) (url-filename urlobj)))
         (path (and (stringp path) (propertize path 'face 'consult-web-path-face)))
         (path-string (concat domain path))
         (path-string (and (stringp path-string) (consult-web--set-string-width path-string (* 3 frame-width-percent))))
         (str (concat title-str
                      " "
                      (if path-string path-string)
                      (if snippet (concat "\s\s" snippet))
                      (if source (concat "\t" source)))))
    (if consult-web-highlight-matches
        (cond
         ((listp match-str)
          (mapcar (lambda (match) (setq str (consult-web--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-web--highlight-match match-str str t)))))
    str))

(defvar consult-web-google-search-url "https://www.google.com/search")

(defvar consult-web-google-customsearch-api-url "https://www.googleapis.com/customsearch/v1")

(defcustom consult-web-google-customsearch-key nil
  "Key for Google custom search API

See URL `https://developers.google.com/custom-search/' and URL `https://developers.google.com/custom-search/v1/introduction' for details"
  :group 'consult-web
  :type '(choice (const :tag "API Key" string)
                 (function :tag "Custom Function")))

(defcustom consult-web-google-customsearch-cx nil
  "CX for Google custom search API

See URL `https://developers.google.com/custom-search/' and URL `https://developers.google.com/custom-search/v1/introduction' for details"
  :group 'consult-web
  :type '(choice (const :tag "CX String" string)
                 (function :tag "Custom Function")))


(cl-defun consult-web--google-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetches search results for INPUT from “Google custom search” service.

Refer to URL `https://programmablesearchengine.google.com/about/' and `https://developers.google.com/custom-search/' for more info.
"
  (pcase-let* ((`(,query . ,opts) (consult-web--split-command input))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (page (plist-get opts :page))
               (filter (plist-get opts :filter))
               (count (or (and (integerp count) count)
                          (and count (string-to-number (format "%s" count)))
                          consult-web-default-count))
               (page (or (and (integerp page) page)
                         (and page (string-to-number (format "%s" page)))
                         consult-web-default-page))
               (filter (or (and (integerp filter) filter)
                           (and filter (string-to-number (format "%s" filter)))
                           1))
               (filter (if (member filter '(0 1)) filter 1))
               (count (min count 10))
               (page (+ (* page count) 1))
               (params `(("q" . ,(replace-regexp-in-string " " "+" input))
                         ("key" . ,(consult-web-expand-variable-function consult-web-google-customsearch-key))
                         ("cx" . ,(consult-web-expand-variable-function consult-web-google-customsearch-cx))
                         ("gl" . "en")
                         ("filter" . ,(format "%s" filter))
                         ("num" . ,(format "%s" count))
                         ("start" . ,(format "%s" page))))
               (headers '(("Accept" . "application/json")
                          ("Accept-Encoding" . "gzip")
                          ("User-Agent" . "consult-web (gzip)"))))
    (consult-web--fetch-url consult-web-google-customsearch-api-url consult-web-http-retrieve-backend
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
                                                         ((source "Google")
                                                          (url (format "%s" (gethash "link" item)))
                                                          (title (format "%s" (gethash "title" item)))
                                                          (snippet (string-trim (format "%s" (gethash "snippet" item))))
                                                          (search-url (consult-web--make-url-string consult-web-google-search-url params '("key" "cx" "gl")))

                                                          (decorated (consult-web-google-format-candidate source query url search-url title snippet)))
                                                       (propertize decorated
                                                                   :source source
                                                                   :title title
                                                                   :url url
                                                                   :search-url search-url
                                                                   :query query
                                                                   :snippet snippet)))

                                                   raw-results)))
                                (when annotated-results
                                  (funcall callback annotated-results))
                                annotated-results)))))


(consult-web-define-source "Google"
                           :narrow-char ?g
                           :type 'async
                           :face 'consult-web-engine-source-face
                           :request #'consult-web--google-fetch-results
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :enabled (lambda () #'my:google-search-api-cx-and-key)
                           :group #'consult-web--group-function
                           :sort t
                           :dynamic 'both
                           :annotate nil
                           )

;;; provide `consult-web-google' module

(provide 'consult-web-google)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-google)
;;; consult-web-google.el ends here
