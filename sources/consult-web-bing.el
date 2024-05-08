;;; consult-web-bing.el --- Consulting Bing -*- lexical-binding: t -*-

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

(defun consult-web--bing-format-candidate (source query url search-url title)
  "Returns a formatted string for candidates of `consult-web-scopus'.

TABLE is a hashtable from `consult-web--scopus-fetch-results'."
  (let* ((source (if (stringp source) (propertize source 'face 'consult-web-source-face) nil))
         (title (and (stringp title) (propertize title 'face 'consult-web-engine-source-face)))
         (urlobj (and url (url-generic-parse-url url)))
         (domain (and (url-p urlobj) (url-domain urlobj)))
         (domain (and (stringp domain) (propertize domain 'face 'font-lock-variable-name-face)))
         (path (and (url-p urlobj) (url-filename urlobj)))
         (path (and (stringp path) (propertize path 'face 'font-lock-warning-face)))
         (match-str (if (stringp query) (consult--split-escaped query) nil))
         (title-str (consult-web--set-string-width title (floor (* (frame-width) 0.4))))
         (title-str (propertize title-str 'face (or 'consult-web-scholar-source-face)))
         (str (concat title "\t"
                      (propertize " " 'display '(space :align-to center))
                      (if domain domain)
                      (if path path)
                      (if source (concat "\t" source)))))
    (if consult-web-highlight-matches
        (cond
         ((listp match-str)
          (mapcar (lambda (match) (setq str (consult-web--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-web--highlight-match match-str str t)))))
    str))

(defvar consult-web-bing-search-api-url "https://api.bing.microsoft.com/v7.0/search")

(defcustom consult-web-bing-search-api-key nil
"Key for Bing (Microsoft Azure) search API

See URL `https://www.microsoft.com/en-us/bing/apis/bing-web-search-api' and URL `https://learn.microsoft.com/en-us/bing/search-apis/bing-web-search/search-the-web' for details"
:group 'consult-web
:type '(choice (const :tag "API Key" string)
               (function :tag "Custom Function")))

(cl-defun consult-web--bing-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetches search results for INPUT from Bing web search api.

Refer to URL `https://programmablesearchengine.google.com/about/' and `https://developers.google.com/custom-search/' for more info.
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
                (count (max count 1))
         (page (* page count))
         (params `(("q" . ,(replace-regexp-in-string " " "+" input))
                   ("count" . ,(format "%s" count))
                   ("offset" . ,(format "%s" page))))
         (headers `(("Ocp-Apim-Subscription-Key" . ,(consult-web-expand-variable-function consult-web-bing-search-api-key)))))
    (consult-web--fetch-url consult-web-bing-search-api-url consult-web-http-retrieve-backend
      :encoding 'utf-8
      :params params
      :headers headers
      :parser #'consult-web--default-url-parse-buffer
      :callback
      (lambda (attrs)
        (let* ((raw-results (map-nested-elt attrs '("webPages" "value")))
                    (search-url (gethash "webSearchUrl" attrs))
                    (annotated-results
                     (mapcar (lambda (item)
                               (let*
                                   ((source "Bing")
                                    (url (format "%s" (gethash "url" item)))
                                    (title (gethash "name" item))
                                    (snippet (gethash "snippet" item))
                                    (decorated (consult-web--bing-format-candidate source query url search-url title)))
                                 (propertize decorated
                                             :source source
                                             :title title
                                             :url url
                                             :search-url search-url
                                             :query query)))

                             raw-results)))
          (when annotated-results
            (funcall callback annotated-results))
          annotated-results)))))

(consult-web-define-source "Bing"
                           :narrow-char ?m
                           :type 'async
                           :face 'consult-web-engine-source-face
                           :request #'consult-web--bing-fetch-results
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :enabled (lambda () #'my:bing-api-key)
                           :group #'consult-web--group-function
                           :sort t
                           :dynamic 'both
                           :annotate nil
                           )

;;; provide `consult-web-bing' module

(provide 'consult-web-bing)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-bing)
;;; consult-web-bing.el ends here
