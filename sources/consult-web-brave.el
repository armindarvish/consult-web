;;; consult-web-brave.el --- Consulting Brave -*- lexical-binding: t -*-

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

(defun consult-web--brave-format-candidate (source query url search-url title snippet)
  "Returns a formatted string for candidates of `consult-web-scopus'.

TABLE is a hashtable from `consult-web--scopus-fetch-results'."
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (source (and (stringp source) (propertize source 'face 'consult-web-source-face)))
         (match-str (and (stringp query) (consult--split-escaped query) nil))
         (title-str (consult-web--set-string-width title (* 4 frame-width-percent)))
         (title-str (propertize title-str 'face 'consult-web-engine-source-face))
         (snippet (and (stringp snippet) (consult-web--set-string-width snippet (* 3 frame-width-percent))))
         (snippet (and (stringp snippet) (propertize snippet 'face 'consult-web-snippet-face)))
         (urlobj (and url (url-generic-parse-url url)))
         (domain (and (url-p urlobj) (url-domain urlobj)))
         (domain (and (stringp domain) (propertize domain 'face 'consult-web-domain-face)))
         (path (and (url-p urlobj) (url-filename urlobj)))
         (path (and (stringp path) (propertize path 'face 'consult-web-path-face)))
         (url-str (consult-web--set-url-width domain path (* frame-width-percent 2)))
         (str (concat title-str
                      (when url-str (concat "\s" url-str))
                      (when snippet (concat "\s\s" snippet))
                      (when source (concat "\t" source)))))
    (if consult-web-highlight-matches
        (cond
         ((listp match-str)
          (mapcar (lambda (match) (setq str (consult-web--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-web--highlight-match match-str str t)))))
    str))

(defvar consult-web-brave-search-url "https://search.brave.com/search")

(defvar consult-web-brave-url "https://api.search.brave.com/res/v1/web/search")

(defcustom consult-web-brave-api-key nil
  "Key for Brave API.

See URL `https://brave.com/search/api/' for more info"
  :group 'consult-web
  :type '(choice (const :tag "Brave API Key" string)
                 (function :tag "Custom Function")))


(cl-defun consult-web--brave-fetch-results (input &rest args &key callback &allow-other-keys)
  ""
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
               (count (min (max count 1) 20))
               (params `(("q" . ,(url-hexify-string query))
                         ("count" . ,(format "%s" count))
                         ("page" . ,(format "%s" page))))
               (headers `(("User-Agent" . "Emacs:consult-web/0.1 (Emacs consult-web package; https://github.com/armindarvish/consult-web)")
                          ("Accept" . "application/json")
                          ("Accept-Encoding" . "gzip")
                          ("X-Subscription-Token" . ,(consult-web-expand-variable-function consult-web-brave-api-key))
                          )))
    (consult-web--fetch-url consult-web-brave-url consult-web-http-retrieve-backend
      :encoding 'utf-8
      :params params
      :headers headers
      :parser #'consult-web--default-url-parse-buffer
      :callback
      (lambda (attrs)
        (when-let* ((raw-results (map-nested-elt attrs '("web" "results")))
                    (annotated-results
                     (mapcar (lambda (item)
                               (let*
                                   ((source "Brave")
                                    (url (gethash "url" item))
                                    (title (gethash "title" item))
                                    (snippet (gethash "description" item))
                                    (search-url (consult-web--make-url-string consult-web-brave-search-url params))
                                    (decorated (consult-web--brave-format-candidate source query url search-url title snippet)))
                                 (propertize decorated
                                             :source source
                                             :title title
                                             :url url
                                             :search-url search-url
                                             :query query
                                             :snippet snippet)))

                             raw-results)))
          (funcall callback annotated-results)
          annotated-results)))))

(consult-web-define-source "Brave"
                           :narrow-char ?b
                           :type 'async
                           :face 'consult-web-engine-source-face
                           :format #'consult-web--brave-format-candidate
                           :request #'consult-web--brave-fetch-results
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :enabled (lambda () (bound-and-true-p consult-web-brave-api-key))
                           :group #'consult-web--group-function
                           :sort t
                           :dynamic 'both
                           )

;;; provide `consult-web-brave' module

(provide 'consult-web-brave)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-brave)
;;; consult-web-brave.el ends here
