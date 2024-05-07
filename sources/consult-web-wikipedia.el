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

(defvar consult-web-wikipedia-search-url "https://www.wikipedia.org/search-redirect.php")
(defvar consult-web-wikipedia-url "https://wikipedia.org/")
(defvar consult-web-wikipedia-api-url "https://wikipedia.org/w/api.php")

(cl-defun consult-web--wikipedia-fetch-results (input &rest args &key callback &allow-other-keys)
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
      :parser #'consult-web--default-url-parse-buffer
      :callback
      (lambda (attrs)
        (when-let* ((raw-results (map-nested-elt attrs '("query" "search")))
                    (titles  (mapcar (lambda (item) (format "%s" (gethash "title" item))) raw-results))
                    (annotated-results
                     (mapcar (lambda (item)
                               (let*
                                   ((source "Wikipedia")
                                    (url (concat consult-web-wikipedia-url "wiki/" (string-replace " " "_" item)))
                                    (title (format "%s" item))
                                    (title (and (stringp title)
                                                (propertize title 'face 'consult-web-engine-source-face)))
                                    (urlobj (and url (url-generic-parse-url url)))
                                    (domain (and (url-p urlobj) (url-domain urlobj)))
                                    (domain (and (stringp domain)
                                                 (propertize domain 'face 'font-lock-variable-name-face)))
                                    (path (and (url-p urlobj) (url-filename urlobj)))
                                    (path (and (stringp path)
                                               (propertize path 'face 'font-lock-warning-face)))
                                    (search-url (concat  consult-web-wikipedia-search-url "?" "search=" query))

                                    (decorated (concat title "\t"
                                                       (propertize " " 'display '(space :align-to center))
                                                       domain path " "
                                                       )))
                                 (propertize decorated
                                             :source source
                                             :title title
                                             :url url
                                             :search-url search-url
                                             :query query)))

                             titles)))
          (funcall callback annotated-results)
          annotated-results)))))


(consult-web-define-source "Wikipedia"
                           :narrow-char ?w
                           :type 'async
                           :face 'consult-web-engine-source-face
                           :request #'consult-web--wikipedia-fetch-results
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :enabled (lambda () "true")
                           :group #'consult-web--group-function
                           :sort t
                           :type 'async
                           :dynamic 'both
                            )

;;; provide `consult-web-wikipedia' module

(provide 'consult-web-wikipedia)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-wikipedia)
;;; consult-web-wikipedia.el ends here
