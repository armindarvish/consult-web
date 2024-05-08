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
                                    (url (format "%s" (gethash "url" item)))
                                    (title (format "%s" (gethash "title" item)))
                                    (title (and (stringp title)
                                                (propertize title 'face 'consult-web-engine-source-face)))
                                    (urlobj (and url (url-generic-parse-url url)))
                                    (domain (and (url-p urlobj) (url-domain urlobj)))
                                    (domain (and (stringp domain)
                                                 (propertize domain 'face 'font-lock-variable-name-face)))
                                    (path (and (url-p urlobj) (url-filename urlobj)))
                                    (path (and (stringp path)
                                               (propertize path 'face 'font-lock-warning-face)))
                                    (search-url nil)

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

                             raw-results)))
          (funcall callback annotated-results)
          annotated-results)))))

(consult-web-define-source "Brave"
                           :narrow-char ?b
                           :type 'async
                           :face 'consult-web-engine-source-face
                           :request #'consult-web--brave-fetch-results
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :enabled (lambda () #'my:brave-key)
                           :group #'consult-web--group-function
                           :sort t
                           :dynamic 'both
                           :annotate nil
                           )

;;; provide `consult-web-brave' module

(provide 'consult-web-brave)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-brave)
;;; consult-web-brave.el ends here
