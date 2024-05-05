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


<<<<<<< HEAD
(defun consult-web--brave-fetch-results (input callback)
  ""
  (pcase-let* ((`(,query . ,args) (consult-web--split-command input))
               (args (car-safe args))
               (params `(("q" . ,(url-hexify-string query))
                         ("count" . ,(format "%s" (or (plist-get args :count) consult-web-default-count)))
                         ("page" . ,(format "%s" (or (plist-get args :page) 0)))))
               (headers `(("User-Agent" . "Emacs:consult-web/0.1 (Emacs consult-web package; https://github.com/armindarvish/consult-web)")
                          ("Accept" . "application/json")
                          ("Accept-Encoding" . "gzip")
                          ("X-Subscription-Token" . ,(consult-web-expand-variable-function consult-web-brave-api-key))
                          )))
    (consult-web--fetch-url-async consult-web-brave-url 'url
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
||||||| parent of 2f8257f (add indicator)
(cl-defun consult-web--brave-fetch-results (input &rest args &key count page &allow-other-keys)
  "Retrieve search results from Brave for INPUT.
=======
>>>>>>> 2f8257f (add indicator)

<<<<<<< HEAD
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
||||||| parent of 2f8257f (add indicator)
COUNT is passed as count in query parameters.
PAGE is passed as page in query paramters.
"
  (let* ((count (or (and (integerp count) count)
                    (and count (string-to-number (format "%s" count)))
                    consult-web-default-count))
         (page (or (and (integerp page) page)
                   (and page (string-to-number (format "%s" page)))
                   consult-web-default-count))
         (count (min count 20))
         (params `(("q" . ,(url-hexify-string input))
                   ("count" . ,(format "%s" count))
                   ("page" . ,(format "%s" page))))
         (headers `(("User-Agent" . "Emacs:consult-web/0.1 (Emacs consult-web package; https://github.com/armindarvish/consult-web)")
                    ("Accept" . "application/json")
                    ("Accept-Encoding" . "gzip")
                    ("X-Subscription-Token" . ,(consult-web-expand-variable-function consult-web-brave-api-key))
                    )))
    (funcall consult-web-retrieve-backend
     consult-web-brave-url
     :params params
     :headers headers
     :parser
     (lambda ()
       (goto-char (point-min))
       (let* ((results (gethash "results" (gethash "web" (json-parse-buffer))))
              (items  (mapcar (lambda (item) `(:url ,(format "%s" (gethash "url" item)) :title ,(format "%s" (gethash "title" item)))) results))
              )
         (cl-loop for a in items
                  collect
                  (let ((table (make-hash-table :test 'equal)))
                    (puthash :url
                             (plist-get a :url) table)
                    (puthash :search-url (consult-web--make-url-string consult-web-brave-search-url params) table)
                    (puthash :title
                             (plist-get a :title) table)
                    (puthash :source "Brave"
                             table)
                    (puthash :query input
                             table)
                    table
                    ))))
=======
(defun consult-web--brave-fetch-results (input callback)
  ""
  (pcase-let* ((`(,query . ,args) (cw--split-command input))
               (args (car-safe args))
               (params `(("q" . ,(url-hexify-string query))
                         ("count" . ,(format "%s" (or (plist-get args :count) cw--count)))
                         ("page" . ,(format "%s" (or (plist-get args :page) 0)))))
               (headers `(("User-Agent" . "Emacs:consult-web/0.1 (Emacs consult-web package; https://github.com/armindarvish/consult-web)")
                          ("Accept" . "application/json")
                          ("Accept-Encoding" . "gzip")
                          ("X-Subscription-Token" . ,(consult-web-expand-variable-function consult-web-brave-api-key))
                          )))
    ( consult-web--fetch-url-async consult-web-brave-url 'url
                                     :encoding 'utf-8
                                     :params params
                                     :headers headers
                                     :parser #'consult-web--default-url-parse-buffer
                                     :callback (lambda (attrs)
                                                 (when-let* ((raw-results (map-nested-elt attrs '("web" "results")))
                                                             (annotated-results  (mapcar (lambda (item)
                                                                                           (let*
                                                                                               ((url (format "%s" (gethash "url" item)))
                                                                                                (title (format "%s" (gethash "title" item)))
                                                                                                (urlobj (and url (url-generic-parse-url url)))
                                                                                                (domain (and (url-p urlobj) (url-domain urlobj)))
                                                                                                (domain (and (stringp domain)
                                                                                                             (propertize domain 'face 'font-lock-variable-name-face)))
                                                                                                (path (and (url-p urlobj) (url-filename urlobj)))
                                                                                                (path (and (stringp path)
                                                                                                           (propertize path 'face 'font-lock-warning-face)))
                                                                                                (search-url nil)
>>>>>>> 2f8257f (add indicator)

<<<<<<< HEAD
                             raw-results)))
          (funcall callback annotated-results))))))
||||||| parent of 2f8257f (add indicator)
     )))
=======
                                                                                                (decorated (concat title "\t"
                                                                                                                   (propertize " " 'display '(space :align-to center))
                                                                                                                   domain path
                                                                                                                   )))
                                                                                             (propertize decorated
                                                                                                         :source "Brave"
                                                                                                         :title title
                                                                                                         :url url
                                                                                                         :search-url search-url
                                                                                                         :query query)))

                                                                                         raw-results)))
                                                   (funcall callback annotated-results))))))
>>>>>>> 2f8257f (add indicator)

(consult-web-define-source "Brave"
                           :narrow-char ?b
                           :face 'consult-web-engine-source-face
                           :request #'consult-web--brave-fetch-results
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
<<<<<<< HEAD
                           :enabled (lambda () #'my:brave-key)
                           :group #'consult-web--group-function
                           :sort t
||||||| parent of 2f8257f (add indicator)
=======
                           :enabled (lambda () cw-brave-api-key)
                           :sort t
>>>>>>> 2f8257f (add indicator)
                           :dynamic 'both
                           :annotate nil
                           )

;;; provide `consult-web-brave' module

(provide 'consult-web-brave)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-brave)
;;; consult-web-brave.el ends here
