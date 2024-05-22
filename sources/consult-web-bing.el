;;; consult-web-bing.el --- Consulting Bing -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.2
;; Package-Requires: ((emacs "28.1") (consult "1.1") (consult-web "0.2"))
;; Homepage: https://github.com/armindarvish/consult-web
;; Keywords: convenience

;;; Commentary:

;;; Code:
(require 'consult-web)

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
               (page (* page count))
               (params `(("q" . ,(replace-regexp-in-string " " "+" query))
                         ("count" . ,(format "%s" count))
                         ("offset" . ,(format "%s" page))))
               (headers `(("Ocp-Apim-Subscription-Key" . ,(consult-web-expand-variable-function consult-web-bing-search-api-key)))))
    (consult-web--fetch-url consult-web-bing-search-api-url consult-web-http-retrieve-backend
                            :encoding 'utf-8
                            :params params
                            :headers headers
                            :parser #'consult-web--json-parse-buffer
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
                                                     (decorated (funcall consult-web-default-format-candidate :source source :query query :url url :search-url search-url :title title :snippet snippet)))
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

(consult-web-define-source "Bing"
                           :narrow-char ?m
                           :type 'dynamic
                           :face 'consult-web-engine-source-face
                           :request #'consult-web--bing-fetch-results
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :enabled (lambda () (bound-and-true-p consult-web-bing-search-api-key))
                           :group #'consult-web--group-function
                           :sort t
                           :static 'both
                           :annotate nil
                           )

;;; provide `consult-web-bing' module

(provide 'consult-web-bing)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-bing)
;;; consult-web-bing.el ends here
