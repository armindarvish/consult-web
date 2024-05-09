;;; consult-web-duckduckgo.el --- Consulting DuckDuckGo -*- lexical-binding: t -*-

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

(defvar consult-web-duckduckgo-api-url "http://api.duckduckgo.com/")

(defvar consult-web-duckduckgo-search-url "https://duckduckgo.com/")

(cl-defun consult-web--duckduckgoapi-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetch search results got INPUT from DuckDuckGo limited API.

See URL `https://duckduckgo.com/duckduckgo-help-pages/settings/params/'
for some limited documentation"
  (pcase-let* ((`(,query . ,opts) (consult-web--split-command input))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (page (plist-get opts :page))
               (extra-args (seq-difference (append opts args) '(:count count :page page)))
               (count (or (and (integerp count) count)
                          (and count (string-to-number (format "%s" count)))
                          consult-web-default-count))
               (page (or (and (integerp page) page)
                         (and page (string-to-number (format "%s" page)))
                         consult-web-default-page))
               (count (min count 10))
               (page (+ (* page count) 1))
               (params `(("q" . ,input)
                   ("format" . "json")))
               (headers `(("Accept" . "application/json"))))
    (consult-web--fetch-url consult-web-duckduckgo-api-url consult-web-http-retrieve-backend
                            :encoding 'utf-8
                            :params params
                            :headers headers
                            :parser #'consult-web--default-url-parse-buffer
                            :callback
                            (lambda (attrs)
                              (let* ((raw-results (gethash "RelatedTopics" attrs))
                                     (annotated-results
                                           (mapcar (lambda (item)
                                                     (let*
                                                         ((source "DuckDuckGo API")
                                                          (url (gethash "FirstURL" item))
                                                          (title (gethash "Result" item))
                                                          (title (if (string-match "<a href=.*>\\(?1:.*\\)</a>.*" title) (match-string 1 title) ""))
                                                          (snippet (format "%s" (gethash "Text" item)))

                                                          (search-url (consult-web--make-url-string consult-web-duckduckgo-search-url params '("format")))

                                                          (decorated (funcall consult-web-default-format-candidate :source source :query query :url url :search-url search-url :title title :snippet snippet :face 'consult-web-engine-source-face)))
                                                       (propertize decorated
                                                                   :source source
                                                                   :title title
                                                                   :url url
                                                                   :search-url search-url
                                                                   :query query
                                                                   )))

                                                   raw-results)))
                                (when annotated-results
                                  (funcall callback annotated-results))
                                annotated-results)))))


(consult-web-define-source "DuckDuckGo API"
                           :narrow-char ?d
                           :type 'async
                           :face 'consult-web-engine-source-face
                           :request #'consult-web--duckduckgoapi-fetch-results
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :enabled (lambda () (bound-and-true-p consult-web-duckduckgo-search-url))
                           :group #'consult-web--group-function
                           :sort t
                           :dynamic t
                           :annotate nil
                           )

;;; provide `consult-web-duckduckgo' module

(provide 'consult-web-duckduckgo)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-duckduckgo)
;;; consult-web-duckduckgo.el ends here
