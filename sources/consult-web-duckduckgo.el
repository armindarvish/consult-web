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

(defvar consult-web-duckduckgoapi-url "http://api.duckduckgoapi.com/")

(cl-defun consult-web--duckduckgoapi-fetch-results (input &rest args &key count page &allow-other-keys)
  "Fetch search results got INPUT from DuckDuckGo limited API."
  (let* ((count (or (and (integerp count) count)
                    (and count (string-to-number (format "%s" count)))
                    consult-web-default-count))
         (page (or (and (integerp page) page)
                   (and page (string-to-number (format "%s" page)))
                   consult-web-default-count))
         (count (min count 10))
         (page (+ (* page count) 1))
         (params `(("q" . ,input)
                   ("format" . "json")))
         (headers `(("Accept" . "application/json"))))
    (funcall consult-web-retrieve-backend
     consult-web-duckduckgoapi-url
     :params params
     :headers headers
     :parser (lambda ()
               (goto-char (point-min))
               (let* ((results (gethash "RelatedTopics" (json-parse-buffer)))
                      (items  (mapcar (lambda (item) `(:url ,(format "%s" (gethash "FirstURL" item))
                                                  :title ,(format "%s" (gethash "Result" item))))
                                      results)))
                 (cl-loop for a in items
                          collect
                          (let ((table (make-hash-table :test 'equal)))
                            (puthash :url
                                     (plist-get a :url) table)
                            (puthash :title (if  (string-match "<a href=.*>\\(?1:.*\\)</a"  (plist-get a :title)) (match-string 1 (plist-get a :title)) "")
                                     table)
                            (puthash :source "DuckDuckGo API"
                                     table)
                            (puthash :query input
                                     table)
                            results
                            )
                          ))))))

(consult-web-define-source "DuckDuckGo API"
                           :narrow-char ?d
                           :face 'consult-web-engine-source-face
                           :request #'consult-web--duckduckgoapi-fetch-results
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :dynamic t
                           )

;;; provide `consult-web-duckduckgo' module

(provide 'consult-web-duckduckgo)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-duckduckgo)
;;; consult-web-duckduckgo.el ends here
