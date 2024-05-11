;;; consult-web-browser-history.el --- Consulting Browser History -*- lexical-binding: t -*-

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
(require 'browser-hist nil t)

(cl-defun consult-web--browser-history-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetch search results for INPUT from browser history.
"
 (pcase-let* ((`(,query . ,opts) (consult-web--split-command input))
               (opts (car-safe opts))
               (browser (or (plist-get opts :browser) browser-hist-default-browser))
               (browser-hist-default-browser browser)
               (results (browser-hist--send-query query))
               (source "Browser History"))
      (mapcar (lambda (item)
                (let* ((url (car-safe item))
                      (title (cdr-safe item))
                      (decorated (funcall consult-web-default-format-candidate :source source :query query :url url :title title)))
                  (propertize decorated
                              :source source
                              :title title
                              :url url
                              :query query)))
              results)))

(consult-web-define-source "Browser History"
                           :narrow-char ?h
                           :type 'sync
                           :face 'consult-web-engine-source-face
                           :request #'consult-web--browser-history-fetch-results
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :enabled (lambda () (fboundp 'browser-hist-search))
                           :group #'consult-web--group-function
                           :sort t
                           :dynamic 'both
                           :annotate nil
                           )

;;; provide `consult-web-browser-history' module

(provide 'consult-web-browser-history)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-browser-history)
;;; consult-web-browser-history.el ends here
