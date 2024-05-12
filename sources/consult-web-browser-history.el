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

(cl-defun consult-web--browser-history-format-candidate (&rest args &key source query url search-url title face &allow-other-keys)
  "Returns a highlighted formatted string for candidates.

SOURCE is the name string of the source for candidate

QUERY is the query string used for searching

URL is a string pointing to url of the candidate

SEARCH-URL is a string pointing to the url for
the search results of QUERY on the SOURCE website

TITLE is the title of the candidate

SNIPPET is a string containing a snippet/description of candidate
"
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (source (and (stringp source) (propertize source 'face 'consult-web-source-face)))
         (match-str (and (stringp query) (consult--split-escaped query) nil))
         (face (or (consult-web--get-source-prop source :face) face 'consult-web-default-face))
         (title-str (propertize title 'face face))
         (title-str (consult-web--set-string-width title-str (* 4 frame-width-percent)))
         (urlobj (and url (url-generic-parse-url url)))
         (domain (and (url-p urlobj) (url-domain urlobj)))
         (domain (and (url-p urlobj) (or (url-domain urlobj) (url-host urlobj))))
         (port (and (url-p urlobj) (url-port urlobj)))
         (domain (if port (format "%s:%s" domain port) (format "%s" domain)))
         (domain (and (stringp domain) (propertize domain 'face 'consult-web-domain-face)))
         (path (and (url-p urlobj) (url-filename urlobj)))
         (path (and (stringp path) (propertize path 'face 'consult-web-path-face)))
         (url-str (consult-web--set-url-width domain path (* frame-width-percent 5)))
         (str (concat title-str
                      (when url-str (concat "\s" url-str))
                      (when source (concat "\t" source)))))
    (if consult-web-highlight-matches
        (cond
         ((listp match-str)
          (mapcar (lambda (match) (setq str (consult-web--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-web--highlight-match match-str str t)))))
    str))

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
                      (decorated (consult-web--browser-history-format-candidate :source source :query query :url url :title title)))
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
