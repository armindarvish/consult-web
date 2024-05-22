;;; consult-web-sources.el --- Sources for Consulting Web Search Engines -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.2
;; Homepage: https://github.com/armindarvish/consult-web
;; Keywords: convenience

;;; Commentary:

;;; Code:

(require 'consult-web)

(setq consult-web-sources--all-modules-list
      (list 'consult-web-bing
            'consult-web-brave-autosuggest
            'consult-web-brave
            'consult-web-browser-history
            'consult-web-buffer
            'consult-web-chatgpt
            'consult-web-consult-notes
            'consult-web-doi
            'consult-web-duckduckgo
            'consult-web-elfeed
            'consult-web-gh
            'consult-web-google
            'consult-web-google-autosuggest
            'consult-web-grep
            'consult-web-ripgrep
            'consult-web-gptel
            'consult-web-invidious
            'consult-web-line-multi
            'consult-web-mu4e
            'consult-web-notes
            'consult-web-notmuch
            'consult-web-pubmed
            'consult-web-scopus
            'consult-web-stackoverflow
            'consult-web-wikipedia
            'consult-web-youtube))

(defun consult-web-sources--load-module (symbol)
"Loads feature SYMBOL"
(require symbol nil t))

(defun consult-web-sources-load-modules (&optional list)
  "Loads the LIST of symbols.
If list is nil, loads `consult-web-sources-modules-to-load'and if that is nil as well, loads `consult-web-sources--all-modules-list'."
  (mapcar #'consult-web-sources--load-module (or list consult-web-sources-modules-to-load consult-web-sources--all-modules-list)))

(consult-web-sources-load-modules)

;;; provide `consult-web-sources' module

(provide 'consult-web-sources)
;;; consult-web-sources.el ends here
