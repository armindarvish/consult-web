;;; consult-web-sources.el --- Sources for Consulting Web Search Engines -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.1
;; Homepage: https://github.com/armindarvish/consult-web
;; Keywords: convenience

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'consult-web)
)

(setq consult-web-sources--all-modules-list
      (list 'consult-web-brave-autosuggest
            'consult-web-brave
            'consult-web-doi
            'consult-web-pubmed
            'consult-web-scopus
            'consult-web-wikipedia))

(defun consult-web-sources--load-module (symbol)
"Loads feature SYMBOL"
(require symbol))

(defun consult-web-sources-load-modules (&optional list)
  "Loads the LIST of symbols.
If list is nil, loads `consult-web-sources-modules-to-load'and if that is nil as well, loads `consult-web-sources--all-modules-list'."
  (mapcar #'consult-web-sources--load-module (or list consult-web-sources-modules-to-load consult-web-sources--all-modules-list)))

(consult-web-sources-load-modules)

;;; provide `consult-web-sources' module

(provide 'consult-web-sources)
;;; consult-web-sources.el ends here
