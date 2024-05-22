;;; consult-web-notmuch.el --- Consulting Notmuch Command -*- lexical-binding: t -*-

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
(require 'consult-notmuch nil t)

(cl-defun consult-web--notmuch-command-builder (input &rest args &key callback &allow-other-keys)
  "makes builder command line args for “notmuch”.
"
  (setq consult-notmuch--partial-parse nil)
  (pcase-let* ((`(,query . ,opts) (consult-web--split-command input args))
               (opts (car-safe opts)))
  (consult-notmuch--command query)
  ))

(defun consult-web-notmuch--transformer (str &optional query)
  "Transforms STR to notmuch display style."
  (let ((string (if consult-notmuch-show-single-message
      (consult-notmuch--show-transformer str)
    (consult-notmuch--search-transformer str))))
    (if (stringp string)
        (propertize string :source "notmuch" :query query))
    ))

(defun consult-web--notmuch--preview (cand)
  "Preview function for notmuch candidates."
  (when-let ((id (consult-notmuch--candidate-id (cdr (get-text-property 0 'multi-category cand)))))
    (when (get-buffer consult-notmuch--buffer-name)
      (kill-buffer consult-notmuch--buffer-name))
    (consult-notmuch--show-id id consult-notmuch--buffer-name)))

(defun consult-web--notmuch-callback (cand)
  "Callback function for notmuch candidates.
"
  (consult-notmuch--show (cdr (get-text-property 0 'multi-category cand))))

(consult-web-define-source "notmuch"
                           :narrow-char ?m
                           :type 'async
                           :category 'notmuch-result
                           :face 'consult-web-engine-source-face
                           :request #'consult-web--notmuch-command-builder
                           :on-preview #'consult-web--notmuch--preview
                           :on-return #'identity
                           :on-callback #'consult-web--notmuch-callback
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :group #'consult-web--group-function
                           :sort t
                           :static 'both
                           :transform (lambda (items &optional query) (remove nil (mapcar (lambda (string)
                                                                (consult-web-notmuch--transformer string query)) items)))
                           :annotate nil
                           )

;;; provide `consult-web-notmuch' module

(provide 'consult-web-notmuch)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-notmuch)
;;; consult-web-notmuch.el ends here
