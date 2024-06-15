;;; consult-web-find.el --- Consulting Find Command -*- lexical-binding: t -*-

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

(defcustom consult-web-find-show-hidden-files t
  "Whether to show hidden files in `consult-web-find'."
  :type 'boolean)

(defcustom consult-web-find-args  "find ."
  "Command line arguments for find.

Similar to `consult-find-args' bur for consult-web."
  :type '(choice string (repeat (choice string sexp))))

(defun consult-web--find-transform (candidates &optional query)
  "Formats consult-web-find candidates.
"
  (mapcar (lambda (candidate)
           (string-remove-prefix (file-truename default-directory) candidate))
          candidates))

(defun consult-web--find-filter (candidates &optional query)
  "Formats consult-web-find candidates.
"
  (seq-filter (lambda (candidate) (not (string-match "^find:.*$" candidate nil nil))) candidates))

(defun consult-web--find-preview (cand)
  "Grep preview function."
(funcall (consult--file-preview) 'preview cand))

(defun consult-web--find-callback (cand)
  "Find callback function."
  (consult--file-action cand)
  )

(cl-defun consult-web--find-builder (input &rest args &key callback &allow-other-keys)
  "makes builder command line args for “grep”.
"
  (pcase-let* ((`(,query . ,opts) (consult-web--split-command input args))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (hidden (if (plist-member opts :hidden) (plist-get opts :hidden) consult-web-find-show-hidden-files))
               (ignore (plist-get opts :ignore))
               (ignore (if ignore (format "%s" ignore)))
               (dir (plist-get opts :dir))
               (dir (if dir (file-truename (format "%s" dir))))
               (count (or (and (integerp count) count)
                          (and count (string-to-number (format "%s" count)))
                          consult-web-default-count))
               (default-directory (or dir default-directory))
               (`(_ ,paths _) (consult--directory-prompt "" dir))
               (paths (if dir
                        (mapcar (lambda (path) (file-truename (concat dir path))) paths)
                      paths))
               (consult-find-args (concat consult-web-find-args
                                          (if (not hidden) " -not -iwholename *./[a-z]*")
                                          (if ignore (concat " -not -iwholename *" ignore "*"))))
               )
   (funcall (consult--find-make-builder paths) query)
            ))

(consult-web-define-source "find"
                           :narrow-char ?f
                           :category 'file
                           :type 'async
                           :face 'consult-web-engine-source-face
                           :request #'consult-web--find-builder
                           :transform #'consult-web--find-transform
                           :filter #'consult-web--find-filter
                           :on-preview #'consult-web--find-preview
                           :on-return #'identity
                           :on-callback #'consult-web--find-callback
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :group #'consult-web--group-function
                           :sort t
                           :static 'both
                           :enabled (lambda () (if (executable-find "find") t nil))
                           :annotate nil
                           )

;;; provide `consult-web-find' module

(provide 'consult-web-find)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-find)
;;; consult-web-find.el ends here
