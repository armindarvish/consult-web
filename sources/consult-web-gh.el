;;; consult-web-gh.el --- Consulting Github Client -*- lexical-binding: t -*-

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
(require 'consult-gh nil t)

(cl-defun consult-web--gh-search-repos-builder (input &rest args &key callback &allow-other-keys)
  "makes builder command line args for “GitHub CLI”.
"
  (pcase-let* ((`(,query . ,opts) (consult-web--split-command input args))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (count (or (and (integerp count) count)
                          (and count (string-to-number (format "%s" count)))
                          consult-web-default-count))
               (cmd (consult--build-args '("gh" "search" "repos")))
               (cmd-opts (list "--limit" (format "%s" count)))
               (`(,re . ,hl) (funcall consult--regexp-compiler query 'basic t)))
      (when re
        (cons (append cmd
                      (list (string-join re " "))
                      cmd-opts)
              hl))))

(defun consult-web--gh-preview (cand)
  "Preview for github repo candidates"
  (when-let ((info (text-properties-at 0 (cdr (get-text-property 0 'multi-category cand))))
             (repo (plist-get info :repo))
             (query (plist-get info :query))
             (match-str (consult--build-args query))
             (buffer (get-buffer-create consult-gh-preview-buffer-name)))
    (add-to-list 'consult-gh--preview-buffers-list buffer)
    (consult-gh--repo-view (format "%s" repo) buffer)
    (with-current-buffer buffer
      (if consult-gh-highlight-matches
          (cond
           ((listp match-str)
            (mapcar (lambda (item)
                      (highlight-regexp item 'consult-gh-preview-match-face)) match-str))
           ((stringp match-str)
            (highlight-regexp match-str 'consult-gh-preview-match-face))
           )))
    (funcall (consult--buffer-preview) 'preview
             buffer
             )
    ))

(defun consult-web--gh-callback (cand)
  "Callback for github repo candidates."
  (funcall consult-gh-repo-action (cons cand (text-properties-at 0 (cdr (get-text-property 0 'multi-category cand))))))

(consult-web-define-source "GitHub"
                           :narrow-char ?G
                           :type 'async
                           :category 'consult-gh-repos
                           :face 'consult-web-engine-source-face
                           :request #'consult-web--gh-search-repos-builder
                           :on-preview #'consult-web--gh-preview
                           :on-return #'identity
                           :on-callback #'consult-web--gh-callback
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :group #'consult-web--group-function
                           :sort t
                           :static 'both
                           :transform (lambda (items &optional query) (mapcar (lambda (string)
                                                                (consult-gh--repo-format string (or query "") t)) items))
                           :annotate nil
                           )

;;; provide `consult-web-gh' module

(provide 'consult-web-gh)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-gh)
;;; consult-web-gh.el ends here
