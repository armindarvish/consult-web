;;; consult-web-locate.el --- Consulting Locate Command -*- lexical-binding: t -*-

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

(defcustom consult-web-locate-limit consult-web-default-count
  "Max number results for `consult-web-locate'

This is passes to “-l” command line argument.
"
  :type 'integer)

(defcustom consult-web-locate-args "locate -i"
"Command line arguments for locate.

Similar to `consult-locate-args' bur for consult-web."
  :type '(choice string (repeat (choice string sexp))))

(defun consult-web--locate-preview (cand)
  "Grep preview function."
(funcall (consult--file-preview) 'preview cand))

(defun consult-web--locate-callback (cand)
  "Find callback function."
  (consult--file-action cand)
  )

(defun consult-web--find-transform (candidates &optional query)
  "Formats consult-web-find candidates.
"
  (mapcar (lambda (candidate)
           (string-remove-prefix (file-truename default-directory) candidate))
          candidates))

(defun consult-web--locate-filter (candidates &optional query)
  "Formats consult-web-find candidates.
"
  (seq-filter (lambda (candidate) (not (string-match "^locate:.*$" candidate nil nil))) candidates))

(cl-defun consult-web--locate-builder (input &rest args &key callback &allow-other-keys)
  "makes builder command line args for “locate”.
"
  (pcase-let* ((`(,query . ,opts) (consult-web--split-command input args))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (dir (plist-get opts :dir))
               (dir (if dir (file-truename (format "%s" dir))))
               (count (or (and (integerp count) count)
                          (and count (string-to-number (format "%s" count)))
                          consult-web-locate-limit))
               (default-directory (or dir default-directory))
               (consult-locate-args (concat consult-web-locate-args
                                            (if count (format " -l %s" count))))
               )

   (funcall #'consult--locate-builder query)
            ))

(consult-web-define-source "locate"
                           :narrow-char ?f
                           :category 'file
                           :type 'async
                           :face 'consult-web-engine-source-face
                           :request #'consult-web--locate-builder
                           ;; :transform nil
                           :filter #'consult-web--locate-filter
                           :on-preview #'consult-web--locate-preview
                           :on-return #'identity
                           :on-callback #'consult-web--locate-callback
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :group #'consult-web--group-function
                           :enabled (lambda () (if (executable-find "locate") t nil))
                           :sort t
                           :static 'both
                           :annotate nil
                           )

;;; provide `consult-web-locate' module

(provide 'consult-web-locate)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-locate)
;;; consult-web-locate.el ends here

;;; consult-web-mdfind.el --- Consulting mdfind Command -*- lexical-binding: t -*-

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

(defcustom consult-web-mdfind-interpret t
  "Whether to use the toggle -interpret in mdfind.
See mdfind documents (e.g. “man mdfind”) for more details.
"
  :type 'boolean)

(defcustom consult-web-mdfind-args "mdfind"
"Command line arguments for mdfind.

Similar to other command line args for consult but for mdfind.
See `consult-locate-args' for example."
  :type '(choice string (repeat (choice string sexp))))

(defun consult-web--mdfind-preview (cand)
  "Mdfind preview function."
(funcall (consult--file-preview) 'preview cand))

(defun consult-web--mdfind-callback (cand)
  "Mdfind callback function."
  (consult--file-action cand)
  )

(defun consult-web--find-transform (candidates &optional query)
  "Formats consult-web-mdfind candidates.
"
  (mapcar (lambda (candidate)
           (string-remove-prefix (file-truename default-directory) candidate))
          candidates))

(defun consult-web--mdfind-filter (candidates &optional query)
  "Formats `consult-web-mdfind' candidates.
"
  ;; (seq-filter (lambda (candidate) (not (string-match "^mdfind:.*$" candidate nil nil))) candidates)
)

(cl-defun consult-web--mdfind-builder (input &rest args &key callback &allow-other-keys)
  "makes builder command line args for “mdfind”.
"
  (pcase-let* ((`(,query . ,opts) (consult-web--split-command input args))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (dir (plist-get opts :dir))
               (dir (if dir (file-truename (format "%s" dir))))
               (count (or (and (integerp count) count)
                          (and count (string-to-number (format "%s" count)))
                          consult-web-default-count))
               (default-directory (or dir default-directory))
               (consult-locate-args (concat consult-web-mdfind-args
                                            (if consult-web-mdfind-interpret " -interpret")
(if dir (format " -onlyin %s" dir)))))
   (funcall #'consult--locate-builder query)
            ))

(consult-web-define-source "mdfind"
                           :narrow-char ?f
                           :category 'file
                           :type 'async
                           :face 'consult-web-engine-source-face
                           :request #'consult-web--mdfind-builder
                           ;; :transform nil
                           ;; :filter #'consult-web--mdfind-filter
                           :on-preview #'consult-web--mdfind-preview
                           :on-return #'identity
                           :on-callback #'consult-web--mdfind-callback
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :group #'consult-web--group-function
                           :enabled (lambda () (if (executable-find "mdfind") t nil))
                           :sort t
                           :static 'both
                           :annotate nil
                           )

;;; provide `consult-web-mdfind' module

(provide 'consult-web-mdfind)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-mdfind)
;;; consult-web-mdfind.el ends here
