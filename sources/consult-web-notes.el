;;; consult-web-notes.el --- Consulting Note Files -*- lexical-binding: t -*-

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
(require 'consult-web-grep nil t)

(defcustom consult-web-notes-files (apply #'append
                                     (when (bound-and-true-p consult-notes-file-dir-sources)
                                     ;; dir sources
                                     (apply #'append (mapcar #'cddr consult-notes-file-dir-sources)))
                                     ;; org roam
                                     (when (bound-and-true-p org-roam-directory)
                                       (list (expand-file-name org-roam-directory)))
                                     ;; denote
                                     (when (bound-and-true-p denote-directory)
                                       (list (expand-file-name denote-directory)))
                                     ;; org agenda files
                                     (when (bound-and-true-p consult-notes-org-headings-mode)
                                       (list (mapcar #'expand-file-name consult-notes-org-headings-files))))
"List of all note files for consult-web-notes."
:type '(repeat :tag "list of files" string))

(defcustom consult-web-notes-use-rg t
"whether to use ripgrep when searching ntoes?"
:type 'boolean)

(cl-defun consult-web--notes-builder (input &rest args &key callback &allow-other-keys)
  "makes builder command line args for `consult-web-notes'.
"
  (pcase-let* ((`(,query . ,opts) (consult-web--split-command input args))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (dir (plist-get opts :dir))
               (dir (if dir (file-truename (format "%s" dir))))
               (dir (or dir consult-web-notes-files))
               (count (or (and (integerp count) count)
                          (and count (string-to-number (format "%s" count)))
                          consult-web-default-count))
               )
   (funcall (consult-web--grep-make-builder (if (and consult-web-notes-use-rg (executable-find "rg")) #'consult--ripgrep-make-builder #'consult--grep-make-builder) dir) query)
            ))

(defun consult-web--notes-transform (candidates &optional query)
  "Formats `consult-web-notes' candidates.
"
(let* ((frame-width-percent (floor (* (frame-width) 0.1)))
      (file "")
      (file-len 0)
      (file-str "")
      result)
          (save-match-data
            (dolist (str candidates)
              (when (and (string-match consult--grep-match-regexp str)
                         ;; Filter out empty context lines
                         (or (/= (aref str (match-beginning 3)) ?-)
                             (/= (match-end 0) (length str))))
                ;; We share the file name across candidates to reduce
                ;; the amount of allocated memory.
                (unless (and (= file-len (- (match-end 1) (match-beginning 1)))
                             (eq t (compare-strings
                                    file 0 file-len
                                    str (match-beginning 1) (match-end 1) nil)))
                  (setq file (match-string 1 str))
                  ;; (setq file (file-truename file))
                  ;; (if (> file-len (* frame-width-percent 2))
                  ;;   (setq file-str (consult-web--set-string-width file (* frame-width-percent 2) (* frame-width-percent 1)))
                  ;;   (setq file-str file))
                  ;; (when (> file-len (* frame-width-percent 2))
                  ;;   (setq file (consult-web--set-string-width file (* frame-width-percent 2) (* frame-width-percent 1))
                  ;;                          ))

                  ;; (propertize file-str 'face 'consult-file 'consult--prefix-group file)
                  (setq file-len (length file))
)
                (let* ((line (propertize (match-string 2 str) 'face 'consult-line-number))
                       (ctx (= (aref str (match-beginning 3)) ?-))
                       (sep (if ctx "-" ":"))
                       (content (substring str (match-end 0)))
                       (line-len (length line)))
                  (when (length> content consult-grep-max-columns)
                    ;; (setq content (substring content 0 consult-grep-max-columns))
                    (setq content  (consult-web--set-string-width content consult-grep-max-columns))
                    )
                  (setq str (concat file sep line sep content))

                  ;; (setq str (concat content sep line sep (propertize (file-name-nondirectory file) 'face 'consult-file 'consult--prefix-group file)))
                  ;; Store file name in order to avoid allocations in `consult--prefix-group'
                  (add-text-properties 0 file-len `(face consult-file consult--prefix-group ,file) str)
                  (put-text-property (1+ file-len) (+ 1 file-len line-len) 'face 'consult-line-number str)
                  ;; (when ctx
                  ;;   (add-face-text-property (+ 2 file-len line-len) (length str) 'consult-grep-context 'append str))
                  (push (propertize str :source "Notes Search" :title query :file file) result)))))
          result))

(consult-web-define-source "Notes Search"
                           :narrow-char ?n
                           :type 'async
                           :face 'consult-web-engine-source-face
                           :request #'consult-web--notes-builder
                           :transform #'consult-web--notes-transform
                           :on-preview #'consult-web--grep-preview
                           :on-return #'identity
                           :on-callback #'consult-web--grep-callback
                           :preview-key 'any
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :group #'consult-web--group-function
                           ;;:group #'consult--prefix-group
                           :sort t
                           :static 'both
                           :annotate nil
                           )

;;; provide `consult-web-notes' module

(provide 'consult-web-notes)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-notes)
;;; consult-web-notes.el ends here
