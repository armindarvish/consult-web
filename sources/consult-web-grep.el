;;; consult-web-grep.el --- Consulting Grep Command -*- lexical-binding: t -*-

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

(defun consult-web--grep-make-builder (make-builder &optional dir)
  "General builders for grep and similar process.
"
(pcase-let* ((`(_ ,paths ,dir) (consult--directory-prompt "" dir))
             (paths (if dir
                        (mapcar (lambda (path) (file-truename (concat dir path))) paths)
                      paths))
             )
(funcall make-builder paths)
))

(defun consult-web--grep-transform (candidates &optional query)
  "Formats consult-web-grep candidates.
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
                  (setq file (file-relative-name (match-string 1 str) (buffer-file-name)))
                  (when (and file (stringp file) (> file-len (* frame-width-percent 2)))
                    (setq file (consult-web--set-string-width file (* frame-width-percent 2) (* frame-width-percent 1))))
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
                  ;; Store file name in order to avoid allocations in `consult--prefix-group'
                  (add-text-properties 0 file-len `(face consult-file consult--prefix-group ,file) str)
                  ;; (put-text-property (1+ file-len) (+ 1 file-len line-len) 'face 'consult-line-number str)
                  (when ctx
                    (add-face-text-property (+ 2 file-len line-len) (length str) 'consult-grep-context 'append str))
                  (push (propertize str :source "grep" :title query) result)))))
          result))

(defun consult-web--grep-preview (cand)
  "Grep preview function."
(funcall  (consult--jump-state) 'preview (consult--grep-position (cdr (get-text-property 0 'multi-category cand))))
  )

(defun consult-web--grep-callback (cand)
  "Grep callback function."
(funcall  (consult--jump-state) 'return (consult--grep-position (cdr (get-text-property 0 'multi-category cand))))
  )

(cl-defun consult-web--grep-builder (input &rest args &key callback &allow-other-keys)
  "makes builder command line args for “grep”.
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
               )
   (funcall (consult-web--grep-make-builder #'consult--grep-make-builder dir) query)
            ))

(consult-web-define-source "grep"
                           :narrow-char ?r
                           :type 'async
                           :face 'consult-web-engine-source-face
                           :request #'consult-web--grep-builder
                           :transform #'consult-web--grep-transform
                           :on-preview #'consult-web--grep-preview
                           :on-return #'identity
                           :on-callback #'consult-web--grep-callback
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :group #'consult-web--group-function
                           ;; :group #'consult--prefix-group
                           :sort t
                           :static 'both
                           :transform #'consult-web--ripgrep-transform
                           :annotate nil
                           )

;;; provide `consult-web-grep' module

(provide 'consult-web-grep)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-grep)
;;; consult-web-grep.el ends here
