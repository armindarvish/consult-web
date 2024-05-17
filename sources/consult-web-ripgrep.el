;;; consult-web-ripgrep.el --- Consulting Ripgrep Command -*- lexical-binding: t -*-

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
(require 'consult-web-grep)

(cl-defun consult-web--ripgrep-builder (input &rest args &key callback &allow-other-keys)
  "makes builder command line args for “ripgrep”.
"
  (pcase-let* ((`(,query . ,opts) (consult-web--split-command input))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (dir (plist-get opts :dir))
               (dir (if dir (file-truename (format "%s" dir))))
               (count (or (and (integerp count) count)
                          (and count (string-to-number (format "%s" count)))
                          consult-web-default-count))
               (default-directory (or dir default-directory))
               )
   (funcall (consult-web--grep-make-builder #'consult--ripgrep-make-builder dir) query)
            ))

(defun consult-web--ripgrep-transform (candidates &optional query)
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
                  (push (propertize str :source "ripgrep" :title query) result)))))
          result))

(file-relative-name "/Users/armin/projects/consult-web/README.org" (buffer-file-name))

(consult-web-define-source "ripgrep"
                           :narrow-char ?r
                           :type 'async
                           :face 'consult-web-engine-source-face
                           :request #'consult-web--ripgrep-builder
                           :transform #'consult-web--ripgrep-transform
                           :on-preview #'consult-web--grep-preview
                           :on-return #'identity
                           :on-callback #'consult-web--grep-callback
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :group #'consult-web--group-function
                           ;; :group #'consult--prefix-group
                           :sort t
                           :dynamic 'both
                           :transform #'consult-web--ripgrep-transform
                           :annotate nil
                           )

;;; provide `consult-web-ripgrep' module

(provide 'consult-web-ripgrep)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-ripgrep)
;;; consult-web-ripgrep.el ends here
