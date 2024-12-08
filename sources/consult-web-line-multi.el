;;; consult-web-line-multi.el --- Search Lines in All Buffers  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: "0.1"
;; Package-Requires: ((emacs "28.1") (consult "1.1"))
;; Homepage: https://github.com/armindarvish/consult-web
;; Keywords: convenience

;;; Commentary:

;;; Code:

(require 'consult)
(require 'consult-web)

(defun consult-web--line-multi-candidates (input &optional buffers)
  "Wrapper around consult--line-multi-candidates for consult-web."
  (let  ((buffers (or buffers (consult--buffer-query :directory (consult--normalize-directory default-directory) :sort 'alpha-current))))
    (consult--line-multi-candidates buffers input)))

(cl-defun consult-web--line-multi-fetch-results (input &rest args)
"Fetches search results for INPUT from `consult-line-multi'."
(unless (functionp 'consult-web--line-multi-candidates)
  (error "consult-web: consult-web-line-multi not available. Make sure `consult' is loaded properly"))
(let ((items (consult-web--line-multi-candidates input)))
  (cl-loop for a in items
           collect
           (let* ((table (make-hash-table :test 'equal))
                  (marker  (consult--get-location a))
                  (title (substring-no-properties a 0 -1)))
           (puthash :title title
                    table)
           (puthash :url nil
                    table)
           (puthash :query input
                    table)
           (puthash :source "Consult Line Multi"
                    table)
           (puthash :marker marker
                    table)
           table)))
)

(defun consult-web-dynamic--line-multi-format-candidate (table &optional face &rest args)
  "Returns a formatted string for candidates of `consult-web-dynamic-line-multi'.

TABLE is a hashtable from `consult-web--line-multi-fetch-results'."
  (let* ((pl (consult-web-hashtable-to-plist table))
         (title (format "%s" (gethash :title table)))
         (source (gethash :source table))
         (source (if (stringp source) (propertize source 'face 'consult-web-source-face)))
         (query (gethash :query table))
         (marker (car (gethash :marker table)))
         (buff (marker-buffer marker))
         (pos (marker-position marker))
         (match-str (if (stringp query) (consult--split-escaped (car (consult--command-split query))) nil))
         (title-str (consult-web--set-string-width title (floor (* (frame-width) 0.66))))
         (title-str (propertize title-str 'face (or face 'default)))
         (extra-args (consult-web-hashtable-to-plist table '(:title :url :search-url :query :source :marker)))
         (str (concat title-str (if buff (concat "\t" (propertize (format "%s" buff) 'face 'consult-web-domain-face))) (if pos (concat "\t" (propertize (format "%s" pos) 'face 'consult-web-path-face))) (if source (concat "\t" source)) (if extra-args (format "\t%s" extra-args))))
         (str (apply #'propertize str pl))
         )
    (if consult-web-highlight-matches
        (cond
         ((listp match-str)
          (mapcar (lambda (match) (setq str (consult-web--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-web--highlight-match match-str str t)))))
    str))

(defun consult-web--line-multi-preview (cand)
"Preview function for consult-web-line-multi."
  (let* ((marker (car (get-text-property 0 :marker cand)))
         (query (get-text-property 0 :query cand)))
    (consult--jump marker)
       ))

(consult-web-define-source "Consult Line Multi"
                           :category 'consult-location
                           :narrow-char ?L
                           :face 'consult-web-files-source-face
                           :request #'consult-web--line-multi-fetch-results
                           :format #'consult-web-dynamic--line-multi-format-candidate
                           :preview-key consult-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :on-preview #'consult-web--line-multi-preview
                           :on-return #'identity
                           :on-callback
                           #'consult-web--line-multi-preview
                           :dynamic 'both
                           )

;;; provide `consult-web-line-multi' module

(provide 'consult-web-line-multi)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-line-multi)
;;; consult-web-line-multi.el ends here
