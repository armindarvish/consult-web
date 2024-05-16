;;; consult-web-line-multi.el --- Search Lines in All Buffers  -*- lexical-binding: t -*-

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

(require 'consult)
(require 'consult-web)

(defun consult-web--line-multi-candidates (input &optional buffers)
  "Wrapper around consult--line-multi-candidates for consult-web."
  (let  ((buffers (or buffers (consult--buffer-query :directory (consult--normalize-directory default-directory) :sort 'alpha-current))))
    (consult--line-multi-candidates buffers input)))

(cl-defun consult-web--line-multi-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetches search results for INPUT from `consult-line-multi'."
(unless (functionp 'consult-web--line-multi-candidates)
  (error "consult-web: consult-web-line-multi not available. Make sure `consult' is loaded properly"))
(pcase-let* ((`(,query . ,opts) (consult-web--split-command input))
               (opts (car-safe opts))
               (items (consult-web--line-multi-candidates query))
               (annotated-results (mapcar (lambda (item)
                                            (let* ((source "Consult Line Multi")
                                                   (marker  (consult--get-location item))
                                                   (title (substring-no-properties item 0 -1))
                                                   (decorated (consult-web--line-multi-format-candidate :source source :query query :marker marker :title title)))
                                           (propertize decorated
                                                       :source source
                                                       :title title
                                                       :url nil
                                                       :marker marker
                                                       :query query
                                                       ))) items)))
    annotated-results))

(cl-defun consult-web--line-multi-format-candidate (&rest args &key source query marker title face &allow-other-keys)
  "Formats the cnaiddates of `consult-web-line-multi'.

SOURCE is the name to use (e.g. “Line MUlti”)

QUERY is the query input from the user

MARKER is the marker pointing to results of line multi search

TITLE is the title of the candidate (e.g. line text)

FACE is the face to apply to TITLE"
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (source (if (stringp source) (propertize source 'face 'consult-web-source-face)))
         (marker (car marker))
         (buff (marker-buffer marker))
         (pos (marker-position marker))
         (buff (and buff (propertize (format "%s" buff) 'face 'consult-web-domain-face)))
         (pos (and pos (propertize (format "%s" pos) 'face 'consult-web-path-face)))
         (match-str (if (stringp query) (consult--split-escaped (car (consult--command-split query))) nil))
         (face (or (consult-web--get-source-prop source :face) face 'consult-web-default-face))
         (title-str (propertize title 'face face))
         (title-str (consult-web--set-string-width title-str (* 6 frame-width-percent)))
         (str (concat title-str
                      (when buff (concat "\t" buff))
                      (when pos (concat "\s\s" pos ))
                      (when source (concat "\t" source))))
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
                           :narrow-char ?L
                           :type 'sync
                           :category 'consult-location
                           :face 'default
                           :request #'consult-web--line-multi-fetch-results
                           :preview-key consult-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :on-preview #'consult-web--line-multi-preview
                           :on-return #'identity
                           :on-callback #'consult-web--line-multi-preview
                           :enabled (lambda () (fboundp 'consult-web--line-multi-candidates))
                           :group #'consult-web--group-function
                           :sort t
                           :dynamic 'both
                           :annotate nil
                           )

;;; provide `consult-web-line-multi' module

(provide 'consult-web-line-multi)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-line-multi)
;;; consult-web-line-multi.el ends here
