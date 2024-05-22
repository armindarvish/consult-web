;;; consult-web-consult-notes.el --- Consulting Consult Notes -*- lexical-binding: t -*-

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
(require 'consult-notes nil t)

(defun consult-web--org-roam-note-preview (cand)
  "Preview function for org-roam files."
  (if cand
      (let* ((title (get-text-property 0 :title cand))
             (node (org-roam-node-from-title-or-alias title)))
        (if (org-roam-node-p node)
            (consult--file-action (org-roam-node-file node))
          ))))

(defun consult-web--org-headings-preview (cand)
  "Preview function for org headings."
  (if cand
      (let* ((title (get-text-property 0 :title cand))
             (marker (get-text-property 0 'consult--candidate title)))
        (if marker
            (consult--jump marker)))))

(defun consult-web--org-roam-note-callback (cand &rest args)
  "Callback function for org-roam files."
  (let* ((title (get-text-property 0 :title cand))
         (node (org-roam-node-from-title-or-alias title)))
    (org-roam-node-open node)))

(defun consult-web--org-headings-callback (cand &rest args)
  "Callback function for org headings."
  (if cand
      (let* ((title (get-text-property 0 :title cand))
             (marker (get-text-property 0 'consult--candidate title)))
        (if marker
           (let* ((buff (marker-buffer marker))
                 (pos (marker-position marker)))
             (if buff (with-current-buffer buff
               (if pos (goto-char pos))
               (funcall consult--buffer-display buff)
               (recenter nil t)
               )))
             ))))

;; make consult-web sources from consult-notes sources
(when consult-notes-org-headings-mode
  (consult-web--make-source-from-consult-source 'consult-notes-org-headings--source
                                                :category 'file
                                                :type 'sync
                                                :face 'consult-web-notes-source-face
                                                :search-history 'consult-web--search-history
                                                :selection-history 'consult-web--selection-history
                                                :on-preview #'consult-web--org-headings-preview
                                                :on-return #'identity
                                                :on-callback #'consult-web--org-headings-callback
                                                :search-history 'consult-web--search-history
                                                :selection-history 'consult-web--selection-history
                                                :preview-key 'consult-preview-key
                                                :group #'consult-web--group-function
                                                :static 'both
                                                ))

(when consult-notes-org-roam-mode
  (cl-loop for source in '(consult-notes-org-roam--refs consult-notes-org-roam--nodes)
           do (consult-web--make-source-from-consult-source source
                                                            :category 'file
                                                            :type 'sync
                                                            :face 'consult-web-notes-source-face
                                                            :search-history 'consult-web--search-history
                                                            :selection-history 'consult-web--selection-history
                                                            :on-preview #'consult-web--org-roam-note-preview
                                                            :on-return #'identity
                                                            :on-callback #'consult-web--org-roam-note-callback

                                                            :preview-key 'consult-preview-key
                                                            :static 'both
                                                            :group #'consult-web--group-function
                                                            :annotate nil
                                                            )))

;;; provide `consult-web-consult-notes' module

(provide 'consult-web-consult-notes)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-consult-notes)
;;; consult-web-consult-notes.el ends here
