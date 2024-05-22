;;; consult-web-buffer.el --- Consulting Buffers -*- lexical-binding: t -*-

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

(defun consult-web--consult-buffer-preview (cand)
  "Preview function for `consult-web--buffer'."
  (if cand
      (let* ((title (get-text-property 0 :title cand)))
        (when-let ((buff (get-buffer title)))
          (consult--buffer-action buff))
        )))

;; make consult-web sources from `consult-buffer-sources'
(cl-loop for source in consult-buffer-sources
         do (if (symbolp source) (consult-web--make-source-from-consult-source source
                                              :type 'sync
                                              :on-preview #'consult-web--consult-buffer-preview
                                              :on-return #'identity
                                              :on-callback #'consult--buffer-action
                                              :search-history 'consult-web--search-history
                                              :selection-history 'consult-web--selection-history
                                              :static 'both
                                              :preview-key 'consult-web-preview-key
                                              :group #'consult-web--group-function
                                              )))

;;; provide `consult-web-buffer' module

(provide 'consult-web-buffer)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-buffer)
;;; consult-web-buffer.el ends here
