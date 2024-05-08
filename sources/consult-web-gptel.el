;;; consult-web-gptel.el --- Consulting gptel -*- lexical-binding: t -*-

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

(require 'gptel)
(require 'consult-web)

;;; Customization Variables
(defcustom consult-web-gptel-buffer-name  "*consult-web-gptel*"
  "Name for consult-web-gptel buffer."
  :type '(choice (:tag "A string for buffer name" string)
                 (:tag "A custom function taking prompt (and other args) as input and returning buffer name string" function)))

(cl-defun consult-web--gptel-fetch-results (input &rest args &key callback &allow-other-keys)
  "Makes cnaidate with INPUT as placeholder for `consult-web-gptel'.

This makes a placeholder string “ask gptel: %s” %s=INPUT with
metadata MODEL and BACKEND as text properties, so it can be send to
`gptel'."
  (unless (featurep 'gptel)
    (error "consult-web: gptel is not available. Make sure to install and load `gptel'."))
  (pcase-let* ((`(,query . ,opts) (consult-web--split-command input))
               (opts (car-safe opts))
               (backend (and (plist-member opts :backend) (format "%s" (plist-get opts :backend))))
               (backend (and backend (car (seq-filter (lambda (item) (when (string-match (format "%s" backend) item) item)) (mapcar #'car gptel--known-backends)))))
               (backend (or backend (gptel-backend-name gptel-backend)))
               (backend-struct  (cdr (assoc (format "%s" backend) gptel--known-backends)))
               (model (and (plist-member opts :model) (format "%s" (plist-get opts :model))))
               (model (or (and model backend-struct (member model (cl-struct-slot-value (type-of backend-struct) 'models backend-struct)) model)
                          (and backend-struct (car (cl-struct-slot-value (type-of backend-struct) 'models backend-struct)))))
               (stream (or (and (plist-member opts :stream) (plist-get opts :stream)) gptel-stream))
               (backend (propertize backend 'face 'consult-web-domain-face))
               (model (propertize model 'face 'consult-web-path-face))
               (title-str (format "ask gptel: %s" (string-trim-right query)))
               (title-str (propertize title-str 'face 'consult-web-ai-source-face))
               (placeholder (concat title-str
                                    (when backend (concat "\t" backend))
                                    (when model (concat ":" model))
                                    (and stream (propertize " ~stream~ " 'face 'consult-web-snippet-face))))
               (annotated-results (propertize placeholder
                                              :source "gptel"
                                              :title query
                                              :url nil
                                              :query query
                                              :model model
                                              :stream stream
                                              :backend backend)))
    (list annotated-results)
    ))

(defun consult-web--gptel-buffer-name (&optional query &rest args)
  "Returns a string for `consult-web-gptel' buffer name"
  (cond
   ((functionp consult-web-gptel-buffer-name)
    (funcall consult-web-gptel-buffer-name query args))
   ((stringp consult-web-gptel-buffer-name)
    consult-web-gptel-buffer-name)
   (t
    "*consult-web-gptel*")))

(cl-defun consult-web--gptel-response-preview (query &rest args &key backend model stream &allow-other-keys)
  "Returns a `gptel' buffer.

QUERY is sent to BACKEND using MODEL.
If STREAM is non-nil, the response is streamed."
  (save-excursion
    (with-current-buffer (gptel (consult-web--gptel-buffer-name query args) nil nil nil)
      (let* ((query-sent)
             (backend (and backend (car (seq-filter (lambda (item) (when (string-match (format "%s" backend) item) item)) (mapcar #'car gptel--known-backends)))))
             (backend (or backend (gptel-backend-name gptel-backend)))
             (backend (cdr (assoc (format "%s" backend) gptel--known-backends)))
             (model (or (and model (format "%s" model))
                        (and backend (car (cl-struct-slot-value (type-of backend) 'models backend)))
                        gptel-model))
             (stream (if stream t nil))
             )
        (setq-local gptel-backend backend)
        (setq-local gptel-model model)
        (setq-local gptel-stream stream)
        (erase-buffer)
        (insert (gptel-prompt-prefix-string))
        (insert (format "%s" query))
        (unless query-sent
          (erase-buffer)
          (insert (gptel-prompt-prefix-string) query)
          (setq query-sent t)
          (gptel-send)))
      (current-buffer))))

(defun consult-web--gptelbuffer-preview (cand)
  "Shows a preview buffer of CAND for `consult-web-gptel'.

The preview buffer is from `consult-web--gptel-response-preview'."
  (if (listp cand) (setq cand (or (car-safe cand) cand)))
  (let*  ((query (get-text-property 0 :query cand))
          (backend (get-text-property 0 :backend cand))
          (model (get-text-property 0 :model cand))
          (stream (get-text-property 0 :stream cand))
          (buff (consult-web--gptel-response-preview query :model model :backend backend :stream stream)))
    (if buff
        (funcall (consult--buffer-preview) 'preview
                 buff
                 ))))

(consult-web-define-source "gptel"
                           :narrow-char ?G
                           :type 'sync
                           :face 'consult-web-ai-source-face
                           :format #'consult-web-dynamic--gptel-format-candidate
                           :request #'consult-web--gptel-fetch-results
                           :on-preview #'consult-web--gptelbuffer-preview
                           :on-return #'identity
                           :on-callback #'consult-web--gptelbuffer-preview
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :enabled (lambda () (fboundp 'gptel))
                           :group #'consult-web--group-function
                           :sort t
                           :dynamic 'both
                           :annotate nil
                           )

;;; provide `consult-web-gptel' module

(provide 'consult-web-gptel)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-gptel)
;;; consult-web-gptel.el ends here
