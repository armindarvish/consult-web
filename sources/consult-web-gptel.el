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

(defun consult-web-dynamic--gptel-format-candidate (table &optional face &rest args)
  "Returns a formatted string for candidates of `consult-web-gptel'.

TABLE is a hashtable from `consult-web--gptel-fetch-results'."
  (let* ((pl (consult-web-hashtable-to-plist table))
         (title (format "%s" (gethash :title table)))
         (source (gethash :source table))
         (source (if (stringp source) (propertize source 'face 'consult-web-source-face)))
         (query (gethash :query table))
         (backend (gethash :backend table))
         (model (gethash :model table))
         (stream (gethash :stream table))
         (match-str (if (stringp query) (consult--split-escaped (car (consult--command-split query))) nil))
         (title-str (consult-web--set-string-width title (floor (* (frame-width) 0.4))))
         (title-str (propertize title-str 'face (or face 'consult-web-ai-source-face)))
         (extra-args (consult-web-hashtable-to-plist table '(:title :url :search-url :query :source :backend :model :stream)))
         (str (concat title-str
                      (if backend (concat
                                   (propertize (format "\t%s" backend) 'face 'consult-web-domain-face)
                                   (if model (propertize (format ":%s" model) 'face 'consult-web-path-face))))
                      (if stream (propertize " ~stream~ " 'face 'consult-web-source-face))
                      (if source (concat "\t" source)) (if extra-args (format "\t%s" extra-args))))
         (str (apply #'propertize str pl))
         )
    (if consult-web-highlight-matches
        (cond
         ((listp match-str)
          (mapcar (lambda (match) (setq str (consult-web--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-web--highlight-match match-str str t)))))
    str))

(cl-defun consult-web--gptel-fetch-results (input &rest args &key backend model stream &allow-other-keys)
 "Makes cnaidate with INPUT as placeholder for `consult-web-gptel'.

This makes a placeholder string “ask gptel: %s” %s=INPUT with
metadata MODEL and BACKEND as text properties, so it can be send to
`gptel'."
 (unless (featurep 'gptel)
   (error "consult-web: gptel is not available. Make sure to install and load `gptel'."))
  (let* ((table (make-hash-table :test 'equal))
         (backend (if backend (format "%s" backend) nil))
         (backend (and backend (car (seq-filter (lambda (item) (when (string-match (format "%s" backend) item) item)) (mapcar #'car gptel--known-backends)))))
        (backend (or backend (and gptel-backend (cl-struct-slot-value (type-of gptel-backend) 'name gptel-backend))))
        (backend-struct  (cdr (assoc (format "%s" backend) gptel--known-backends)))
        (model (if model (format "%s" model)))
        (model (or (and model backend-struct (member model (cl-struct-slot-value (type-of backend-struct) 'models backend-struct)) model)
               (and model gptel-backend (member model (cl-struct-slot-value (type-of gptel-backend) 'models gptel-backend)) model)
               (and backend-struct (car (cl-struct-slot-value (type-of backend-struct) 'models backend-struct)))
               (and gptel-backend (car (cl-struct-slot-value (type-of gptel-backend) 'models gptel-backend)))))
        (stream (if (member :stream args) stream gptel-stream)))
    (puthash :url nil table)
    (puthash :title (concat "ask gptel: " (format "%s" input))
             table)
    (puthash :source "gptel"
             table)
    (puthash :query input
             table)
    (puthash :model model
             table)
    (puthash :stream stream
             table)
    (puthash :backend backend
             table)
    (list table)))

(defun consult-web--gptel-buffer-name (&optional query &rest args)
  "Returns a string for `consult-web-gptel' buffer name"
    (cond
     ((functionp consult-web-gptel-buffer-name)
      (funcall consult-web-gptel-buffer-name query args))
     ((stringp consult-web-gptel-buffer-name)
      consult-web-gptel-buffer-name)
     (t
      "*consult-web-gptel*")))

(cl-defun consult-web--gptel-response-preview (query &rest args &key backend model stream &allow-other-key)
"Returns a `gptel' buffer.

QUERY is sent to BACKEND using MODEL.
If STREAM is non-nil, the response is streamed."
 (save-excursion
    (with-current-buffer (gptel (consult-web--gptel-buffer-name query args) nil nil nil)
      (let* ((backend (if backend (format "%s" backend) nil))
             (backend (and backend (car (seq-filter (lambda (item) (when (string-match (format "%s" backend) item) item)) (mapcar #'car gptel--known-backends)))))
             (backend (or (and backend (cdr (assoc backend gptel--known-backends)))
                          gptel-backend))
             (model (or (and model (format "%s" model))
                        (and backend (car (cl-struct-slot-value (type-of backend) 'models backend)))
                        (and gptel-backend (car (cl-struct-slot-value (type-of gptel-backend) 'models gptel-backend)))
                        gptel-model))
             (stream (if stream t nil))
             )

        (setq-local gptel-backend backend)
        (setq-local gptel-model model)
        (setq-local gptel-stream stream)
        (erase-buffer)
        (insert (gptel-prompt-prefix-string))
        (insert (format "%s" query))
        (gptel-send)
        (current-buffer)))))

(defun consult-web--gptelbuffer-preview (cand)
 "Shows a preview buffer of CAND for `consult-web-gptel'.

The preview buffer is from `consult-web--gptel-response-preview'."
  (let*  ((query (cond ((listp cand)
                             (get-text-property 0 :query (car cand)))
                            (t
                             (get-text-property 0 :query cand))))
               (backend (cond ((listp cand)
                             (get-text-property 0 :backend (car cand)))
                            (t
                             (get-text-property 0 :backend cand))))
               (model (cond ((listp cand)
                             (get-text-property 0 :model (car cand)))
                            (t
                             (get-text-property 0 :model cand))))
               (stream (cond ((listp cand)
                             (get-text-property 0 :stream (car cand)))
                            (t
                             (get-text-property 0 :stream cand))))
               (buff (consult-web--gptel-response-preview query :model model :backend backend :stream stream)))
  (if buff
    (funcall (consult--buffer-preview) 'preview
             buff
             ))))

(consult-web-define-source "gptel"
                           :narrow-char ?G
                           :face 'consult-web-ai-source-face
                           :format #'consult-web-dynamic--gptel-format-candidate
                           :request #'consult-web--gptel-fetch-results
                           :on-preview #'consult-web--gptelbuffer-preview
                           :on-return #'identity
                           :on-callback #'consult-web--gptelbuffer-preview
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :dynamic 'both
                           )

;;; provide `consult-web-gptel' module

(provide 'consult-web-gptel)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-gptel)
;;; consult-web-gptel.el ends here
