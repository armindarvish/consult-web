;;; consult-web-chatgpt.el --- Consulting chatGPT -*- lexical-binding: t -*-

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

(defun consult-web-dynamic--chatgpt-format-candidate (table &optional face &rest args)
  "Returns a formatted string for candidates of `consult-web-chatgpt'.

TABLE is a hashtable from `consult-web--chatgpt-fetch-results'."
  (let* ((pl (consult-web-hashtable-to-plist table))
         (title (format "%s" (gethash :title table)))
         (source (gethash :source table))
         (source (if (stringp source) (propertize source 'face 'consult-web-source-face)))
         (query (gethash :query table))
         (model (gethash :model table))
         (match-str (if (stringp query) (consult--split-escaped (car (consult--command-split query))) nil))
         (title-str (consult-web--set-string-width title (floor (* (frame-width) 0.4))))
         (title-str (propertize title-str 'face (or face 'consult-web-ai-source-face)))
         (extra-args (consult-web-hashtable-to-plist table '(:title :url :search-url :query :source :model)))
         (str (concat title-str (if model (propertize (format "\tmodel: %s" model) 'face 'consult-web-path-face)) (if source (concat "\t" source)) (if extra-args (format "\t%s" extra-args))))
         (str (apply #'propertize str pl))
         )
    (if consult-web-highlight-matches
        (cond
         ((listp match-str)
          (mapcar (lambda (match) (setq str (consult-web--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-web--highlight-match match-str str t)))))
    str))

(defvar consult-web-chatgpt-api-url "https://api.openai.com/v1/chat/completions")

(defcustom consult-web-openai-api-key nil
"Key for OpeAI API

See URL `https://openai.com/product' and URL `https://platform.openai.com/docs/introduction' for details"
:group 'consult-web
:type '(choice (const :tag "API Key" string)
               (function :tag "Custom Function")))


(cl-defun consult-web--chatgpt-fetch-results (input &rest args &key model &allow-other-keys)
  "Fetches chat response for INPUT from chatGPT."
  (let* ((model (or model gptel-model))
         (headers `(("Content-Type" . "application/json")
                    ("Authorization" . ,(concat "Bearer " (consult-web-expand-variable-function consult-web-openai-api-key))))))
    (funcall consult-web-retrieve-backend
     consult-web-chatgpt-api-url
     :type "POST"
     :headers headers
     :data  (json-encode `((model . ,model)
                    (messages . [((role . "user")
                                  (content . ,input))])))
     :parser
     (lambda ()
       (goto-char (point-min))
       (let* ((table (make-hash-table :test 'equal))
              (response (json-parse-buffer))
              (title (gethash "content" (gethash "message" (aref (gethash "choices" response) 0)))))
         (puthash :url nil
                  table)
         (puthash :title title
                  table)
         (puthash :source "chatGPT"
                  table)
         (puthash :model model
                  table)
         (puthash :query input
                  table)
         (list table)))
     )))

(defun consult-web--chatgpt-response-preview (response &optional query)
  "Returns a buffer with formatted RESPONSE from chatGPT"
  (save-excursion
    (let ((buff (get-buffer-create "*consult-web-chatgpt-response*")))
      (with-current-buffer buff
        (erase-buffer)
        (if query (insert (format "# User:\n\n %s\n\n" query)))
        (if response (insert (format "# chatGPT:\n\n %s\n\n" response)))
        (if (featurep 'mardown-mode)
            (require 'markdown-mode)
          (markdown-mode)
          )
        (point-marker))
      )))


(defun consult-web--chatgpt-preview (cand)
  "Shows a preview buffer with chatGPT response from CAND"
  (when-let ((buff (get-buffer "*consult-web-chatgpt-response*")))
    (kill-buffer buff))

  (when-let*  ((query (cond ((listp cand)
                             (get-text-property 0 :query (car cand)))
                            (t
                             (get-text-property 0 :query cand))))
               (response (cond ((listp cand)
                                (or (get-text-property 0 :title (car cand)) (car cand)))
                               (t
                                (or (get-text-property 0 :title cand) cand))))
               (marker (consult-web--chatgpt-response-preview response query)))
    (consult--jump marker)
))


(consult-web-define-source "chatGPT"
                           :narrow-char ?G
                           :face 'consult-web-ai-source-face
                           :request #'consult-web--chatgpt-fetch-results
                           :format #'consult-web-dynamic--chatgpt-format-candidate
                           :on-preview #'consult-web--chatgpt-preview
                           :on-return #'identity
                           :on-callback #'consult-web--chatgpt-preview
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :dynamic 'both
                           )

;;; provide `consult-web-chatgpt' module

(provide 'consult-web-chatgpt)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-chatgpt)
;;; consult-web-chatgpt.el ends here
