;;; consult-web.el --- Consulting Web Search Engines -*- lexical-binding: t -*-

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

;;; Requirements
(eval-when-compile
(require 'consult)
(require 'url)
(when (featurep 'json)
  (require 'json))
(when (featurep 'request)
  (require 'request))
)

;;; Group
(defgroup consult-web nil
  "Consulting search engines and AI assistants"
  :group 'convenience
  :group 'minibuffer
  :group 'consult
  :group 'web
  :group 'search
  :prefix "consult-web-")

;;; Customization Variables
(defcustom consult-web-sources-modules-to-load  (list)
  "List of source modules/features to load.

This variable is a list of symbols;
each symbol being a source featue (e.g. consult-web-brave)"
  :type '(repeat :tag "list of source modules/features to load" symbol))

(defcustom consult-web-default-browse-function #'browse-url
  "consult-web default function when selecting a link"
  :type '(choice (function :tag "(Default) Browse URL" #'browse-url)
                 (function :tag "Custom Function")))

(defcustom consult-web-alternate-browse-function #'eww-browse-url
  "consult-web default function when selecting a link"
  :type '(choice (function :tag "(Default) EWW" #'eww-browse-url)
                 (function :tag "Custom Function")))

(defcustom consult-web-default-preview-function #'eww-browse-url
  "consult-web default function when previewing a link"
  :type '(choice (function :tag "(Default) EWW" #'eww-browse-url)
                 (function :tag "Custom Function")))


(defcustom consult-web-show-preview nil
  "Should`consult-web' show previews?
This turns previews on/off globally for all consult-web sources."
  :type 'boolean)

(defcustom consult-web-preview-key consult-preview-key
  "Preview key for consult-web.
This is similar to `consult-preview-key' but explicitly For consult-web."
  :type '(choice (const :tag "Any Key" Any)
                 (List :tag "Debounced"
                       (const :Debounce)
                       (Float :tag "Seconds" 0.1)
                       (const Any))
                 (const :tag "No Preview" nil)
                 (Key :tag "Key")
                 (repeat :tag "List Of Keys" Key)))

(defcustom consult-web-default-count 5
  "Number Of search results to retrieve."
  :type 'integer)

(defcustom consult-web-default-page 0
  "Offset of search results to retrieve.
If this is set to N, the first N “pages”
(or other first N entities, items for example,
depending On the source search engine capabilities)
of the search results are omitted and the rest are shown."
  :type 'integer)

(defcustom consult-web-default-timeout 30
  "Default timeout in seconds for
`consult-web--url-retrieve-synchronously."
  :type 'integer)

(defcustom consult-web-log nil
  "Default timeout in seconds for
`consult-web--url-retrieve-synchronously."
  :type 'boolean)

(defcustom consult-web-log-buffer-name " *consult-web-log*"
"String for consult-web-log buffer name"
:type 'string)

(defcustom consult-web-log-level nil
  "How to make logs for consult-web requests?
This can be set to nil, info or debug
nil: Does not log anything
info: Logs urls and response's http header
debug: Logs urls and the entire http response.

When non-nil, information is logged to `consult-web-log-buffer-name'."
  :type '(choice
          (const :tag "No Logging" nil)
          (const :tag "Just HTTP Header" info)
          (const :tag "Full Response" debug)))

(defcustom consult-web-group-by :domain
  "What field to use to group the results in the minibuffer?

By default it is set to :domain. but can be any of:

  :url      group by URL
  :domain   group by the domain of the URL
  :source   group by source
 "
  :type '(radio (const :tag "url path" :url)
                (const :tag "domain of url path":domain)
                (const :tag "name of the search engine or source" :source)
                (const :tag "custom other field (constant)" :any)
                (const :tag "do not group" nil)))


(defcustom consult-web-multi-sources  (list)
  "List of sources used by `consult-web-multi'.

This variable is a list of strings, each string being name of a source.
The source name has to be a key from `consult-web-sources-alist'.
Sources can be made with the convinient macro `consult-web-define-source'."
  :type '(choice (repeat :tag "list of source names" string)))

(defcustom consult-web-omni-sources  (list)
"List of sources used by `consult-web-omni'.

This variable is a list of strings or symbols;
 - strings can be name of a source, a key from `consult-web-sources-alist',
which can be made with the convinient macro `consult-web-define-source'
or by using `consult-web--make-srouce-from-consult-source'.
 - symbols can be other consult sources
(see `consult-buffer-sources' for example.)"
:type '(repeat :tag "list of source names" (choice (string symbol))))

(defcustom consult-web-dynamic-omni-sources  (list)
"List of sources used by `consult-web-dynamic-omni'.

This variable is a list of strings, each string being name of a source.
The source name has to be a key from `consult-web-sources-alist'.
Sources can be made with the convinient macro `consult-web-define-source'
or by using `consult-web--make-srouce-from-consult-source'."
  :type '(choice (repeat :tag "list of source names" string)))

(defcustom consult-web-scholar-sources  (list)
  "List of sources used by `consult-web-scholar'.

This variable is a list of strings, each string being name of a source.
The source name has to be a key from `consult-web-sources-alist'.
Sources can be made with the convinient macro `consult-web-define-source'
or by using `consult-web--make-srouce-from-consult-source'."
  :type '(choice (repeat :tag "list of source names" string)))

(defcustom consult-web-dynamic-sources  (list)
  "List of sources used by `consult-web-dynamic'.

This variable is a list of strings, each string being name of a source.
The source name has to be a key from `consult-web-sources-alist'.
Sources can be made with the convinient macro `consult-web-define-source'
or by using `consult-web--make-srouce-from-consult-source'."
  :type '(choice (repeat :tag "list of source names" string)))

(defcustom consult-web-highlight-matches t
  "Should `consult-web' highlight search queries in the minibuffer?"
  :type 'boolean)


(defcustom consult-web-default-interactive-command #'consult-web-multi
  "Which command should `consult-web' call?"
  :type '(choice (function :tag "(Default) Search with dynamic completion (i.e. `consult-web-dynamic')" #'consult-web-dynamic)
                 (function :tag "Search without dynamic completion (i.e. `consult-web-multi')"  #'consult-web-multi)
                 (function :tag "Search academic research literature (i.e. `consult-web-scholar')"  #'consult-web-scholar)
                 (function :tag "Custom function")))

(defcustom consult-web-retrieve-backend #'consult-web-url-retrieve-sync
  "Which command should `consult-web' use for url requests?"
  :type '(choice (function :tag "(Default) url-retrieve backend" #'consult-web-url-retrieve-sync)
                 (function :tag "Emacs Request Backend"  #'consult-web-request)))

(defcustom consult-web-default-autosuggest-command nil
  "Which command should `consult-web' use for auto suggestion on search input?"
  :type '(choice (function :tag "(default) use brave autosuggestion (i.e. `consult-web-dynamic-brave-autosuggest')" #'consult-web-dynamic-brave-autosuggest)
                 (function :tag "use google autosuggestion (i.e. `consult-web-dynamic-google-autosuggest')" #'consult-web-dynamic-google-autosuggest)
                 (function :tag "custom function")))

(defcustom consult-web-dynamic-input-debounce consult-async-input-debounce
  "Input debounce for dynamic commands.

The dynamic collection process is started only when
there has not been new input for consult-web-dynamic-input-debounce seconds. This is similarto `consult-async-input-debounce' but
specifically for consult-web dynamic commands.

By default inherits from `consult-async-input-debounce'."
  :type '(float :tag "delay in seconds"))


(defcustom consult-web-dynamic-input-throttle consult-async-input-throttle
  "Input throttle for dynamic commands.

The dynamic collection process is started only every
`consult-web-dynamic-input-throttle' seconds. this is similar
to `consult-async-input-throttle' but specifically for
consult-web dynamic commands.

By default inherits from `consult-async-input-throttle'."
  :type '(float :tag "delay in seconds"))

(defcustom consult-web-dynamic-refresh-delay consult-async-refresh-delay
  "refreshing delay of the completion ui for dynamic commands.

The completion UI is only updated every
`consult-web-dynamic-refresh-delay' seconds.
This is similar to `consult-async-refresh-delay' but specifically
for consult-web dynamic commands.

By default inherits from `consult-async-refresh-delay'. "
  :type '(float :tag "delay in seconds"))

;;; Other Variables

(defvar consult-web-sources--all-modules-list (list)
"List of all source modules.")

(defvar consult-web-category 'consult-web
  "Category symbol for the `consult-web' package.")

(defvar consult-web-scholar-category 'consult-web-scholar
  "Category symbol for the `consult-web' package.")

(defvar consult-web--selection-history (list)
  "History variable that keeps selected items.")

(defvar consult-web--search-history (list)
  "History variable that keeps search terms.")

(defvar consult-web-sources-alist (list)
  "Alist of search engine or ai assistant sources.

This is an alist mapping source names to source property lists.
This alist is used to define how to process data form
a source (e.g. format data) or find what commands to run on
selecting candidates from a source, etc.

You can use the convinient macro `consult-web-define-source'
or the command `consult-web--make-srouce-from-consult-source'
to add to this alist.")

(defvar consult-web--override-group-by nil
"Override grouping in `consult-group' based on user input.

This is used in dynamic collection to change grouping.")

(defvar consult-web--current-sources (list)
"List of sources of the candidates in the current minibuffer.

This is used for defining narrow functions
(e.g. `consult-web--dynamic-narrow-function'."
)

;;; Faces

(defface consult-web-default-face
  `((t :inherit 'default))
"Default face used for listing items in minibuffer.")

(defface consult-web-prompt-face
  `((t :inherit 'font-lock-variable-use-face))
"The face used for prompts in minibuffer.")

(defface consult-web-engine-source-face
  `((t :inherit 'font-lock-variable-use-face))
"The face for search engine source types in minibuffer.")

(defface consult-web-ai-source-face
  `((t :inherit 'font-lock-operator-face))
"The face for AI assistant source types in minibuffer.")

(defface consult-web-files-source-face
  `((t :inherit 'font-lock-number-face))
"The face for file source types in minibuffer.")

(defface consult-web-notes-source-face
  `((t :inherit 'font-lock-warning-face))
"The face for notes source types in minibuffer.")

(defface consult-web-scholar-source-face
  `((t :inherit 'font-lock-function-call-face))
"The face for academic literature source types in minibuffer.")

(defface consult-web-domain-face
  `((t :inherit 'font-lock-variable-face))
"The face for domain annotation in minibuffer.")

(defface consult-web-path-face
  `((t :inherit 'font-lock-warning-face))
"The face for path annotation in minibuffer.")

(defface consult-web-source-face
  `((t :inherit 'font-lock-comment-face))
"The face for source annotation in minibuffer.")

(defface consult-web-highlight-match-face
  `((t :inherit 'consult-highlight-match))
  "Highlight match face for `consult-web'.")

(defface consult-web-preview-match-face
  `((t :inherit 'consult-preview-match))
  "Preview match face in `consult-web' preview buffers.")

;;; Bakcend Functions

(defun consult-web--set-string-width (string width &optional prepend)
  "Sets the STRING width to a fixed value, WIDTH.

If the STRING is longer than WIDTH, it truncates the STRING
 and adds ellipsis, \"...\". if the STRING is shorter,
it adds whitespace to the STRING.
If PREPEND is non-nil, it truncates or adds whitespace from
 the beginning of STRING, instead of the end."
  (let* ((string (format "%s" string))
         (w (string-width string)))
    (when (< w width)
      (if prepend
          (setq string (format "%s%s" (make-string (- width w) ?\s) (substring string)))
        (setq string (format "%s%s" (substring string) (make-string (- width w) ?\s)))))
    (when (> w width)
      (if prepend
          (setq string (format "...%s" (substring string (- w (- width 3)) w)))
        (setq string (format "%s..." (substring string 0 (- width (+ w 3)))))))
    string))

(defun consult-web--justify-left (string prefix maxwidth)
  "Sets the width of STRING+PREFIX justified from left.
It uses `consult-web--set-string-width' and sets the width
 of the concatenate of STRING+PREFIX
(e.g. `(concat PREFIX STRING)`) within MAXWIDTH.
This is used for aligning marginalia info in minibuffer."
  (let ((s (string-width string))
        (w (string-width prefix)))
    (if (> maxwidth w)
    (consult-web--set-string-width string (- maxwidth w) t)
    string
          )
    ))

(defun consult-web--highlight-match (regexp str ignore-case)
  "Highlights REGEXP in STR.

If a regular expression contains capturing groups,
 only these are highlighted.
If no capturing groups are used, highlight the whole match.
Case is ignored, if ignore-case is non-nil.
(This is adapted from `consult--highlight-regexps'.)"
  (let ((i 0))
    (while (and (let ((case-fold-search ignore-case))
                  (string-match regexp str i))
                (> (match-end 0) i))
      (let ((m (match-data)))
        (setq i (cadr m)
              m (or (cddr m) m))
        (while m
          (when (car m)
            (add-face-text-property (car m) (cadr m)
                                     'consult-web-highlight-match-face nil str)
            )
          (setq m (cddr m))))))
  str)

(defun consult-web--overlay-match (match-str buffer ignore-case)
  "Highlights MATCH-STR in BUFFER using an overlay.
If IGNORE-CASE is non-nil, it uses case-insensitive match.

This is provided for convinience,
if needed in formating candidates or preview buffers."
(with-current-buffer (or (get-buffer buffer) (current-buffer))
  (remove-overlays (point-min) (point-max) 'consult-web-overlay t)
  (goto-char (point-min))
  (let ((case-fold-search ignore-case)
        (consult-web-overlays (list)))
    (while (search-forward match-str nil t)
      (when-let* ((m (match-data))
                  (beg (car m))
                  (end (cadr m))
                  (overlay (make-overlay beg end))
                  )
        (overlay-put overlay 'consult-web-overlay t)
        (overlay-put overlay 'face 'consult-web-highlight-match-face)
        )))))

(defun consult-web-overlays-toggle (&optional buffer)
  "Toggles overlay highlights in consult-web view/preview buffers."
(interactive)
(let ((buffer (or buffer (current-buffer))))
(with-current-buffer buffer
  (dolist (o (overlays-in (point-min) (point-max)))
    (when (overlay-get o 'consult-web-overlay)
      (if (and (overlay-get o 'face) (eq (overlay-get o 'face) 'consult-web-highlight-match-face))
          (overlay-put o 'face nil)
         (overlay-put o 'face 'consult-web-highlight-match-face))
      )
))))

(defun consult-web--make-url-string (url params &optional ignore-keys)
"Adds key value pairs in PARAMS to URL as “&key=val”.

PARMAS should be an alist with keys and values to add to the URL.
Does not add keys for the key in IGNORE-KEYS list."

  (let* ((url (if (equal (substring-no-properties url -1 nil) "?")
                 url
               (concat url "?")))
         (list (append (list url) (cl-loop for (key . value) in params
                                           collect
                                           (unless (member key ignore-keys)
                                             (format "&%s=%s" key value))))))
  (mapconcat #'identity list)))

(defun consult-web-properties-to-plist (string &optional ignore-keys)
"Returns a plist of the text properties of STRING.

Ommits keys in IGNORE-KEYs."
(let ((properties (text-properties-at 0 string))
      (pl nil))
  (cl-loop for k in properties
           when (keywordp k)
           collect (unless (member k ignore-keys) (push (list k (plist-get properties k)) pl)))
  (apply #'append pl)))

(defun consult-web-hashtable-to-plist (hashtable &optional ignore-keys)
"Converts a HASHTABLE to a plist.

Ommits keys in IGNORE-KEYS."

(let ((pl nil))
    (maphash
     (lambda (k v)
       (unless (member k ignore-keys)
         (push (list k v) pl)))
     hashtable)
    (apply #'append pl)))

(defun consult-web-expand-variable-function (var)
"Call the function if VAR is a function"
  (if (functionp var)
                 (funcall var)
    var))

(defun consult-web--log (string)
  "Logs the response from `consult-web-url-retrieve-sync' in `consult-web-log-buffer-name'."
   (with-current-buffer (get-buffer-create consult-web-log-buffer-name)
     (goto-char (point-min))
     (insert "**********************************************\n")
     (goto-char (point-min))
     (insert (format-time-string "%F - %T%n" (current-time)))
     (insert string)
     (insert "\n")
     (goto-char (point-min))
     (insert "\n\n**********************************************\n")))

(defun consult-web--parse-http-response (&optional buffer)
  "Parse the first header line such as \"HTTP/1.1 200 OK\"."
(with-current-buffer (or buffer (current-buffer))
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "\\=[ \t\n]*HTTP/\\(?1:[0-9\\.]+\\) +\\(?2:[0-9]+\\)" url-http-end-of-headers t)
    `(:http-version ,(match-string 1) :code ,(string-to-number (match-string 2)))))))

(cl-defun consult-web--url-retrieve-synchronously (url &rest settings &key params headers parser data type error encoding timeout)
"Retrieves URL synchronously.

Passes all the arguments to url-retriev and fetches the results.

PARAMS are parameters added to the base url using `consult-web--make-url-string'.
HEADERS are headers passed to `url-request-extra-headers'.
DATA are http request data passed to `url-request-data'.
TYPE is the http request type (e.g. “GET”, “POST”)
ERROR
ENCODING
TIMEOUT
PARSER is a function that is executed in the url-retrieve response buffer and the results are returned s the output of this function.
"
  (let* ((url-request-method type)
         (url-request-extra-headers headers)
         (url-request-data data)
         (url-with-params (consult-web--make-url-string url params))
         (response-data nil)
         (buffer (if timeout
                     (with-timeout
                         (timeout
                          (setf response-data (plist-put response-data :status 'timeout))
                          nil)
                       (url-retrieve-synchronously url-with-params t))
                   (url-retrieve-synchronously url-with-params t))
                 ))

    (when buffer
      (with-current-buffer buffer
        (when consult-web-log-level
          (save-excursion
            (goto-char (point-min))
            (cond
             ((eq consult-web-log-level 'info)
              (consult-web--log (format "URL: %s\nRESPONSE: %s" url (buffer-substring (point-min) (pos-eol)))))
             ((eq consult-web-log-level 'debug)
                 (consult-web--log (format "URL: %s\n\nRESPONSE-HEADER:\n%s\n\nRESPONSE-BODY: %s\n" url (buffer-substring (point-min) url-http-end-of-headers) (buffer-substring url-http-end-of-headers (point-max))))))
            ))

        (let* ((response-header (buffer-substring (point-min) url-http-end-of-headers))
               (response-content (buffer-substring (+ url-http-end-of-headers 1) (point-max)))
               (response-status (consult-web--parse-http-response))
               )
          (delete-region (point-min) (+ url-http-end-of-headers 1))

          (when-let ((parsed-data (funcall parser)))
            (setf response-data (plist-put response-data :data parsed-data))
            )

          (when response-header
            (setf response-data (plist-put response-data :header response-header)))

          (when response-status
            (setf response-data (plist-put response-data :status response-status)))

          (when response-content
            (setf response-data (plist-put response-data :content response-content)))

          )))
    response-data
    ))

(defun consult-web--url-response-body (response-data)
"Extracts the response body from `url-retrieve'."
(plist-get response-data :data))

(cl-defun consult-web-url-retrieve-sync (url &key params headers parser data type error encoding timeout)
"Retrieves URL synchronously.

Passes all the arguments to `consult-web--url-retrieve-synchronously' and in trun to `url-retrieve' fetches the results.

PARAMS are parameters added to the base url using `consult-web--make-url-string'.
HEADERS are headers passed to `url-request-extra-headers'.
DATA are http request data passed to `url-request-data'.
TYPE is the http request type (e.g. “GET”, “POST”)
ERROR
ENCODING
TIMEOUT
PARSER is a function that is executed in the url-retrieve response buffer and the results are returned s the output of this function.
"
  (let ((type (or type "GET"))
        (encoding (or encoding 'utf8))
        (timeout (or timeout consult-web-default-timeout))
        )
    (consult-web--url-response-body
     (consult-web--url-retrieve-synchronously url :params params :headers headers :parser parser :data data :type type :error error :encoding encoding :timeout timeout))))

  (cl-defun consult-web--error-handler (&rest args &key symbol-status error-thrown &allow-other-keys)
    "Handles errors for `consult-web-request'."
    (message "request: %s - %s" symbol-status error-thrown))

  (cl-defun consult-web-request (url &rest args &key params headers data parser placeholder error &allow-other-keys)
    "Convinient wrapper for `request'.

Passes all the arguments to request and fetches the results *synchronously*.

Refer to `request' documents for details."
    (unless (functionp 'request)
      (error "Request backend not available. Either install the package “emacs-request” or change the custom variable `consult-web-retrieve-backend'"))
    (let (candidates)
      (request
        url
        :sync t
        :params params
        :headers headers
        :parser parser
        :error (or error #'consult-web--error-handler)
        :data data
        :encoding 'utf-8
        :success (cl-function (lambda (&key data &allow-other-keys)
                                (setq candidates data))))

      candidates))

(defun consult-web-dynamic--split-thingatpt (thing &optional split-initial)
  "Return THING at point.
If SPLIT-INITIAL is non-nil, use `consult--async-split-initial' to format the string."
  (when-let (str (thing-at-point thing t))
    (if split-initial
        (consult--async-split-initial str)
      str)))

(defun consult-web--table-to-formatted-candidate-simple (table &optional face &rest args)
"Returns a formatted candidate for TABLE.

TABLE is a hashtable that stores metadata for a consult-web candidate.
Returns a cons set of `key . value`;
The key is the value of :title key in the TABLE.
The value is all the (key value) pairs in the table as a plist.
"
           (let* ((query (gethash :query table))
                  (title (format "%s" (gethash :title table)))
                  (title-str (consult-web--set-string-width title (floor (* (frame-width) 0.4))))
                  (pl (consult-web-hashtable-to-plist table))
                   )
              (apply #'propertize title-str pl)
))

(defun consult-web--table-to-formatted-candidate-searchable (table &optional face &rest args)
"Formats a consult-web candidate.

TABLE is a hashtable with metadata for the candidate as (key value) pairs.
Returns a string (from :title field in TABLE) with text-properties that conatin
all the key value pairs in the table.
"
  (let* ((pl (consult-web-hashtable-to-plist table))
         (title (format "%s" (gethash :title table)))
         (url (gethash :url table))
         (urlobj (if url (url-generic-parse-url url)))
         (domain (if (url-p urlobj) (url-domain urlobj)))
         (domain (if (stringp domain) (propertize domain 'face 'consult-web-domain-face)))
         (path (if (url-p urlobj) (url-filename urlobj)))
         (path (if (stringp path) (propertize path 'face 'consult-web-path-face)))
         (source (gethash :source table))
         (source (if (stringp source) (propertize source 'face 'consult-web-source-face)))
         (query (gethash :query table))
         (snippet (gethash :snippet table))
         (snippet (if (and snippet (stringp snippet) (> (string-width snippet) 25)) (concat (substring snippet 0 22) "...") snippet))
         (match-str (if (stringp query) (consult--split-escaped (car (consult--command-split query))) nil))
         (title-str (consult-web--set-string-width title (floor (* (frame-width) 0.4))))
         (title-str (propertize title-str 'face (or face 'consult-web-default-face)))
         (extra-args (consult-web-hashtable-to-plist table '(:title :url :search-url :query :source :snippet)))
         (str (concat title-str (if domain (concat "\t" domain (if path path))) (if snippet (format "\s\s%s" snippet)) (if source (concat "\t" source)) (if extra-args (format "\s\s%s" extra-args))))
         (str (apply #'propertize str pl))
         )
    (if consult-web-highlight-matches
        (cond
         ((listp match-str)
          (mapcar (lambda (match) (setq str (consult-web--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-web--highlight-match match-str str t)))))
    str))

(defun consult-web--format-candidates-list (list &optional format-func face)
"Format a LIST of candidates.

LIST is a list of hashtables, each representing one candidate.
FORMAT-FUNC is a function that is used to format candidates if provided.
Returns a list of formatted candidates using either FORMAT-FUNC or otherwise uses default formating for the source retrieved from `consult-web-sources-alist'."
  (mapcar (lambda (table)
            (let* ((source (gethash :source table))
                  (format-func (or format-func
                         (plist-get (cdr (assoc source consult-web-sources-alist)) :format-func)
                         #'consult-web--table-to-formatted-candidate-searchable))
                  (face (or face
                         (plist-get (cdr (assoc source consult-web-sources-alist)) :face)
                         'consult-web-default-face))
                  )
              (funcall format-func table face))) list))

(defun consult-web--annotate-function (cand)
"Annotates each candidate in the minibuffer.

This is provided for convinience to be passed as `:annotate' key when making sources using `consult-web-define-source'.
For more info on annotation refer to `consult' manual, particularly 'consult--read' and `consult--read-annotate' documentation."

    (let* ((url (get-text-property 0 :url cand))
           (urlobj (if url (url-generic-parse-url url)))
           (domain (if (url-p urlobj) (url-domain urlobj) nil))
           (path (if (url-p urlobj) (url-filename urlobj) nil))
           (url-str nil)
           (source (get-text-property 0 :source cand))
           (snippet (get-text-property 0 :snippet cand))
           (extra-args (consult-web-properties-to-plist cand '(:url :source :title :search-url :query :snippet :model :backend))))
      (if domain (setq domain (propertize domain 'face 'consult-web-domain-face)))
      (if path (setq path (propertize path 'face 'consult-web-path-face)))
      (if (and snippet (stringp snippet) (> (string-width snippet) 25)) (setq snippet (concat (substring snippet 0 22) "...")))
      (setq url-str (concat (if domain domain) (if path path)))
      (unless (string-empty-p url-str) (setq url url-str))
      (when (and url (> (string-width url) (floor (* (frame-width) 0.4))))
        (setq url (consult-web--set-string-width url (floor (* (frame-width) 0.4)))))
      (concat (if url (format "\s%s" url)) (if source (format "\t%s" source)) (if snippet (format "\s\s%s" snippet)) (if extra-args (format "\t%s" extra-args)))
    ))

(defun consult-web--group-function (group-by cand transform)
  "Group candidates by GROUP-BY keyword.

This is passed as GROUP to `consult--read' on candidates and is used to define the grouping for CAND. "
  (let* ((group-by (or consult-web--override-group-by group-by consult-web-group-by))
         (group-by (if (not (keywordp group-by)) (intern (concat ":" (format "%s" group-by))) group-by))
         (name (or (if group-by (get-text-property 0 group-by cand) "N/A"))))
    (cond
     ((equal group-by :domain)
      (when-let* ((url (get-text-property 0 :url cand))
                  (urlobj (if url (url-generic-parse-url url) nil))
                  (domain (if (url-p urlobj) (url-domain urlobj))))
        (setq name domain))))
  (if transform (substring cand) name)))

(defun consult-web--narrow-function (source)
"Make a narrowing (key . value) pair for the SOURCE string.

key is the first character, and value is the entire source STRING.
For example when “wikipedia” is passed as a source, it returns (w . “wikipedia”)."
 `(,(string-to-char source) . ,source)
)

(defun consult-web--dynamic-narrow-function ()
  "Dynamically makes a list of (key . value) for all the sources in the current list of candidates using `consult-web--narrow-function'."
  (let* ((narrow-pred (lambda (cand)
                       (if-let ((source (get-text-property 0 :source (car cand))))
                         (equal (string-to-char source) consult--narrow)
                           )))
        (narrow-keys (mapcar (lambda (c) (cons (string-to-char c) c))
                              consult-web--current-sources)))
`(:Predicate ,narrow-pred :keys ,narrow-keys)
))

(defun consult-web--lookup-function ()
"Lookup function for `consult-web' minibuffer candidates.

This is passed as LOOKUP to `consult--read' on candidates and is used to format the output when a candidate is selected."
  (lambda (sel cands &rest args)
     (let* ((info (or (car (member sel cands)) ""))
            (title (get-text-property 0 :title info))
            (url (get-text-property 0 :url info))
            )
      (apply #'propertize (or title url "nil") (or (text-properties-at 0 info) (list)))
      )))

(defun consult-web--default-url-preview (cand)
"Default function to use for previewing CAND."
(when-let* ((url (cond
                  ((listp cand)
                   (or (get-text-property 0 :url (car cand)) (car cand)))
                  (t
                   (or (get-text-property 0 :url cand) cand))))
            (buff (funcall consult-web-default-preview-function url)))
               (funcall (consult--buffer-preview) 'preview
                        buff
                        )
               )
)

(cl-defun consult-web--make-state-function (&rest args &key setup preview exit return &allow-other-keys)
"Convinient wrapper for `consult-web' to make custom state functions.

This can be passed as STATE to `consult--read' on candidates and is
used to define actions when setting up, previewing or selecting a
candidate. Refer to `consult--read' documentation for more details."
    (lambda (action cand &rest args)
      (if cand
          (pcase action
            ('setup
             (funcall setup cand))
            ('preview
             (funcall preview cand))
            ('exit
             (funcall exit cand))
            ('return
             (funcall return cand))
             )))
      )

(defun consult-web--dynamic-state-function ()
"State function for `consult-web' minibuffer candidates.

This is passed as STATE to `consult--read' on candidates and is used
to define actions that happen when a candidate is previewed or
selected.
The preview and retrun actions are retrieve from `consult-web-sources-alist'."
  (lambda (action cand &rest args)
    (if cand
        (let* ((source (get-text-property 0 :source cand))
               (state (plist-get (cdr (assoc source consult-web-sources-alist)) :state))
               (preview (plist-get (cdr (assoc source consult-web-sources-alist)) :on-preview))
               (return (plist-get (cdr (assoc source consult-web-sources-alist)) :on-return)))
          (if state
              (funcall state action cand args)
              (pcase action
                ('preview
                 (if preview (funcall preview cand) (consult-web--default-url-preview cand)))
                ('return
                 (if return (funcall return cand) cand))
                ))
          )))
    )

(defun consult-web--default-callback (cand)
"Default CALLBACK for CAND.

The CALLBACK is called when a CAND is selected.
When making consult-web sources, if a CALLBACK is not provided, this
CALLBACK is used as a fall back."
  (if-let ((url (get-text-property 0 :url cand)))
      (funcall consult-web-default-browse-function url)))

(defun consult-web--read-search-string (&optional initial)
  (consult--read nil
                 :prompt "Search: "
                 :initial initial
                 :category 'consult-web
                 :history 'consult-web--search-history
                 :add-history (delq nil
                                    (cl-remove-duplicates
                                     (append (mapcar (lambda (thing) (consult-web-dynamic--split-thingatpt thing nil))
                                             (list 'number 'word 'sexp 'symbol 'url 'filename 'sentence 'line)) (list isearch-string))))
                                        ))

(defun consult-web--extract-opt-pair (opt opts ignore-opts)
  "Extracts a pair of (OPT . value) from a list OPTS.

values is the next element after OPT in OPTS.
Excludes keys in IGNORE_OPTS.
This i suseful for example to extract key value pairs
from command-line options in alist of strings"
 (let* ((key (cond
             ((string-match "--.*$" opt)
             (intern (concat ":" (replace-regexp-in-string "--" "" opt))))
             ((string-match ":.*$" opt)
              (intern opt))
             (t nil)))
       (val (or (nth (+ (cl-position opt opts :test 'equal) 1) opts) "nil"))
       (val (cond
             ((string-match "--.*$\\|:.*$" val)
              nil)
             ((stringp val)
              (intern val)))))
   (when (and key (not (member opt ignore-opts)))
   (cons key val))
))

(defun consult-web--split-args (args)
  "Splits ARGS to remaining args and input
input is the last element of ARGS
remaining args are turned into a plist"
 (pcase-let* ((input (car (last args)))
              (args (seq-difference (remove input args) '((nil nil) (nil)))) ;;this is hacky should find a better way
              (`(,arg . ,opts) (consult--command-split input))
              (remaining-opts (list)))
    (cl-loop for opt in opts
             do
             (pcase-let* ((`(,key . ,val) (consult-web--extract-opt-pair opt opts (list "--group" ":group"))))

        (when key
          (setq args (append args (list key val)))
          (setq remaining-opts (cl-delete-duplicates (append remaining-opts (list opt (format "%s" val))))))
        ))

    (setq opts (seq-difference opts remaining-opts))

    (when (member "-n" opts)
      (setq args (append args `(:count ,(intern (or (nth (+ (cl-position "-n" opts :test 'equal) 1) opts) "nil"))))))

    (when (member "-p" opts)
      (setq args (append args `(:page ,(intern (or (nth (+ (cl-position "-p" opts :test 'equal) 1) opts) "nil")))))
      )

    (if (or (member "-g" opts) (member ":group" opts) (member "--group" opts))
      (cond
       ((member "-g" opts)
        (setq consult-web--override-group-by (intern (or (nth (+ (cl-position "-g" opts :test 'equal) 1) opts) "nil")))
        )
       ((member "--group" opts)
        (setq consult-web--override-group-by (intern (or (nth (+ (cl-position "--group" opts :test 'equal) 1) opts) "nil")))
        )
       ((member ":group" opts)
        (setq consult-web--override-group-by (intern (or (nth (+ (cl-position ":group" opts :test 'equal) 1) opts) "nil")))
        ))
       (setq consult-web--override-group-by nil)
        )
    (list (or arg input) args)
))

(defun consult-web-dynamic--list-from-sources (sources &optional format-func face &rest args)
  "Builds ARGS from user input and collects candidates from all
SOURCES."
  (pcase-let* ((`(,input ,args) (consult-web--split-args args)))
    (cond
     ((and (listp sources))
      (apply 'append
             (cl-loop for source in sources
                      collect
                      (consult-web--format-candidates-list
                       (apply source input args)))))
     ((functionp sources)
      (consult-web--format-candidates-list
       (apply sources input args) format-func face))
     (t
      (error "%s is not a consult-web-source!")))))

(defun consult-web-dynamic--collection (sources &optional format-func face &rest args)
"This is a wrapper using `consult--dynamic-collection' and
`consult-web-dynamic--list-from-sources'."
(consult--dynamic-collection (apply-partially #'consult-web-dynamic--list-from-sources sources format-func face args)))

(defun consult-web-dynamic--internal (prompt collection &optional initial category lookup history-var preview-key)
"internal function to run `consult--read'.

PROMPT COLLECTION and INITIAL are passed to `consult--read'."
(consult--read collection
                   :prompt prompt
                   :group (apply-partially #'consult-web--group-function :source)
                   :narrow (consult-web--dynamic-narrow-function)
                   :lookup (or lookup (consult-web--lookup-function))
                   :state (consult-web--dynamic-state-function)
                   :initial (consult--async-split-initial initial)
                   :category (or category 'consult-web)
                   :preview-key (and consult-web-show-preview (or preview-key consult-web-preview-key))
                   :history (cond
                             ((eq history-var t)
                              t)
                             ((eq history-var nil)
                              nil)
                             ((and history-var (symbolp history-var))
                              `(:input ,history-var)))
                   :add-history (delq nil
                                    (cl-remove-duplicates
                                     (append (mapcar (lambda (thing) (consult-web-dynamic--split-thingatpt thing t))
                                             (list 'number 'word 'sexp 'symbol 'url 'filename 'sentence 'line)) (list isearch-string))))
                   :sort t
                   )
)

(defun consult-web--source-name (source-name &optional suffix)
  "Returns a symbol for SOURCE-NAME variable.

The variable is consult-web--source-%s (%s=source-name).
Adds suffix to the name if provided."
  (intern (format "consult-web--source-%s" (concat (replace-regexp-in-string " " "-" (downcase source-name)) (if suffix (downcase suffix) nil)))))

(defun consult-web--source-generate-docstring (source-name)
  "Makes a generic documentation string for SOURCE-NAME.

This is used in `consult-web-define-source' macro to make generic
docstrings for variables."
  (format "consult-web source for %s.\n \nThis function was defined by the macro `consult-web-define-source'."
          (capitalize source-name)))

(defun consult-web--func-name (source-name &optional prefix suffix)
  "Make a function symbol for interactive command for SOURCE-NAME.

Adds prefix and suffix if non-nil."
  (intern (concat "consult-web-" (if prefix prefix) (replace-regexp-in-string " " "-" (downcase source-name)) (if suffix suffix))))

(defun consult-web--func-generate-docstring (source-name &optional dynamic)
  "Make a generic documentaion string for an interactive command.

This is used to make docstring for function made by `consult-web-define-source'."
  (concat "consult-web's " (if dynamic "dynamic ") (format "interactive command to search %s."
                                                             (capitalize source-name))))

(defun consult-web--make-source-list (source-name request format annotate face narrow-char state preview-key category lookup selection-history input args)
  "Internal function to make a source for `consult--multi'.

Do not use this function directly, use `consult-web-define-source' macro
instead."
  `(:name ,source-name
          ,(if (and annotate face) :face)
          ,(if (and annotate face) (cond
            ((eq face t)
             'consult-web-default-face)
            (t face)))
          :narrow ,narrow-char
          :state ,(or state #'consult-web--dynamic-state-function)
          :category ,(or category 'consult-web)
          :history ,selection-history
          :add-history (delq nil
                                    (cl-remove-duplicates
                                     (append (mapcar (lambda (thing) (consult-web-dynamic--split-thingatpt thing))
                                             (list 'number 'word 'sexp 'symbol 'url 'filename 'sentence 'line)) (list isearch-string))))
          :items ,(funcall #'consult-web--format-candidates-list  (funcall request input args) format)

          :annotate ,(cond
                      ((and annotate (functionp annotate))
                       annotate)
                      ((eq annotate t)
                       #'consult-web--annotate-function)
                      (t nil))
          :lookup (if (and lookup (functionp lookup))
                      lookup
                    (consult-web--lookup-function))
          :preview-key ,(and consult-web-show-preview (or preview-key consult-web-preview-key))
          :sort t
          )
  )

(defun consult-web--call-static-command (input no-callback args request format face state source-name category lookup selection-history-var annotate preview-key on-callback)
  "Internal function to make static `consult--read' command.

Do not use this function directly, use `consult-web-define-source' macro
instead."
  (let* ((input (or input
                    (and consult-web-default-autosuggest-command (funcall-interactively consult-web-default-autosuggest-command))
                    (consult-web--read-search-string)))
         (selected
          (consult--read (funcall #'consult-web--format-candidates-list (funcall  request input args) format face)
                         :state (or state (consult-web--dynamic-state-function))
                         :require-match nil
                         :prompt (concat "[" (propertize (format "%s" (consult-web--func-name source-name)) 'face 'consult-web-prompt-face) "]" " Search:  ")
                         :sort t
                         :history (cond
                                   ((eq selection-history-var nil)
                                    nil)
                                   ((eq selection-history-var t)
                                    t)
                                   ((symbolp selection-history-var)
                                    selection-history-var))
                         :add-history (delq nil
                                            (cl-remove-duplicates
                                             (append (mapcar (lambda (thing) (consult-web-dynamic--split-thingatpt thing))
                                                             (list 'number 'word 'sexp 'symbol 'url 'filename 'sentence 'line)) (list isearch-string))))
                         :group (if (functionp consult-web-group-by) consult-web-group-by (apply-partially #'consult-web--group-function consult-web-group-by))
                         :category (or category 'consult-web)
                         :lookup (if (and lookup (functionp lookup))
                                     lookup
                                   (consult-web--lookup-function))
                         :annotate (cond
                                    ((and annotate (functionp annotate)) annotate)
                                    ((eq annotate t) #'consult-web--annotate-function)
                                    (t nil))
                         :preview-key (and consult-web-show-preview (or preview-key consult-web-preview-key))
                         ))
         (source (get-text-property 0 :source selected))
         )
    (unless no-callback
        (if source
            (funcall (plist-get (cdr (assoc source consult-web-sources-alist)) :on-callback) selected)))
    selected)
  )

(defun consult-web--call-dynamic-command (initial no-callback args source-name request category face lookup search-history-var selection-history-var preview-key)
  "Internal function to make dynamic `consult--read' command.

Do not use this function directly, use `consult-web-define-source' macro
 instead."
  (let* ((consult-async-refresh-delay consult-web-dynamic-refresh-delay)
         (consult-async-input-throttle consult-web-dynamic-input-throttle)
         (consult-async-input-debounce consult-web-dynamic-input-debounce)
         (prompt (concat "[" (propertize (format "%s" (consult-web--func-name source-name "dynamic-")) 'face 'consult-web-prompt-face) "]" " Search:  "))
         (collection (consult-web-dynamic--collection (list
                                                       request) nil face nil args))
         (selected (consult-web-dynamic--internal prompt collection initial category lookup search-history-var preview-key))
         (source (get-text-property 0 :source selected))
         (title (get-text-property 0 :title selected)))
    (add-to-history selection-history-var title)
    (unless no-callback
      (funcall (plist-get (cdr (assoc source consult-web-sources-alist)) :on-callback) selected)
      )
    selected
    ))

;;; Macros
;;;###autoload
(cl-defmacro consult-web-define-source (source-name &rest args &key request format on-preview on-return state on-callback lookup dynamic group narrow-char category search-history selection-history face annotate preview-key docstring &allow-other-keys)
  "Macro to make a consult-web-source for SOURCE-NAME.

\* Makes
- source for `consult-web-multi' and/or `consult-web-dynamic'
- interactive commands (static or dynamic) for single source
- adds a new row to to `consult-web-sources-alist' with all the
metadata as a property list.

\* Keyword Arguments

Brief Description:

==========  ==========      =================================================
Keyword     Type            Explanation
==========  ==========      =================================================

REQUEST     (function)      Fetch results from source

FORMAT      (function)      Formats a single candidate

ON-PREVIEW  (function)      Preview action in `consult--read'

ON-RETURN   (function)      Return action in `consult--read'

STATE       (function)      STATE passed to `consult--read'
                            (bypasses ON-PREVIEW and ON-RETURN)

ON-CALLBACK (function)      Function called on selected candidate

DYNAMIC     (boolean/'both) Whether to make dynamic or non-dynamic commands

GROUP       (function)      Passed as GROUP to `consult--read'

ANNOTATE    (function)      Passed as ANNOTATE to `consult--read'

NARROW-CHAR (char)          Ppassed as NARROW to `consult-read'

CATEGORY    (symbol)        Passed as CATEGORY to `consult--read'

HISTORY     (symbol)        Passed as HISTORY to `consult--read'

FACE        (face)          Passed as FACE to `consult--read-multi'

PREVIEW-KEY (key)           Passed as PREVIEW-KEY to `consult--read'

DOCSTRING   (string)        DOCSTRING for the variable created for SOURCE-NAME

===================================================================

Detailed Decription:

REQUEST is a function that takes at least one string argument, INPUT, which is
the search term, and potentially other arguments. Keyword arguments
(e.g. by using `cl-defun') can be passed to this function from
minibuffer prompt using
`consult-async' commandline arguments.
Examples can be found in the wiki pages of the repo or in
“consult-web-sources.el” on the repository webpage or :
URL `https://github.com/armindarvish/consult-web/blob/main/consult-web-sources.el'


FORMAT takes a hashtable and returns a cons with a propertized string as key
 and plist property as value. For an example see
`consult-web--table-to-formatted-candidate-simple' or `consult-web--table-to-formatted-candidate-searchable'.

ON-PREVIEW is used as a function to call on the candidate, when a preview is
requested. It takes one required argument, the candidate. For an example,
see `consult-web-default-preview-function'.

ON-RETURN is used as a function to call on the candidate, when the
candidate is selected. This is passed to consult built-in state
function machinery.
Note that the output of this function will be returned in the consult-web
commands. In consult-web, ON-CALLBACK is used to call further actions on
this returned value. This allows to separate the return value from the
commands and the action that i run on the selected candidates. Therefore
for most use cases, ON-RETURN can just be `#'identity' to get
the candidate back as it is. But if some transformation is needed,
ON-RETURN can be used to transform the selected candidate.


STATE is a function that takes no argument and returns a function for
consult--read STATE argument. For an example see
`consult-web--dynamic-state-function' that builds state function based on
 ON-PREVIEW and ON-RETURN. If STATE is non-nil, instead of using
ON-PREVIEW and ON-RETURN to make a state function, STATE will be directly
used in consult--read.


ON-CALLBACK is the function that is called with one required input argument,
 the selected candidate. For example, see `consult-web--default-callback'
that opens the url of the candidate in the default browser.
Other examples can be found in the wiki pages of the repo or in
“consult-web-sources.el” on the repository webpage or :
URL `https://github.com/armindarvish/consult-web/blob/main/consult-web-sources.el'

DYNAMIC can be a bollean (nil or t) or the symbol 'both.
If nil only \*non-dynamic\* interactive commands are created in this macro.
if t only \*dynamic\* interactive commands are created in this macro.
If something else (e.g. 'both) \*Both\* dynamic and non-dynamic commands
are created.

GROUP, ANNOTATE, NARROW-CHAR, CATEGORY, and PREVIEW-KEY are passed to
`consult--read' or `consult--multi'. See consult's Documentaion for more
 details.

FACE is passed to `consult-multi'. See consult's Documentaion for more
details.


DOCSTRING is used as docstring for the variable consult-web--source-%s
variable that this macro creates for %s=SOURCE-NAME.
"
  (if (symbolp source-name) (setq source-name (eval source-name)))

  `(progn

     ;; make a function that creates a consult--read source for consult-web-multi
     (defun ,(consult-web--source-name source-name "-list") (input &rest args)
       ,(or docstring (consult-web--source-generate-docstring source-name))
       (consult-web--make-source-list ,source-name ,request ,format ,annotate ,face ,narrow-char ,state ,preview-key ,category ,lookup ,selection-history input args)
       )

     ;; make a static interactive command consult-web-%s (%s=source-name)
     (unless (eq ,dynamic t)
       (defun ,(consult-web--func-name source-name) (&optional input no-callback &rest args)
         ,(or docstring (consult-web--func-generate-docstring source-name))
         (interactive "P")
         (consult-web--call-static-command input no-callback args ,request ,format ,face ,state ,source-name ,category ,lookup ,selection-history ,annotate ,preview-key ,on-callback)
         ))

     ;; make a dynamic interactive command consult-web-dynamic-%s (%s=source-name)
     (if ,dynamic
         (defun ,(consult-web--func-name source-name "dynamic-") (&optional initial no-callback &rest args)
           ,(or docstring (consult-web--func-generate-docstring source-name t))
           (interactive "P")
           (consult-web--call-dynamic-command initial no-callback args ,source-name ,request ,category ,face ,lookup ,search-history ,selection-history ,preview-key)
           ))

     ;; make a variable called consult-web--source-%s (%s=source-name)
     (defvar ,(consult-web--source-name source-name) (list))
     (setq  ,(consult-web--source-name source-name) (cons ,source-name
                                                          (list :name ,source-name
                                                                :source (consult-web--source-name ,source-name "-list")
                                                                :face ,face
                                                                :request-func ,request
                                                                :format-func (or ,format #'consult-web--table-to-formatted-candidate-searchable)


                                                                :on-preview (or ,on-preview #'consult-web--default-url-preview)
                                                                :on-return (or ,on-return #'identity)
                                                                :on-callback (or ,on-callback #'consult-web--default-callback)
                                                                :state ,state
                                                                :group ,group
                                                                :annotate ,annotate
                                                                :narrow-char ,narrow-char
                                                                :preview-key ,preview-key
                                                                :category (or ',category 'consult-web)
                                                                :search-history ,search-history
                                                                :selection-history ,selection-history
                                                                :interactive-static (and (functionp (consult-web--func-name ,source-name)) (consult-web--func-name ,source-name))
                                                                :interactive-dynamic (and (functionp (consult-web--func-name ,source-name "dynamic-")) (consult-web--func-name ,source-name "dynamic-"))
                                                                )))

     ;; add consult-web--source-%s (%s=source-name) to consult-web-sources-alist
     (add-to-list 'consult-web-sources-alist ,(consult-web--source-name source-name))

     ,source-name))

;;;###autoload
(cl-defmacro consult-web--make-fetch-function (source &rest args &key source-name docstring &allow-other-keys)
  "Make a function for fetching result based on SOURCE.

SOURCE is a source for consult (e.g. a plist that is passed
to consult--red). See `consult-buffer-sources' for examples.
SOURCE-NAME is a string name for SOURCE
DOCSTRING is the docstring for the function that is returned."
  (let* ((source (if (plistp source) source (eval source)))
        (source-name (substring-no-properties (plist-get source :name))))
  `(progn
     ;; make a function that creates a consult--read source for consult-web-multi
     (defun ,(consult-web--source-name source-name "-fetch-results") (input &rest args)
       ,(or docstring (consult-web--source-generate-docstring source-name))
  (let ((results (funcall (plist-get ',source :items)))
        (source (substring-no-properties (plist-get ',source :name))))
    (cl-loop for a in results
             if (string-match (concat ".*" input ".*") a)
             collect
             (let* ((table (make-hash-table :test 'equal))
                    (title a))
           (puthash :title title
                    table)
           (puthash :url nil
                    table)
           (puthash :query input
                    table)
           (puthash :source (substring-no-properties source)
                    table)
           table)))))))

;;;###autoload
(cl-defun consult-web--make-srouce-from-consult-source (consult-source &rest args &key request format on-preview on-return state on-callback group narrow-char category dynamic search-history selection-history face annotate preview-key docstring &allow-other-keys)
"Makes a consult-web source from a consult source, CONSULT-SOURCE.
All othe input variables are passed to `consult-web-define-source'
macro. See `consult-web-define-source' for more details"
  (if (boundp consult-source)
        (let* ((source (eval consult-source))
               (source (if (plistp source) source (eval source)))
               (name (and (plistp source) (substring-no-properties (plist-get source :name))))
               (preview-key (or preview-key (and (plistp source) (plist-get source :preview-key))))
               (narrow-char (or narrow-char (and (plistp source) (plist-get source :narrow))))
               (narrow-char (if (listp narrow-char) (car narrow-char)))
               (face (if (member :face args) face (and (plistp source) (plist-get source :face))))
               (state (if (member :state args) state (and (plistp source) (plist-get source :state))))
               (annotate (if (member :annotate args) annotate (and (plistp source) (plist-get source :annotate))))
               (preview-key (or preview-key (and (plistp source) (plist-get source :preview-key)) consult-web-preview-key))
               (group (or group (and (plistp source)(plist-get source :group))))
               (category (or category (and (plistp source) (plist-get source :category)) 'consult-web)))
          (eval (macroexpand
           `(consult-web-define-source ,name
                                     :docstring ,docstring
                                     :annotate ',annotate
                                     :narrow-char ,narrow-char
                                     :category ',category
                                     :request (or ,request (consult-web--make-fetch-function ,source))
                                     :format ',format
                                     :face ',face
                                     :search-history ',search-history
                                     :selection-history ',selection-history
                                     :on-preview ',on-preview
                                     :on-return ',on-return
                                     :on-callback ',on-callback
                                     :preview-key ,preview-key
                                     :group ',group
                                     :dynamic ',dynamic))))
    (display-warning :warning (format "Consult-web: %s is not available. Make sure `consult-notes' is loaded and set up properly" consult-source)))
  )

;;; Frontend Interactive Commands
(defun consult-web-multi (&optional input sources no-callback &rest args)
  "Interactive “multi-source search”

INPUT is the initial search query.
Searches all sources in SOURCES. if SOURCES is nil
`consult-web-multi-sources' is used.
If NO-CALLBACK is t, only the selected candidate is returned without
any callback action.
"
  (interactive "P")
  (let* ((input (or input
                    (and consult-web-default-autosuggest-command (funcall-interactively consult-web-default-autosuggest-command))
                    (consult-web--read-search-string)))
         (sources (or sources consult-web-multi-sources))
         (sources (remove nil (mapcar (lambda (source) (plist-get (cdr (assoc source consult-web-sources-alist)) :source)) sources)))
         (candidates (consult--slow-operation "The web is a big place, allow me a few seconds..." (mapcar (lambda (func) (funcall func input args)) sources)))
         (selected (consult--multi candidates
                                   :require-match nil
                                   :prompt (concat "[" (propertize "consult-web-multi" 'face 'consult-web-prompt-face) "]" " Search:  ")
                                   :sort t
                                   :annotate nil
                                   :category 'consult-web
                                   :history 'consult-web--selection-history
                                   ))
         (source (get-text-property 0 :source (car selected)))
         )
    (unless no-callback
      (funcall (plist-get (cdr (assoc source consult-web-sources-alist)) :on-callback) (car selected)))
    (car selected)
    ))

(defun consult-web-dynamic (&optional initial sources no-callback &rest args)
  "Interactive “multi-source dynamic search”

INITIAL is the initial search prompt in minibuffer.
Searches all sources in SOURCES. if SOURCES is nil
`consult-web-dynamic-sources' is used.
If NO-CALLBACK is t, only the selected candidate is returned without
any callback action.

This is an interactive command that fetches results form all the sources in `consult-web-dynamic-sources' with dynamic completion meaning that the search term can be dynamically updated by the user
and the results are fetched as the user types.

Additional commandline arguments can be passed in the minibuffer
entry similar to `consult-grep' by typing `--` followed by arguments.

For example the user can enter:

`#consult-web -- -g domain'

this will run a search on all the `consult-web-dynamic-sources' for
the term “consult-web” and then groups the results by the “domain
of the URL” of the results.

Built-in arguments include:

 -g, --groups, or :groups  for grouping (see `consult-web-group-by' and `consult-web--override-group-by'. for more info)

 -n, --count, or :count is passed as the value for COUNT to any source in `consult-web-dynamic-sources'.
If the request function for the source takes a keyword argument for COUNT (e.g. :count value), this is used as the value otherwise it is ignored.

 -p, --page, or :page is passed as the value for PAGE to any source in `consult-web-dynamic-sources'.
If the request function for the source takes a keyword argument for page (e.g. :page value), this is used as the value otherwise it is ignored.

Custom arguments can be passed by using “--ARG value” (or “:ARG value”).
For example, if the user types the following in the minibuffer:
“#how to do web search in emacs? -- --model gpt-4”
The term “how to do web search in emacs?” is passed as the search
term and the “gpt-4” as a keyword argument for :model to every
source in `consult-web-dynamic-sources'. If any request function of
the sources takes a keyword argument for :model, “gpt-4” is
used then.

Once the results are fetched, narrowing down can be done by using “#” after the serach term similar to `consult-grep'.
For example:
“#consult-web#github.com”
uses “consult-web” as the search term, and then narrows the choices to
results that have “github.com” in them.

For more examples, refer to the official documentation of the repo here:
URL `https://github.com/armindarvish/consult-web'.

For more details on consult--async functionalities, see `consult-grep'
and the official manual of consult, here: URL `https://github.com/minad/consult'."
  (interactive "P")
  (let* ((consult-async-refresh-delay consult-web-dynamic-refresh-delay)
         (consult-async-input-throttle consult-web-dynamic-input-throttle)
         (consult-async-input-debounce consult-web-dynamic-input-debounce)
         (sources (or sources consult-web-dynamic-sources))
         (request-sources (remove nil (mapcar (lambda (source)
(plist-get (cdr (assoc source consult-web-sources-alist)) :request-func)) sources)))
         (prompt (concat "[" (propertize "consult-web-dynamic" 'face 'consult-web-prompt-face) "]" " Search:  "))
         (collection (consult-web-dynamic--collection request-sources nil nil args))
         (selected (consult-web-dynamic--internal prompt collection initial 'consult-web nil 'consult-web--search-history))
         (source (get-text-property 0 :source selected)))
        (unless no-callback
          (funcall (plist-get (cdr (assoc source consult-web-sources-alist)) :on-callback) selected))
    selected
    ))

(defun consult-web-scholar (&optional initial sources no-callback &rest args)
  "Interactive “multi-source acadmic literature” search

INITIAL is the initial search prompt in minibuffer.
Searches all sources in SOURCES. if SOURCES is nil
`consult-web-scholar-sources' is used.
If NO-CALLBACK is t, only the selected candidate is returned without
any callback action.

This is similar to `consult-web-dynamic', but runs the search on academic literature sources in `consult-web-scholar-sources'.
Refer to `consult-web-dynamic' for more details."
  (interactive "P")
  (let* ((consult-async-refresh-delay consult-web-dynamic-refresh-delay)
         (consult-async-input-throttle consult-web-dynamic-input-throttle)
         (consult-async-input-debounce consult-web-dynamic-input-debounce)
         (sources (or sources consult-web-scholar-sources))
         (request-sources (remove nil (mapcar (lambda (source)
                                                (plist-get (cdr (assoc source consult-web-sources-alist)) :request-func)) sources)))
         (collection (consult-web-dynamic--collection request-sources nil nil args))
         (selected (consult-web-dynamic--internal (concat "[" (propertize "consult-web-scholar" 'face 'consult-web-prompt-face) "]" " Search:  ") collection initial 'consult-web-scholar nil 'consult-web--search-history))
         (source (get-text-property 0 :source selected)))
    (unless no-callback
      (funcall (plist-get (cdr (assoc source consult-web-sources-alist)) :on-callback) selected)
      )
    selected
    ))

(defun consult-web-omni-get-sources (&optional input)
"Returns a flat list of candidates for input.

Passes input to sources in `consult-web-omni-sources' and returns a
flattend list of sources."
(apply #'append (mapcar (lambda (item) (cond
                                        ((stringp item)
                                         (if-let ((func (plist-get (cdr (assoc item consult-web-sources-alist)) :source)))
                                             (list (funcall func input))))
                                        ((symbolp item)
                                         (eval item))))

 consult-web-omni-sources)))

(defun consult-web-omni (&optional input sources no-callback &rest args)
"Interactive “multi-source omni” search.
This is for using combination of web and local sources defined in
`consult-web-omni-sources'.

Passes INPUT to SOURCES and returns results in minibuffer.
If SOURCES is nil, `consult-web-omni-sources' is used.
If NO-CALLBACK is t, only the selected candidate is returned without
any callback action."
  (interactive)
  (let* ((input (or input  (consult-web-dynamic-brave-autosuggest input) ""))
         (consult-web-default-count 10)
         (sources (or sources (consult-web-omni-get-sources input)))
         (selected (consult--multi sources
                                   :prompt "Select: "
                                   :history 'consult-web--omni-history
                                   :add-history (list (thing-at-point 'word t)
                                                      "")
                                   :sort t
                                   :initial input
                                   ))
         (source (get-text-property 0 :source (car selected))))
    (unless no-callback
      (cond
       ((and source (member source (mapcar #'car consult-web-sources-alist)))
        (funcall (plist-get (cdr (assoc source consult-web-sources-alist)) :on-callback) (car selected)))
       ((and (bufferp (car selected)) (buffer-live-p (car selected)))
        (consult--buffer-action (car selected)))
       (t nil))
      )
    (car selected)
    ))

(defun consult-web-dynamic-omni (&optional initial sources no-callback &rest args)
  "Interactive “multi-source and dynamic omni search”
This is for using combination of web and local sources defined in
`consult-web-dynamic-omni-sources'.

INITIAL is the initial search prompt in minibuffer.
Searches all sources in SOURCES. if SOURCES is nil
`consult-web-dynamic-omni-sources' is used.
If NO-CALLBACK is t, only the selected candidate is returned without
any callback action.

This is a dynamic command and additional arguments can be passed in
the minibuffer. See `consult-web-dynamic' for more details."

  (interactive "P")
  (let* ((consult-async-refresh-delay consult-web-dynamic-refresh-delay)
         (consult-async-input-throttle consult-web-dynamic-input-throttle)
         (consult-async-input-debounce consult-web-dynamic-input-debounce)
         (sources (or sources consult-web-dynamic-omni-sources))
         (request-sources (remove nil (mapcar (lambda (source)
                                                (plist-get (cdr (assoc source consult-web-sources-alist)) :request-func)) sources)))
         (prompt (concat "[" (propertize "consult-web-dynamic-omni" 'face 'consult-web-prompt-face) "]" " Search:  "))
         (collection (consult-web-dynamic--collection request-sources nil nil args))
         (selected (consult-web-dynamic--internal prompt collection initial 'consult-web nil 'consult-web--search-history))
         (source (get-text-property 0 :source selected)))
    (unless no-callback
      (funcall (plist-get (cdr (assoc source consult-web-sources-alist)) :on-callback) selected))
    selected
    ))

(defun consult-web (&rest args)
"Wrapper function that calls the function in `consult-web-default-interactive-command'.

This is for conviniece to call the favorite consult-web interactive command."
  (interactive)
  (apply consult-web-default-interactive-command args))

;;; provide `consult-web' module

(provide 'consult-web)

;;; consult-web.el ends here
