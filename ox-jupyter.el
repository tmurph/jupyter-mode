;;; ox-jupyter.el --- export Jupyter code blocks via Org Babel  -*- lexical-binding: t; -*-

;; Author: Trevor Murphy <trevor.m.murphy@gmail.com>
;; Maintainer: Trevor Murphy <trevor.m.murphy@gmail.com>

;; This file is not part of GNU Emacs.

;; Copyright (C) 2018 Trevor Murphy

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Jupyter Minor Mode aims to make Emacs a full-fledged Jupyter client.
;; The mode provides commands to inspect available kernels, start
;; inferior kernel processes, and connect buffers to existing processes.

;; This library supports Org Babel Export of Jupyter code blocks to
;; notebook format.
;;
;; Enable by customizing `org-export-backends' or with
;;
;;   (require 'ox-jupyter)
;;
;; If you would like to customize how Org opens exported files, use
;;
;;   (push '("ipynb" . system) org-file-apps)
;;
;; to let the operating system choose how to open the file, or use
;;
;;   (push '("ipynb" . "nbopen %s") org-file-apps)
;;
;; to specify your own command (in this case "nbopen").

;;; Code:

(require 'ox)
(require 'jupyter)

;;; Define Back-end

(org-export-define-backend 'jupyter
  '((bold . ox-jupyter--bold)
    (babel-call . ox-jupyter--babel-call)
    (code . ox-jupyter--code)
    (example-block . ox-jupyter--example-block)
    (fixed-width . ox-jupyter--fixed-width)
    (footnote-reference . ox-jupyter--footnote-reference)
    (headline . ox-jupyter--headline)
    (italic . ox-jupyter--italic)
    (item . ox-jupyter--item)
    (inner-template . ox-jupyter--inner-template)
    (line-break . ox-jupyter--line-break)
    (link . ox-jupyter--link)
    (paragraph . ox-jupyter--paragraph)
    (plain-list . ox-jupyter--plain-list)
    (section . ox-jupyter--section)
    (src-block . ox-jupyter--src-block)
    (strike-through . ox-jupyter--strike-through)
    (template . ox-jupyter--template)
    (underline . ox-jupyter--underline)
    (verbatim . ox-jupyter--verbatim))
  :menu-entry
  '(?j "Export to Jupyter Notebook"
       ((?J "As JSON buffer" ox-jupyter-export-as-json)
        (?j "As JSON file" ox-jupyter-export-to-json)
        (?o "As JSON file and open" ox-jupyter-export-to-json-and-open)))
  :filters-alist '((:filter-final-output ox-jupyter--no-comma-ending
                                         ox-jupyter--fixup-null-metadata
                                         ox-jupyter--fixup-empty-dict)
                   (:filter-headline . ox-jupyter--normalize-string)
                   (:filter-paragraph . ox-jupyter--normalize-string)
                   (:filter-parse-tree . ox-jupyter--merge-code-results)
                   (:filter-src-block . ox-jupyter--normalize-string))
  :options-alist '((:with-sub-superscript nil "^" nil)
                   (:jupyter-metadata "METADATA" nil
                                      org-export-jupyter-metadata space)))

;;; Constants

(defconst ox-jupyter--nbformat "4.0"
  "Jupyter Notebook version implemented in this library.")

(defconst ox-jupyter--source-line-max 60
  "The maximum length of a string in the source section of a cell.")

;; External Definitions

(autoload 'org-element-extract-element "org-element")

;;; User Options

(defgroup org-export-jupyter nil
  "Options for exporting Org mode files to Jupyter notebooks."
  :tag "Org Jupyter"
  :prefix "org-export-jupyter-"
  :group 'org-export)

(defcustom org-export-jupyter-major-mode 'js-mode
  "The major mode to apply to the *Org Jupyter Export* buffer."
  :type 'symbol
  :group 'org-export-jupyter)

(defcustom org-export-jupyter-metadata ""
  "Optional metadata to include in exported notebooks."
  :type 'string
  :group 'org-export-jupyter)

;;; Helper Functions

(defun ox-jupyter--no-comma-ending (string _backend _info)
  "Trim a trailing comma from STRING.

This is used as a post-processing function run on the final
results of transcoding."
  (if (string-match ",[ \n\t\r]*\\'" string)
      (replace-match "" t t string)
    string))

(defun ox-jupyter--no-newline-ending (string)
  "Trim trailing newlines from STRING."
  (if (string-match "\n+\\'" string)
      (replace-match "" t t string)
    string))

(defun ox-jupyter--normalize-string (string _backend _info)
  "Wrap `org-element-normalize-string' for use as a filter function on STRING.

This is used as a post-processing function run over the results
of cell-level transcoding."
  (org-element-normalize-string string))

(defun ox-jupyter--split-string (string &optional max-length)
  "Split STRING for Jupyter source sections.

Splits after newlines and after MAX-LENGTH characters from the
last split.

Default MAX-LENGTH is `ox-jupyter--source-line-max'."
  (let ((strlen (length string))
        (max-length (or max-length ox-jupyter--source-line-max))
        (start 0)
        end result)
    (while (string-match "\n" string start)
      (setq end (min (match-end 0) (+ start max-length)))
      (push (substring string start end) result)
      (setq start end))
    (while (not (= start strlen))
      (setq end (min strlen (+ start max-length)))
      (push (substring string start end) result)
      (setq start end))
    (nreverse result)))

(defun ox-jupyter--json-encode (object)
  "JSON encode OBJECT, and always pretty print."
  (let ((json-encoding-pretty-print t))
    (concat (json-encode object) ",")))

(defun ox-jupyter--markdown-alist (&rest lines)
  "Return a Jupyter Notebook markdown alist comprising LINES of source."
  `(("cell_type" . "markdown")
    ("metadata" . ,(make-hash-table))
    ("source" . ,lines)))

(defun ox-jupyter--code-alist (output-data lines)
  "Return a Jupyter Notebook code alist comprising OUTPUT-DATA and LINES of source code."
  `(("cell_type" . "code")
    ("execution_count")
    ("metadata" . ,(make-hash-table))
    ("outputs" . ,(or output-data (vector)))
    ("source" . ,lines)))

(defun ox-jupyter--template-alist
    (cell-list metadata major-version minor-version)
  "Return an alist representing the full contents of a Jupyter Notebook.

CELL-LIST is a list of cell data.  METADATA is any optional
metadata to include.  MAJOR-VERSION and MINOR-VERSION give the
notebook format version number."
  `(("cells" . ,cell-list)
    ("metadata" . ,(or metadata (make-hash-table)))
    ("nbformat" . ,major-version)
    ("nbformat_minor" . ,minor-version)))

(defun ox-jupyter--get-code-from (pom)
  "Return the code string from the source code block at POM."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char pom)
      (cadr (org-babel-get-src-block-info 'light)))))

(defun ox-jupyter--get-code-from-lob (name)
  "Return the code associated with NAME in `org-babel-library-of-babel'."
  (cl-caddr (assq (intern name) org-babel-library-of-babel)))

(defun ox-jupyter--display-data-alist (data size-pair)
  "Return an alist representing a Jupyter Notebook display_data.

DATA should be a list of base64 encoded image data and SIZE-PAIR
should be a cons of (width . height)."
  `(("output_type" . "display_data")
    ("data" ("image/png" . ,data))
    ("metadata" ("image/png"
                 ("width" . ,(car size-pair))
                 ("height" . ,(cdr size-pair))))))

(defun ox-jupyter--stdout-stream-alist (data)
  "Return an alist representing a Jupyter Notebook stdout stream.

DATA should be a list of strings."
  `(("name" . "stdout")
    ("output_type" . "stream")
    ("text" . ,data)))

(defun ox-jupyter--adopt-next-paragraph-maybe (info code-element)
  "Adopt a results paragraph (if any) under CODE-ELEMENT.

INFO is a plist of contextual parsing information."
  (let* ((next-element (org-export-get-next-element code-element info))
         (adoptable-p (member (org-element-type next-element)
                              '(paragraph example-block)))
         (is-results-p (org-element-property :results next-element)))
    (when (and adoptable-p is-results-p)
      (org-element-extract-element next-element)
      (org-element-adopt-elements code-element next-element))))

(defun ox-jupyter--merge-code-results (tree _backend info)
  "Combine source code elements of TREE with subsequent results elements.

BACKEND is required by the Org Export API but is not used here.
INFO is a plist of contextual parsing information."
  (org-element-map tree '(src-block babel-call)
    (apply-partially #'ox-jupyter--adopt-next-paragraph-maybe info) info)
  tree)

(defun ox-jupyter--fixup-null-metadata (string _backend _info)
  "Replace \"metadata: null\" with \"metadata: {}\" in STRING.

This is used as a post-processing function run on the final
results of transcoding."
  ;; As long as we are decoding and re-encoding JSON within the
  ;; recursive parser, we will turn empty dictionaries into nulls in the
  ;; final transcoded string.  Jupyter requires empty dictionaries, tho.
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward "\"metadata\": null" nil t)
      (replace-match "\"metadata\": {}"))
    (buffer-string)))

(defun ox-jupyter--fixup-empty-dict (string _backend _info)
  "Replace \"{\n}\" with \"{}\" in STRING.

This is used as a post-processing function run on the final
results of transcoding."
  ;; As long as we are using `json-encoding-pretty-print' then empty
  ;; dictionaries will be split across lines.  I think that's ugly.
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward "{[ \t\n]+}" nil t)
      (replace-match "{}"))
    (buffer-string)))

;;; Parsing Functions

(defun ox-jupyter--bold (_bold contents _info)
  "Transcode BOLD text to strongly emphasized Jupyter notebook JSON.

CONTENTS is the text to be emphasized.  INFO is a plist of
contextual information."
  (format "**%s**" contents))

(defun ox-jupyter--babel-call (babel-call contents _info)
  "Transcode a BABEL-CALL from Org to Jupyter notebook JSON.

CONTENTS is the concatenation of parsed subelements of the babel
call elements.  Normally babel call elements don't contain
subelements, but we merge any results under there with
`ox-jupyter--merge-code-results'.

INFO is a plist of contextual information."
  (let* ((name (org-element-property :call babel-call))
         (block-in-buffer-p (save-match-data
                              ;; returns a buffer position or nil
                              (org-babel-find-named-block name)))
         (code-string (if block-in-buffer-p
                          (ox-jupyter--get-code-from block-in-buffer-p)
                        (ox-jupyter--get-code-from-lob name)))
         (code-string (with-temp-buffer
                        (insert code-string)
                        (org-do-remove-indentation)
                        (buffer-string)))
         (code-string (ox-jupyter--no-newline-ending code-string))
         (code-text (ox-jupyter--split-string code-string))
         (output-data (and contents (json-read-from-string contents)))
         (code-alist (ox-jupyter--code-alist output-data code-text)))
    (ox-jupyter--json-encode code-alist)))

(defun ox-jupyter--code (code _contents _info)
  "Transcode CODE text to backticked Jupyter notebook JSON.

CONTENTS is nil for some reason.  INFO is a plist of contextual
information."
  (format "`%s`" (org-element-property :value code)))

(defun ox-jupyter--section-example-block (contents)
  "Transcode the CONTENTS of an example block."
  (ox-jupyter--section-paragraph contents))

(defun ox-jupyter--results-example-block (contents)
  "Transcode the CONTENTS of a code results example block."
  (ox-jupyter--json-encode
   (list (ox-jupyter--stdout-stream-alist
          (ox-jupyter--split-string contents)))))

(defun ox-jupyter--example-block (example-block _contents _info)
  "Transcode EXAMPLE-BLOCK object to Jupyter notebook JSON.

CONTENTS is the concatenation of parsed subelements of the
example block, which should always be nil.  INFO is a plist of
contextual information."
  (let ((parent-type (org-element-type
                      (org-element-property :parent example-block)))
        (example-contents (org-element-property :value example-block)))
    (cl-case parent-type
      ('section (ox-jupyter--section-example-block example-contents))
      ('src-block (ox-jupyter--results-example-block example-contents)))))

(defun ox-jupyter--section-fixed-width (contents)
  "Transcode the CONTENTS of an example block."
  (ox-jupyter--section-paragraph contents))

(defun ox-jupyter--results-fixed-width (contents)
  "Transcode the CONTENTS of a code results example block."
  (ox-jupyter--json-encode
   (list (ox-jupyter--stdout-stream-alist
          (ox-jupyter--split-string contents)))))

(defun ox-jupyter--fixed-width (fixed-width _contents _info)
  "Transcode FIXED-WIDTH object to Jupyter notebook JSON.

CONTENTS is the concatenation of parsed subelements of the
example block, which should always be nil.  INFO is a plist of
contextual information."
  (let ((parent-type (org-element-type
                      (org-element-property :parent fixed-width)))
        (example-contents (org-element-property :value fixed-width)))
    (cl-case parent-type
      ('section (ox-jupyter--section-fixed-width example-contents))
      ('src-block (ox-jupyter--results-fixed-width example-contents)))))

(defun ox-jupyter--footnote-reference (_footnote-reference contents _info)
  "Transcode FOOTNOTE-REFERENCE to Jupyter notebook JSON.

CONTENTS is the rest of the paragraph after the footnote.  INFO
is a plist of contextual information.

Currently just returns CONTENTS without processing
  FOOTNOTE-REFERENCE at all."
  contents)

(defun ox-jupyter--headline (headline contents info)
  "Transcode a HEADLINE element from Org to Jupyter notebook JSON.

CONTENTS is the concatenation of parsed subelements of the
headline.  INFO is a plist holding contextual information."
  (let* ((raw-value (org-element-property :raw-value headline))
         (level (org-export-get-relative-level headline info))
         (headline-text (with-temp-buffer
                          (insert (make-string level ?#))
                          (insert ? )
                          (insert raw-value)
                          (buffer-string)))
         (headline-alist (ox-jupyter--markdown-alist headline-text))
         (encoded-string (ox-jupyter--json-encode headline-alist)))
    (if contents
        (concat encoded-string "\n" contents)
      encoded-string)))

(defun ox-jupyter--italic (_italic contents _info)
  "Transcode ITALIC text to emphasized Jupyter notebook JSON.

CONTENTS is the text to be emphasized.  INFO is a plist of
contextual information."
  (format "_%s_" contents))

(defun ox-jupyter--item (item contents _info)
  "Transcode a list ITEM element from Org to Jupyter notebook JSON.

CONTENTS is the concatenation of parsed subelements of the
item.  INFO is a plist of contextual information."
  (let ((tag (car (org-element-property :tag item)))
        (tag-sep (if (string-match-p "\\` +-" contents) "\n" ": ")))
    (concat (org-element-property :bullet item)
            (and tag (format "**%s**" tag))
            (and tag tag-sep)
            contents)))

(defun ox-jupyter--inner-template (contents _info)
  "Wrap CONTENTS in square brackets."
  (format "[%s]" (ox-jupyter--no-comma-ending contents nil nil)))

(defun ox-jupyter--link-contents (path)
  "Encode an image at PATH to base64."
  (let* ((encoded-data (with-temp-buffer
                         (insert-file-contents-literally path)
                         (base64-encode-region (point-min) (point-max))
                         (split-string (buffer-string) "\n")))
         (image-size (image-size (create-image path) t))
         (alist (ox-jupyter--display-data-alist encoded-data image-size)))
    (ox-jupyter--json-encode alist)))

(defun ox-jupyter--image-link (link contents)
  "Do something with LINK and CONTENTS."
  (let* ((link-grandparent (org-element-property
                            :parent (org-element-property
                                     :parent link)))
         (link-in-paragraph-p (eq (org-element-type link-grandparent)
                                  'section))
         (link-path (org-element-property :path link)))
    (if link-in-paragraph-p
        (format "![%s](%s)" (or contents "") link-path)
      (ox-jupyter--link-contents link-path))))

(defun ox-jupyter--default-link (path contents)
  "Transcode a file at PATH to Jupyter notebook JSON.

CONTENTS is the description part of the link, or nil."
  (if contents
      (format "[%s](%s)" contents path)
    (format "[%s](%s)" path path)))

(defun ox-jupyter--file-link (link contents)
  "Transcode a file LINK element from Org to Jupyter notebook JSON.

CONTENTS is the description part of the link, or nil."
  (let ((link-path (org-element-property :path link)))
    (if (string-match-p "\\.png\\'" link-path)
        (ox-jupyter--image-link link contents)
      (ox-jupyter--default-link link-path contents))))

(defun ox-jupyter--fuzzy-link (_link contents)
  "Transcode a fuzzy LINK element from Org to Jupyter notebook JSON.

CONTENTS is the description part of the link, or nil.  INFO is a
plist of contextual information."
  contents)

(defun ox-jupyter--web-link (link contents)
  "Transcode a web LINK element from Org to Jupyter notebook JSON.

CONTENTS is the description part of the link, or nil."
  (let ((raw-link (org-element-property :raw-link link)))
    (if contents
        (format "[%s](%s)" contents raw-link)
      raw-link)))

(defun ox-jupyter--line-break (_line-break _contents _info)
  "Transcode a LINE-BREAK element from Org to Jupyter notebook JSON.

CONTENTS is always null, but a required part of the Org Export
API.  INFO is a plist of contextual information."
  "  \n")

(defun ox-jupyter--link (link contents _info)
  "Transcode a LINK element from Org to Jupyter notebook JSON.

CONTENTS is the description part of the link, or nil.  INFO is a
plist of contextual information."
  (cl-case (intern (org-element-property :type link))
    ((http https mailto) (ox-jupyter--web-link link contents))
    (file (ox-jupyter--file-link link contents))
    (fuzzy (ox-jupyter--fuzzy-link link contents))
    (t (ox-jupyter--fuzzy-link link contents))))

(defun ox-jupyter--section-paragraph (contents)
  "Transcode the CONTENTS of a section paragraph."
  (let* ((contents (ox-jupyter--no-newline-ending contents))
         (markdown-text (ox-jupyter--split-string contents))
         (markdown-alist (apply #'ox-jupyter--markdown-alist
                                markdown-text)))
    (ox-jupyter--json-encode markdown-alist)))

(defun ox-jupyter--list-item-paragraph (contents)
  "Transcode the CONTENTS of a list item paragraph."
  contents)

(defun ox-jupyter--results-paragraph (contents)
  "Transcode the CONTENTS of a code results paragraph."
  (ox-jupyter--json-encode
   (list (json-read-from-string contents))))

(defun ox-jupyter--paragraph (paragraph contents _info)
  "Transcode a PARAGRAPH element from Org to Jupyter notebook JSON.

CONTENTS is the concatenation of parsed subelements of the
paragraph.  INFO is a plist of contextual information."
  (let ((parent-type (org-element-type
                      (org-element-property :parent paragraph))))
    (cl-case parent-type
      ('section (ox-jupyter--section-paragraph contents))
      ('item (ox-jupyter--list-item-paragraph contents))
      ('src-block (ox-jupyter--results-paragraph contents)))))

(defun ox-jupyter--top-level-plain-list (plain-list)
  "Transcode a PLAIN-LIST to Jupyter notebook JSON.

PLAIN-LIST is the concatenation of parsed subelements of the list."
  (let* ((contents (ox-jupyter--no-newline-ending plain-list))
         (markdown-text (ox-jupyter--split-string contents))
         (markdown-alist (apply #'ox-jupyter--markdown-alist
                                markdown-text)))
    (ox-jupyter--json-encode markdown-alist)))

(defun ox-jupyter--intermediate-plain-list (plain-list)
  "Indent PLAIN-LIST by two spaces."
  (with-temp-buffer
    (insert plain-list)
    (goto-char (point-min))
    (org-indent-item-tree)
    (org-indent-item-tree)
    (buffer-string)))

(defun ox-jupyter--plain-list (plain-list contents _info)
  "Transcode a PLAIN-LIST element from Org to Jupyter notebook JSON.

CONTENTS is the concatenation of parsed subelements of the list.
INFO is a plist of contextual information."
  (cl-case (org-element-type (org-element-property :parent plain-list))
    (item (ox-jupyter--intermediate-plain-list contents))
    (t (ox-jupyter--top-level-plain-list contents))))

(defun ox-jupyter--section (_section contents _info)
  "Transcode a SECTION element from Org to Jupyter notebook JSON.

CONTENTS is the concatenation of parsed subelements of the
section.  INFO is a plist of contextual information."
  contents)

(defun ox-jupyter--src-block (src-block contents _info)
  "Transcode a SRC-BLOCK element from Org to Jupyter notebook JSON.

CONTENTS is the concatenation of parsed subelements of the source
block.  Normally source blocks don't contain subelements, but we
merge any results under there with
`ox-jupyter--merge-code-results'.

INFO is a plist of contextual information."
  (let* ((code-string (org-element-property :value src-block))
         (preserve-indent-p
          (org-element-property :preserve-indent src-block))
         (code-string (if preserve-indent-p
                          code-string
                        (with-temp-buffer
                          (insert code-string)
                          (org-do-remove-indentation)
                          (buffer-string))))
         (code-string (ox-jupyter--no-newline-ending code-string))
         (code-text (ox-jupyter--split-string code-string))
         (output-data (and contents (json-read-from-string contents)))
         (code-alist (ox-jupyter--code-alist output-data code-text)))
    (ox-jupyter--json-encode code-alist)))

(defun ox-jupyter--strike-through (_strike-through contents _info)
  "Transcode STRIKE-THROUGH text to emphasized Jupyter notebook JSON.

CONTENTS is the text to be emphasized.  INFO is a plist of
contextual information."
  (format "~~%s~~" contents))

(defun ox-jupyter--template (contents info)
  "Add preamble and postamble to transcoded document CONTENTS.

INFO is a plist of export options."
  (let* ((cell-list (json-read-from-string contents))
         (metadata (ignore-errors
                     (read (plist-get info :jupyter-metadata))))
         (version-pair (split-string ox-jupyter--nbformat "\\."))
         (major-version (string-to-number (car version-pair)))
         (minor-version (string-to-number (cadr version-pair)))
         (alist (ox-jupyter--template-alist cell-list metadata
                                            major-version minor-version)))
    (ox-jupyter--json-encode alist)))

(defun ox-jupyter--underline (_underline contents _info)
  "Transcode UNDERLINE text to emphasized Jupyter notebook JSON.

CONTENTS is the text to be emphasized.  INFO is a plist of
contextual information."
  (format "_%s_" contents))

(defun ox-jupyter--verbatim (verbatim _contents _info)
  "Transcode VERBATIM text to backticked Jupyter notebook JSON.

CONTENTS is nil for some reason.  INFO is a plist of contextual
information."
  (format "`%s`" (org-element-property :value verbatim)))

;;; End-user functions

;;;###autoload
(defun ox-jupyter-export-as-json
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a Jupyter notebook JSON buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export just that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Postional argument BODY-ONLY is required by the Org Export API,
however it is ignored in this function.

EXT-PLIST, when provided, must be a property list with external
parameters overriding Org default settings.  This argument will
still be overriden by file-local settings.

Export is done in a buffer named \"*Org Jupyter Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'jupyter "*Org Jupyter Export*"
    async subtreep visible-only body-only ext-plist
    (symbol-function org-export-jupyter-major-mode)))

;;;###autoload
(defun ox-jupyter-export-to-json
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a Jupyter notebook JSON file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export just that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Postional argument BODY-ONLY is required by the Org Export API,
however it is ignored in this function.

EXT-PLIST, when provided, must be a property list with external
parameters overriding Org default settings.  This argument will
still be overriden by file-local settings.

Return output file's name."
  (interactive)
  (let ((file (org-export-output-file-name ".ipynb" subtreep)))
    (org-export-to-file 'jupyter file
      async subtreep visible-only body-only ext-plist)))

(defun ox-jupyter-export-to-json-and-open
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a Jupyter notebook file and call `org-open-file'.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export just that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Postional argument BODY-ONLY is required by the Org Export API,
however it is ignored in this function.

EXT-PLIST, when provided, must be a property list with external
parameters overriding Org default settings.  This argument will
still be overriden by file-local settings.

Return output file's name."
  (let ((file-name (ox-jupyter-export-to-json async subtreep
                                              visible-only body-only
                                              ext-plist)))
    (if async file-name (org-open-file file-name))))

(provide 'ox-jupyter)
;;; ox-jupyter.el ends here
