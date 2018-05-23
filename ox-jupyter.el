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
;; Enable by customizing `org-export-backends' before loading Org, or
;; with (require 'ox-jupyter).
;;
;; The library will take care of setting up Org Source buffers with the
;; appropriate kernel language.

;;; Code:

(require 'ox)
(require 'jupyter)

;;; Define Back-end

(org-export-define-backend 'jupyter
  '((headline . ox-jupyter--headline)
    (paragraph . ox-jupyter--paragraph)
    (src-block . ox-jupyter--src-block))
  :menu-entry
  '(?j "Export to Jupyter Notebook"
       ((?J "As JSON buffer" ox-jupyter-export-as-json)
        (?j "As JSON file" ox-jupyter-export-to-json))))

;;; User Options

(defgroup org-export-jupyter nil
  "Options for exporting Org mode files to Jupyter notebooks."
  :tag "Org Jupyter"
  :group 'org-export)

;;; Helper Functions

(defun ox-jupyter--json-encode-alist (alist)
  "JSON encode ALIST, and always pretty print."
  (let ((json-encoding-pretty-print t))
    (json-encode-alist alist)))

(defun ox-jupyter--markdown-alist (&rest lines)
  "Return a Jupyter Notebook markdown alist comprising LINES of source."
  `(("cell_type" . "markdown")
    ("metadata" . ,(make-hash-table))
    ("source" . ,lines)))

(defun ox-jupyter--code-alist (&rest lines)
  "Return a Jupyter Notebook code alist comprising LINES of source."
  `(("cell_type" . "code")
    ("execution_count")
    ("metadata" . ,(make-hash-table))
    ("outputs" . ,(vector))
    ("source" . ,lines)))

;;; Headline

(defun ox-jupyter--headline (headline _contents _info)
  "Transcode a HEADLINE element from Org to Jupyter notebook JSON.

CONTENTS is the concatenation of parsed subelements of the
headline.  INFO is a plist holding contextual information."
  (let* ((raw-value (org-element-property :raw-value headline))
         (level (org-element-property :level headline))
         (headline-text (with-temp-buffer
                          (dotimes (_ level)
                            (insert ?#))
                          (insert ? )
                          (insert raw-value)
                          (buffer-string)))
         (headline-alist (ox-jupyter--markdown-alist headline-text)))
    (ox-jupyter--json-encode-alist headline-alist)))

;;; Paragraph

(defun ox-jupyter--paragraph (_paragraph contents _info)
  "Transcode a PARAGRAPH element from Org to Jupyter notebook JSON.

CONTENTS is the concatenation of parsed subelements of the
paragraph.  INFO is a plist of contextual information."
  (let* ((markdown-text (split-string (string-trim-right contents) "\n"))
         (markdown-alist (apply #'ox-jupyter--markdown-alist
                                markdown-text)))
    (ox-jupyter--json-encode-alist markdown-alist)))

;;; Src Block

(defun ox-jupyter--src-block (src-block _contents _info)
  "Transcode a SRC-BLOCK element from Org to Jupyter notebook JSON.

CONTENTS is the contents of the src-block.  INFO is a plist of
contextual information."
  (let* ((code-value (org-element-property :value src-block))
         (preserve-indent-p
          (org-element-property :preserve-indent src-block))
         (code-value (if preserve-indent-p
                         code-value
                       (with-temp-buffer
                         (insert code-value)
                         (org-do-remove-indentation)
                         (buffer-string))))
         (code-text (split-string (string-trim-right code-value) "\n"))
         (code-alist (apply #'ox-jupyter--code-alist code-text)))
    (ox-jupyter--json-encode-alist code-alist)))

;;; End-user functions

;;;###autoload
(defun ox-jupyter-export-as-json
    (&optional async subtreep visible-only _body-only ext-plist)
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
  (ignore async subtreep visible-only ext-plist))

;;;###autoload
(defun ox-jupyter-export-to-json
    (&optional async subtreep visible-only _body-only ext-plist)
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
  (ignore async subtreep visible-only ext-plist))

(provide 'ox-jupyter)
;;; ox-jupyter.el ends here
