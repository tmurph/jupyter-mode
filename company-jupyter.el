;;; company-jupyter.el --- company completion from Jupyter kernels

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

;; This library supports completion with Company.
;;
;; Enable with
;;   (add-to-list 'company-backends 'company-jupyter)
;;
;; Completion will only work in a buffer when Jupyter minor mode is
;; active and the buffer has an associated inferior kernel process.

;;; Code:

(require 'company)
(require 'jupyter)

(defun jupyter--company-prefix-sync (kernel pos code)
  "Query KERNEL for the completion prefix at POS in CODE."
  ;; this could easily return a deferred object for use with company async
  ;; however, company does not support async prefix commands
  (deferred:sync!
    (deferred:$
      (jupyter--complete-deferred kernel pos code)
      (deferred:nextc it #'jupyter--cursor-pos)
      (deferred:nextc it
        (lambda (cursor-cons)
          (substring-no-properties
           code (car cursor-cons) (cdr cursor-cons)))))))

(defvar jupyter--company-prefix-cache ""
  "The most recent prefix returned by `jupyter--company-prefix-sync'.")

(defun jupyter--company-prefix (kernel pos code)
  "Return the prefix for company completion, or nil for no completion.

First check the result of `company-grab-symbol-cons' against
`jupyter--company-prefix-cache'.  If that check fails, fall back
to `jupyter--company-prefix-sync' on KERNEL with POS and CODE."
  (let* ((prefix-re (replace-regexp-in-string
                     "\\." "\\." ; literal replace, lol
                     jupyter--company-prefix-cache nil t))
         (symbol-or-cons (company-grab-symbol-cons
                          prefix-re
                          (length jupyter--company-prefix-cache)))
         (prefix-matched (consp symbol-or-cons))
         (trivial-prefix (string= jupyter--company-prefix-cache ""))
         result)
    (if (and (not trivial-prefix) prefix-matched)
        (setq result (concat jupyter--company-prefix-cache
                             (car symbol-or-cons)))
      (setq result (jupyter--company-prefix-sync kernel pos code)
            jupyter--company-prefix-cache
            (replace-regexp-in-string "[^.]*\\'" "" result)))
    result))

(defun jupyter--company-candidates-async (kernel pos code callback)
  "Query KERNEL for completion candidates at POS in CODE and pass the results to CALLBACK."
  (deferred:$
    (jupyter--complete-deferred kernel pos code 1000)
    (deferred:nextc it #'jupyter--matches)
    (deferred:nextc it callback)))

(defun jupyter--company-candidates-sync (kernel pos code)
  "Query KERNEL for completion candidates at POS in CODE."
  (deferred:sync!
    (jupyter--company-candidates-async
     kernel pos code (lambda (x) x))))

(defun jupyter--company-doc-buffer-sync (kernel pos code)
  "Query KERNEL for documentation at POS in CODE and return a doc buffer."
  ;; this could easily return a deferred object for use with company async
  ;; however, company does not support async doc buffer commands
  (deferred:sync!
    (deferred:$
      (jupyter--inspect-deferred kernel pos code 1000)
      (deferred:nextc it #'jupyter--inspect-text)
      (deferred:nextc it #'company-doc-buffer)
      (deferred:nextc it
        (lambda (buf)
          (with-current-buffer buf
            (ansi-color-apply-on-region (point-min) (point-max))
            (current-buffer)))))))

(defun company-jupyter (command &optional arg &rest ignored)
  "Provide completion info according to COMMAND and ARG.

IGNORED is not used."
  (interactive (list 'interactive))
  (let ((kernel jupyter--current-kernel)
        (pos (1- (point)))
        (code (buffer-substring-no-properties (point-min) (point))))
    (cl-case command
      (interactive (company-begin-backend 'company-jupyter))
      (prefix (and jupyter-mode kernel
                   (not (company-in-string-or-comment))
                   (consp (company-grab-symbol-cons "\\."))
                   (jupyter--company-prefix kernel pos code)))
      (candidates (if company--manual-action
                      (jupyter--company-candidates-sync kernel pos code)
                    (cons
                     :async
                     (apply-partially #'jupyter--company-candidates-async
                                      kernel pos code))))
      (sorted t)
      (doc-buffer (jupyter--company-doc-buffer-sync
                   kernel (length arg) arg)))))

(provide 'company-jupyter)
;;; company-jupyter.el ends here
