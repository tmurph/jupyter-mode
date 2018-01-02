;;; ob-jupyter.el --- org-babel functions for Jupyter frontend  -*- lexical-binding: t; -*-

;; Author: Trevor Murphy <trevor.m.murphy@gmail.com>
;; Maintainer: Trevor Murphy <trevor.m.murphy@gmail.com>
;; Version: 0.1.0
;; Keywords: literate programming, reproducible research
;; URL: https://github.com/tmurph/ob-jupyter

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

;; Org-Babel support for working with Jupyter servers.  This library
;; aims to make Emacs a full-fledged Jupyter client.

;; Much of the ZMQ FFI code has been copied without changes from John
;; Kitchin's work here:
;; http://kitchingroup.cheme.cmu.edu/blog/2017/07/13/An-Emacs-zeromq-library-using-an-ffi/
;; Used under a CC BY-AS 4.0 license.

;; The rest is very much inspired by (though not copied from) Greg
;; Sexton's ob-ipython.el here:
;; https://github.com/gregsexton/ob-ipython

;;; Code:

(provide 'ob-jupyter)
;;; ob-jupyter.el ends here
