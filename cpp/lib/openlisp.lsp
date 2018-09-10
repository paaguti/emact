;;;; -*-Mode:LISP; Package:LISP; Base:10; Syntax:ISLISP -*-
;;;; Date:	2006/09/16
;;;; Title:	openlisp.lsp
;;;; Author:	C. Jullien
;;;; RCS:       "$Id: openlisp.lsp,v 1.1 2018/09/04 17:51:14 jullien Exp $"

;;; Standard package

(in-package "emacs")

(defun openlisp-std-package ()
   (fundamental-mode)
   (beginning-of-buffer)
   (insert-string ";;;; -*-Mode:LISP; Package:LISP; Base:10; Syntax:ISLISP -*-\\n")
   (insert-string ";;;; Date:   0000/00/00\\n")
   (insert-string ";;;; Title:  ")
   (insert-buffer-name)
   (newline)
   (insert-string ";;;; Author: ")
   (insert-user-name)
   (newline)
   (insert-string ";;;; RCS:    $")
   (insert-string "Id")
   (insert-string "$\\n\\n")
   (insert-string "(require 'setf)\\n")
   (insert-string "(require 'defstruc)\\n")
   (newline)
   (insert-package)
   (insert-string "(provide '")
   (insert-base-name)
   (insert-string ")")
   (beginning-of-buffer)
   (lisp-mode))

(defun insert-package ()
   (fundamental-mode)
   (insert-string "(defpackage \"")
   (insert-base-name)
   (insert-string "\"\\n")
   (insert-string "   (:use \"openlisp\"))\\n\\n")
   (lisp-mode))

(defun openlisp-defstruct ()
   (fundamental-mode)
   (insert-string "(defstruct (<")
   (insert-base-name)
   (insert-string ">\\n")
   (insert-string "              (:print-function ")
   (insert-base-name)
   (insert-string "-printer))\\n")
   (insert-string "           (field1 nil))\\n")
   (newline)
   (insert-string "(defun ")
   (insert-base-name)
   (insert-string "-printer (obj st f)\\n")
   (insert-string "   (identity f)\\n")
   (insert-string "   (format st \"#<")
   (insert-base-name)
   (insert-string ": ~s>\" (")
   (insert-base-name)
   (insert-string "-field1 obj)))\\n")
   (lisp-mode))

(progn
   (bind-to-key 'openlisp-std-package "C-CP")
   (bind-to-key 'openlisp-defstruct   "C-CS"))
