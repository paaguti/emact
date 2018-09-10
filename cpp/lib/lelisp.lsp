;;;; -*-Mode:LISP; Package:LISP; Base:10; Syntax:ISLISP -*-
;;;; Date:	2009/05/02
;;;; Title:	lelisp.lsp
;;;; Author:	C. Jullien

;;; This  program  is  free  software;  you can redistribute it and/or
;;; modify  it  under  the  terms of the GNU General Public License as
;;; published  by  the  Free  Software Foundation; either version 2 of
;;; the License, or (at your option) any later version.
;;; 
;;; This  program  is  distributed in the hope that it will be useful,
;;; but  WITHOUT ANY WARRANTY;  without  even the implied  warranty of
;;; MERCHANTABILITY  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;; 
;;; You  should have received a copy of the GNU General Public License
;;; along  with  this  program;  if  not,  write  to the Free Software
;;; Foundation,  Inc.,  59  Temple  Place  -  Suite  330,  Boston,  MA
;;; 02111-1307, USA.

;;; Emacs standard macros for Le-Lisp.

(in-package "emacs")

;;; Standard package

(defun lelisp-std-package ()
   (beginning-of-buffer)
   (insert-string ";;;; .EnTete	\"Le-Lisp version 15.26\" \" \" \"")
   (insert-buffer-name)
   (insert-string "\"\n")
   (insert-string ";;;; .Date	\"0000/00/00\"\n")
   (insert-string ";;;; .EnPied	\"")
   (insert-buffer-name)
   (insert-string "\"\n")
   (insert-string ";;;; .Version	\"1.00\"\n")
   (insert-string ";;;; .Auteur 	\"")
   (insert-user-name)
   (insert-string "\"\n")
   (insert-string ";;;; .Annexe I	\"\"\n")
   (newline)
   (insert-string ";;; Ve'rification de la version.\n")
   (newline)
   (insert-string "(unless (>= (version) 15.2)\n")
   (insert-string "     (error 'load 'erricf '")
   (insert-buffer-name)
   (repeat 3 (delete-backward-char))
   (insert-string "))\n")
   (newline)
   (insert-string ";;; Tous les symboles pre'ce'de's de ':' seront")
   (insert-string " de'finis dans le package ")
   (insert-buffer-name)
   (repeat 3 (delete-backward-char))
   (backward-word)
   (upcase-word)
   (insert-string ".\n")
   (newline)
   (insert-string "(defvar #:sys-package:colon '")
   (insert-buffer-name)
   (repeat 3 (delete-backward-char))
   (insert-string ")\n")
   (newline))

;;; Standard module

(defun lelisp-std-module ()
   (beginning-of-buffer)
   (insert-string "defmodule\n")
   (insert-string "	;; module name\n")
   (insert-string "	")
   (insert-buffer-name)
   (repeat 3 (delete-backward-char))
   (newline)
   (insert-string "files\n")
   (insert-string "	;; list of module files\n")
   (insert-string "	(")
   (insert-buffer-name)
   (repeat 3 (delete-backward-char))
   (insert-string ")\n")
   (insert-string "import\n")
   (insert-string "	;; imported modules\n")
   (insert-string "	()\n")
   (insert-string "export\n")
   (insert-string "	;; exported functions\n")
   (insert-string "	()\n")
   (newline)
   (insert-string ";;; Added automatically, don't type beyond this line.\n")
   (newline)
   (beginning-of-buffer))

(progn
	(bind-to-key 'lelisp-std-package "C-CP")
	(bind-to-key 'lelisp-std-module  "C-CM"))
