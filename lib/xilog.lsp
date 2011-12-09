;;;; -*-Mode:LISP; Package:LISP; Base:10; Syntax:ISLISP -*-
;;;; Date:	2009/05/02
;;;; Title:	xilog.lsp
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

;;; Emacs standard macros for XILOG control structures.

(in-package "emacs")

;; Standard 'main' prolog.

(defun xilog-std-main ()
   (beginning-of-buffer)
   (insert-string "/*")
   (newline)
   (insert-string " *	Module: ")
   (insert-buffer-name)
   (insert-string " (c) ")
   (insert-user-name)
   (insert-string " 0000/00/00\"")
   (newline)
   (insert-string "*/")
   (newline)
   (delete-backward-char)
   (newline)
   (insert-string "/*	XILOG Version V2.00	*/")
   (newline)
   (newline))

(progn
	(bind-to-key 'xilog-std-main "C-CM"))
