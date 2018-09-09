;;;; -*-Mode:LISP; Package:LISP; Base:10; Syntax:ISLISP -*-
;;;; Date:	2009/05/02
;;;; Title:	c.lsp
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

;;; Emacs standard macros for C control structures.

(in-package "emacs")

;; Comment.

(defun c-comment ()
   (end-of-line)
   (insert-string "/*\\n")
   (insert-string " *	\\n")
   (delete-backward-char)
   (insert-string " */\\n")
   (newline)
   (repeat 3 (previous-line))
   (end-of-line))

;; 'do' control structure.

(defun c-do ()
   (end-of-line)
   (insert-string "do	{")
   (repeat 2 (newline))
   (delete-backward-char)
   (insert-string "} while(  );")
   (previous-line)
   (indent-for-tab-command))

;; 'for' control structure.

(defun c-for ()
   (end-of-line)
   (insert-string "for(  ) {")
   (repeat 2 (newline))
   (delete-backward-char)
   (insert-string "}")
   (previous-line)
   (indent-for-tab-command)
   (previous-line)
   (end-of-line)
   (repeat 4 (backward-char)))

;; 'if' control structure.

(defun c-if ()
   (end-of-line)
   (insert-string "if(  )")
   (newline)
   (previous-line)
   (end-of-line)
   (repeat 2 (backward-char)))

;; 'C-Package' skelton

(defun c-package ()
   (fundamental-mode)
   (end-of-line)
   (beginning-of-buffer)
   (insert-string "#if	!defined( lint )\\n")
   (insert-string "static	char *sccsid = \"@(#)")
   (insert-buffer-name)
   (insert-string "	(c) ")
   (insert-user-name)
   (insert-string " 0000/00/00\";\\n")
   (insert-string "#endif\\n")
   (newline)
   (insert-string "/*\\n")
   (insert-string " *	")
   (insert-buffer-name)
   (insert-string " :	\\n")
   (insert-string " */\\n")
   (newline)
   (insert-string "#include <stdio.h>\\n")
   (newline)
   (insert-string "int\\n")
   (insert-buffer-name)
   (repeat 2 (delete-backward-char))
   (insert-string "()\\n")
   (insert-string "{\\n")
   (insert-string "	return( 0 );\\n")
   (insert-string "}\\n")
   (repeat 2 (previous-line))
   (end-of-line)
   (set-mark-command)
   (beginning-of-buffer)
   (update-screen)
   (exchange-point-and-mark)
   (c-mode)
   (write-minibuffer ""))

;; Standard switch reading inside 'main' procedure.

(defun c-standard-argument ()
   (end-of-line)
   (insert-string "while( *argv && *(*++argv) == '-' )\\n")
   (insert-string "while( *++(*argv) )\\n")
   (insert-string "switch( **argv ) {\\n")
   (delete-backward-char)
   (insert-string "case '' :\\n")
   (insert-string "break;\\n")
   (delete-backward-char)
   (insert-string "default :\\n")
   (delete-backward-char)
   (insert-string "}\\n")
   (repeat 4 (previous-line))
   (end-of-line)
   (repeat 3 (backward-char)))

;; Standard 'main' prolog.

(defun c-std-main ()
   (fundamental-mode)
   (beginning-of-buffer)
   (insert-string "#if	!defined( lint )\\n")
   (insert-string "static	char *sccsid = \"@(#)")
   (insert-buffer-name)
   (insert-string "	(c) ")
   (insert-user-name)
   (insert-string " 0000/00/00\";\\n")
   (insert-string "#endif\\n")
   (newline)
   (insert-string "/*\\n")
   (insert-string " *	")
   (insert-buffer-name)
   (insert-string " :	\\n")
   (insert-string " */\\n")
   (newline)
   (insert-string "#include <stdio.h>\\n")
   (newline)
   (insert-string "int\\n")
   (insert-string "main(int argc, char *argv[])\\n")
   (insert-string "{\\n")
   (insert-string "	\\n")
   (insert-string "}\\n")
   (repeat 2 (previous-line))
   (end-of-line)
   (set-mark-command)
   (beginning-of-buffer)
   (update-screen)
   (exchange-point-and-mark)
   (c-mode)
   (write-minibuffer ""))

;; 'switch' control structure.

(defun c-switch ()
   (end-of-line)
   (insert-string "switch(  ) {\\n")
   (insert-string "case :\\n")
   (insert-string "break;\\n")
   (delete-backward-char)
   (insert-string "default :\\n")
   (delete-backward-char)
   (insert-string "}")
   (repeat 4 (previous-line))
   (end-of-line)
   (repeat 4 (backward-char)))

;; 'while' control structure.

(defun c-while ()
   (end-of-line)
   (insert-string "while(  ) {\\n")
   (newline)
   (delete-backward-char)
   (insert-string "}")
   (previous-line)
   (indent-for-tab-command)
   (previous-line)
   (end-of-line)
   (repeat 4 (backward-char)))

(progn
        (c-mode)
	(bind-to-key 'c-comment		  "C-C/")
	(bind-to-key 'c-do		  "C-CD")
	(bind-to-key 'c-for		  "C-CF")
	(bind-to-key 'c-if		  "C-CI")
	(bind-to-key 'c-package		  "C-CP")
	(bind-to-key 'c-standard-argument "C-CA")
	(bind-to-key 'c-std-main	  "C-CM")
	(bind-to-key 'c-switch		  "C-CS")
	(bind-to-key 'c-while		  "C-CW"))
