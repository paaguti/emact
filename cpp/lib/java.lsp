;;;; -*-Mode:LISP; Package:LISP; Base:10; Syntax:ISLISP -*-
;;;; Date:	2009/05/02
;;;; Title:	java.lsp
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

;;; Emacs standard macros for Java control structures.

(in-package "emacs")

;;; Comment.

(defun java-comment ()
   (end-of-line)
   (insert-string "//\\n")
   (insert-string "//	\\n")
   (insert-string "//\\n\\n")
   (repeat 3 (previous-line))
   (end-of-line))

;;; 'do' control structure.

(defun java-do ()
   (end-of-line)
   (insert-string "do	{\\n")
   (newline)
   (delete-backward-char)
   (insert-string "} while(  );")
   (previous-line)
   (indent-for-tab-command))

;;; 'for' control structure.

(defun java-for ()
   (end-of-line)
   (insert-string "for(  ) {\\n")
   (newline)
   (delete-backward-char)
   (insert-string "}")
   (previous-line)
   (indent-for-tab-command)
   (previous-line)
   (end-of-line)
   (repeat 4 (backward-char)))

;;; 'if' control structure.

(defun java-if ()
   (end-of-line)
   (insert-string "if(  )\\n")
   (previous-line)
   (end-of-line)
   (repeat 2 (backward-char)))

;;; 'Java package' skelton

(defun java-package ()
   (fundamental-mode)
   (end-of-line)
   (beginning-of-buffer)
   (insert-string "//\\n")
   (insert-string "// @(#)")
   (insert-buffer-name)
   (insert-string "	(c) ")
   (insert-user-name)
   (insert-string " 0000/00/00\";\\n")
   (insert-string "//\\n")
   (newline)
   (insert-string "//\\n")
   (insert-string "//	")
   (insert-buffer-name)
   (insert-string " : \\n")
   (insert-string "//\\n")
   (newline)
   (insert-string "import java.io.*;\\n")
   (insert-string "import java.util.*;\\n")
   (newline)
   (insert-string "/**\\n")
   (insert-string " * class ")
   (insert-base-name)
   (newline)
   (insert-string " *\\n")
   (insert-string " * @author	")
   (insert-user-name)
   (newline)
   (insert-string " * @version	1.00\\n")
   (insert-string " */\\n")
   (newline)
   (insert-string "public class ")
   (insert-base-name)
   (insert-string " extends Object {\\n")
   (newline)
   (insert-string "  public static void main(final String argv[]) {\\n")
   (insert-string "    System.out.println(\"main\");\\n")
   (insert-string "    	System.out.println(\"Press enter\");\\n")
   (insert-string "     try {\\n")
   (insert-string "       System.in.read();\\n")
   (insert-string "     } catch (IOException e) {\\n")
   (insert-string "       return;\\n")
   (insert-string "     }\\n")
   (insert-string "}\\n")
   (newline)
   (insert-string "  // constructor\\n")
   (newline)
   (insert-string "  public ")
   (insert-base-name)
   (insert-string "() {\\n")
   (insert-string "  }\\n")
   (newline)
   (insert-string "}\\n")
   (newline)
   (java-mode)
   (beginning-of-buffer)
   (update-screen)
   (write-minibuffer ""))

;;; 'switch' control structure.

(defun java-switch ()
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

;;; 'while' control structure.

(defun java-while ()
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
        (java-mode)
	(bind-to-key 'java-comment	     "C-C/")
	(bind-to-key 'java-do		     "C-CD")
	(bind-to-key 'java-for		     "C-CF")
	(bind-to-key 'java-if		     "C-CI")
	(bind-to-key 'java-package	     "C-CP")
	(bind-to-key 'java-switch	     "C-CS")
	(bind-to-key 'java-while	     "C-CW"))
