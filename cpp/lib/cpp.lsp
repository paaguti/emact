;;;; -*-Mode:LISP; Package:LISP; Base:10; Syntax:ISLISP -*-
;;;; Date:	2009/05/02
;;;; Title:	cpp.lsp
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

;;; Emacs standard macros for C++ control structures.

(in-package "emacs")

;;; Comment.

(defun cpp-comment ()
   (end-of-line)
   (insert-string "//")
   (newline)
   (insert-string "//	")
   (newline)
   (insert-string "//")
   (repeat 2 (newline))
   (repeat 3 (previous-line))
   (end-of-line))

;;; 'do' control structure.

(defun cpp-do ()
   (end-of-line)
   (insert-string "do	{")
   (repeat 2 (newline))
   (delete-backward-char)
   (insert-string "} while(  );")
   (previous-line)
   (indent-for-tab-command))

;;; 'for' control structure.

(defun cpp-for ()
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

;;; 'if' control structure.

(defun cpp-if ()
   (end-of-line)
   (insert-string "if(  )\\n")
   (previous-line)
   (end-of-line)
   (repeat 2 (backward-char)))

;;; 'C++ package' skelton

(defun cpp-package ()
   (fundamental-mode)
   (beginning-of-buffer)
   (insert-string "#if	!defined( lint )\\n")
   (insert-string "static	const char *sccsid = \"@(#)")
   (insert-buffer-name)
   (insert-string "	(c) ")
   (insert-user-name)
   (insert-string " 0000/00/00\";\\n")
   (insert-string "#endif\\n")
   (newline)
   (insert-string "//\\n")
   (insert-string "//	")
   (insert-buffer-name)
   (insert-string " :	\\n")
   (insert-string "//\\n")
   (newline)
   (insert-string "#include <stdio.h>\\n")
   (insert-string "#include <stdlib.h>\\n")
   (insert-string "#include <string.h>\\n")
   (insert-string "#include <iostream.h>\\n")
   (newline)
   (insert-string "#include \"")
   (insert-base-name)
   (insert-string ".h\"\\n")
   (newline)
   (c++-mode))

;;; 'C++ class header' skelton

(defun cpp-header ()
   (end-of-line)
   (fundamental-mode)
   (beginning-of-buffer)
   (insert-string "//\\n")
   (insert-string "// static const char *sccsid = \"@(#)")
   (insert-buffer-name)
   (insert-string "	(c) ")
   (insert-user-name)
   (insert-string " 0000/00/00\";\\n")
   (insert-string "//\\n")
   (newline)
   (insert-string "//\\n")
   (insert-string "//	")
   (insert-buffer-name)
   (insert-string " :	\\n")
   (insert-string "//\\n")
   (newline)
   (insert-string "#ifndef	__")
   (insert-base-name)
   (backward-word)
   (upcase-word)
   (insert-string "_H\\n")
   (insert-string "#define	__")
   (insert-base-name)
   (backward-word)
   (upcase-word)
   (insert-string "_H\\n")
   (newline)
   (insert-string "#include <stdio.h>\\n")
   (insert-string "#include <stdlib.h>\\n")
   (insert-string "#include <string.h>\\n")
   (insert-string "#include <iostream.h>\\n")
   (newline)
   (insert-string "class ")
   (insert-base-name)
   (insert-string " {\\n")
   (insert-string "  public:\\n")
   (insert-string "	// constructor\\n")
   (insert-string "	")
   (insert-base-name)
   (insert-string "();\\n\\n")
   (insert-string "	// destructor\\n")
   (insert-string "	")
   (insert-string "virtual ~")
   (insert-base-name)
   (insert-string "() {}\\n\\n")
   (insert-string "  private:\\n")
   (insert-string "	// copy constructor\\n")
   (insert-string "	")
   (insert-base-name)
   (insert-string "( const ")
   (insert-base-name)
   (insert-string "& rhs );\\n\\n")
   (insert-string "	// assignment method\\n")
   (insert-string "	")
   (insert-base-name)
   (insert-string "& operator=( const ")
   (insert-base-name)
   (insert-string "& obj );\\n\\n")
   (insert-string "	// comparison method\\n")
   (insert-string "	int operator==( const ")
   (insert-base-name)
   (insert-string "& obj ) const;\\n\\n")
   (insert-string "	// print method\\n")
   (insert-string "	friend ostream& operator<<( ostream& os, const ")
   (insert-base-name)
   (insert-string "& obj );\\n\\n")
   (insert-string "	// read method\\n")
   (insert-string "	friend istream& operator>>( istream& is, ")
   (insert-base-name)
   (insert-string "& obj );\\n\\n")
   (insert-string "  protected:\\n\\n")
   (insert-string "  private:\\n\\n")
   (insert-string "};\\n\\n")
   (insert-string "#endif	/* __")
   (insert-base-name)
   (backward-word)
   (upcase-word)
   (insert-string "_H */")
   (c++-mode)
   (beginning-of-buffer)
   (update-screen)
   (write-minibuffer ""))

;;; Standard switch reading inside 'main' procedure.

(defun cpp-standard-argument ()
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

;;; Standard 'main' prolog.

(defun cpp-std-main ()
   (fundamental-mode)
   (beginning-of-buffer)
   (insert-string "#if	!defined( lint )\\n")
   (insert-string "static	const char *sccsid = \"@(#)")
   (insert-buffer-name)
   (insert-string "	(c) ")
   (insert-user-name)
   (insert-string " 0000/00/00\";\\n")
   (insert-string "#endif\\n")
   (newline)
   (insert-string "//\\n")
   (insert-string "//	")
   (insert-buffer-name)
   (insert-string " :	\\n")
   (insert-string "//\\n")
   (newline)
   (insert-string "#include <stdio.h>\\n")
   (insert-string "#include <stdlib.h>\\n")
   (insert-string "#include <string.h>\\n")
   (insert-string "#include <iostream.h>\\n")
   (newline)
   (insert-string "int\\n")
   (insert-string "main( int argc, char *argv[] )\\n")
   (insert-string "{\\n")
   (insert-string "	return( 0 );\\n")
   (insert-string "}\\n")
   (repeat 2 (previous-line))
   (set-mark-command)
   (beginning-of-buffer)
   (update-screen)
   (exchange-point-and-mark)
   (write-minibuffer "")
   (c++-mode))

;;; 'switch' control structure.

(defun cpp-switch ()
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

(defun cpp-while ()
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
        (c++-mode)
	(bind-to-key 'cpp-comment	    "C-C/")
	(bind-to-key 'cpp-do		    "C-CD")
	(bind-to-key 'cpp-for		    "C-CF")
	(bind-to-key 'cpp-if		    "C-CI")
	(bind-to-key 'cpp-package	    "C-CP")
	(bind-to-key 'cpp-header	    "C-CH")
	(bind-to-key 'cpp-standard-argument "C-CA")
	(bind-to-key 'cpp-std-main	    "C-CM")
	(bind-to-key 'cpp-switch	    "C-CS")
	(bind-to-key 'cpp-while		    "C-CW"))
