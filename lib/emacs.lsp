;;;; -*-Mode:LISP; Package:LISP; Base:10; Syntax:ISLISP -*-
;;;; Date:	2009/05/02
;;;; Title:	emacs.lsp
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

;;;
;;; Startup for EmACT standard environnment
;;;

(in-package "emacs")

;; User name

(defun insert-user-name ()
   (insert-string "C. Jullien"))

;; Load C macros

(defun c-macro-file ()
   (write-minibuffer "Loading C macros from c.lsp ...")
   (load-macro "c.lsp")
   (write-minibuffer ": C macros loaded."))

;; Load C macros

(defun cpp-macro-file ()
   (write-minibuffer "Loading C++ macros from cpp.lsp ...")
   (load-macro "cpp.lsp")
   (write-minibuffer ": C++ macros loaded."))

;; Load Le-Lisp macros

(defun lelisp-macro-file ()
   (write-minibuffer "Loading Le-Lisp macros from lelisp.lsp ...")
   (load-macro "lelisp.lsp")
   (write-minibuffer ": Le-Lisp macros loaded."))

;; Load Java macros

(defun java-macro-file ()
   (write-minibuffer "Loading Java macros from java.lsp ...")
   (load-macro "java.lsp")
   (write-minibuffer ": Java macros loaded."))

;; Load OpenLisp macros

(defun openlisp-macro-file ()
   (write-minibuffer "Loading OpenLisp macros from openlisp.lsp ...")
   (load-macro "openlisp.lsp")
   (write-minibuffer ": OpenLisp macros loaded."))

;; Load prolog macros

(defun xilog-macro-file ()
   (write-minibuffer "Loading XILOG macros from xilog.lsp ...")
   (load-macro "xilog.lsp")
   (write-minibuffer ": XILOG macros loaded."))

;; Load SAM macros

(defun sam-macro-file ()
   (write-minibuffer "Loading SAM macros from sam.lsp ...")
   (load-macro "sam.lsp")
   (write-minibuffer ": SAM macros loaded."))

;; Load SML macros

(defun sml-macro-file ()
   (write-minibuffer "Loading SML macros from wml.lsp ...")
   (load-macro "sml.lsp")
   (write-minibuffer ": SML macros loaded."))

;; Load WML macros

(defun wml-macro-file ()
   (write-minibuffer "Loading WML macros from wml.lsp ...")
   (load-macro "wml.lsp")
   (write-minibuffer ": WML macros loaded."))

(progn

	;; Color code :
        ;;
	;;   black   (0), blue    (1), green   (2), cyan    (3)
        ;;   red     (4), magenta (5), yellow  (6), white   (7)

	;; Common variable initialisation

        (setq assembler-name             "ml")    ;; Microsoft assembler
        (setq background-color           1)       ;; Blue
        (setq backup-before-writing      t)       ;; Save old file with .BAK
        (setq bold-font                  t)       ;; Bold font attribute.
        (setq binary-mode                nil)     ;; Don't convert to CR-LF
        (setq compiler-arguments         "")      ;; Opt. compiler args.
        (setq compiler-name              "cl")    ;; Microsoft C compiler
        (setq confirm-unsaved-buffer     t)       ;; Default GNU behavior
        (setq date-completion            t)       ;; Change the date on save.
        (setq display-43-lines           t)       ;; Obsolete, use screen-XXXX
        (setq fast-redisplay             t)       ;; For very OLD PCs
        (setq foreground-color           3)       ;; Light grey
        (setq fill-prefix                "")      ;; Default fill prefix
        (setq fill-column                70)      ;; Default fill column
        (setq java-compiler-name         "javac") ;; Java compiler
        (setq java-executable-name       "java")  ;; Java machine
        (setq line-number-mode           t)       ;; Add cur. line in mode line
        (setq make-arguments             "")      ;; Opt. make arguments
        (setq make-name                  "nmake") ;; Make utility
        (setq monochrome-monitor         nil)     ;; Use with a color monitor
        (setq mouse-avoidance-mode       t)       ;; Mouse moves after N strokes
        (setq mouse-avoidance-nudge-var  3)       ;; Nb of strokes before move
        (setq mouse-flag                 t)       ;; Use with a mouse (on DOS)
        (setq screen-height              50)      ;; Only a teminal height guess
        (setq screen-width               80)      ;; Only a teminal width  guess
        (setq show-menu                  t)       ;; Windows menu
        (setq system-colors              nil)     ;; Windows colors
        (setq tabulation-display         8)       ;; Tabulation display
        (setq tabulation-size            8)       ;; Tabulation size

	;; Custom Help files (can be any *.hlp or *.htm or *.html)

	;; Java API
        (setq help-file1 "d:/jdk1.1.4/hlp/jdk.hlp")
	;; ISO C++
        (setq help-file2 "c:/usr/jullien/cplusplus/fdis/index.html")
	;; Java API
        (setq help-file3 "d:/jdk1.1.4/docs/index.html")
	;; Common Lisp
        (setq help-file4 "c:/usr/jullien/openlisp/cltl/CLtL2.chm")

	;; Keyboard assigment

	(bind-to-key 'c-macro-file	  "C-CC")
	(bind-to-key 'cpp-macro-file	  "C-C+")
	(bind-to-key 'lelisp-macro-file	  "C-CL")
	(bind-to-key 'openlisp-macro-file "C-CO")
;	(bind-to-key 'sam-macro-file      "C-CS")
	(bind-to-key 'sml-macro-file      "C-CS")
	(bind-to-key 'java-macro-file     "C-CJ")
	(bind-to-key 'wml-macro-file	  "C-CW")
	(bind-to-key 'xilog-macro-file	  "C-CX"))
