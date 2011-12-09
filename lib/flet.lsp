;;;; -*-Mode:LISP; Package:LISP; Base:10; Syntax:ISLISP -*-
;;;; Title:     flet.lsp
;;;; Author:    C. Jullien
;;;; License:   New BSD license
;;;; CVS:       "$Id: flet.lsp,v 1.13 2010-06-12 12:45:42 jullien Exp $"

;;;
;;; FLET / LABELS Special Forms (written as MACRO).
;;;
;;; This  definition is only used by eval.  The compiler uses its own
;;; and more accurate definition.  You can notice differences between
;;; the  two  versions.  The  compiled  one  should  be  taken as the
;;; reference.
;;;

(require "setf")

(export '("flet" "labels") "islisp")

(defpackage "flet"
   (:use    "openlisp"))

(in-package "flet")

(defmacro flet (lfun &rest body-form)
  ;; flet Special Form.
  ;; the scope of a function identifier is only the body-form.
  `(let ,(mapcar (lambda (x)
                    `(,(%fl-concat-f 'flet (car x)) (lambda ,@(cdr x))))
                 lfun)
        ,@(%fl-body 'flet body-form (mapcar #'car lfun))))

(defmacro labels (lfun &rest body-form)
  ;; labels Special Form.
  ;; the scope of a function identifier is the whole labels special
  ;; form (excluding nested scopes, if any).
  (let* ((fs  (mapcar #'car lfun))
         (cfs (mapcar (lambda (x) (%fl-concat-f 'labels x)) fs)))
        `(let ,(mapcar (lambda (x) (list x nil)) cfs)
              ,@(mapcar (lambda (n x)
                           `(setq ,n ,(%fl-one-form 'labels
                                                    `(lambda ,@(cdr x)) fs)))
                        cfs
                        lfun)
              ,@(%fl-body 'labels body-form fs))))

(defun %fl-concat-f (f x)
   ;; generates lexical function name.
   (if (symbolp x)
       (concat '%fl-function- x)
       (system::signal-domain-error (symbol-name f) x (class <symbol>))))

(defun %fl-body (name form lfun)
   ;; expands a list of forms using lexical function list lfun.
   (mapcar (lambda (x) (%fl-one-form name x lfun)) form))

(defun %fl-expand-rest (name form lfun)
   ;; generates  a  binding  form  where initialization value is done
   ;; using  lexical  function  list  lfun.  The  first form is never
   ;; expanded. It's used by binding forms such as LET, LET*, DO ...
   (mapcar (lambda (x)
              (if (atom x)
                  x
                  (cons (car x) (%fl-body name (cdr x) lfun))))
           form))

(defun %fl-one-form (name f l)
   ;; expands one form using lexical function defined in list l.
   (let ((op (if (consp f) (car f))))
        (cond
           ((atom f)
            ;; standard variable, lookup in lexical environment
            f)
           ((consp op)
            (%fl-body name f l))
           ((member op l)
            ;; a  call  to  a  lexically bound function.  Use funcall
            ;; with its associated function.
            `(funcall ,(%fl-concat-f name op) ,@(%fl-body name (cdr f) l)))
           ((and (symbolp op) (macro-function op))
            ;; expand macro and start again.
            (%fl-one-form name (macroexpand f) l))
           (t
            (case op
               ((CLASS DYNAMIC GO QUOTE)
                ;; variables   inside   those  forms  should  not  be
                ;; converted.
                f)
               ((LAMBDA INTERNAL-LAMBDA)
                `(,op ,(cadr f) ,@(%fl-body name (cddr f) l)))
               ((FUNCTION)
                (cond
                      ((consp (cadr f))
                       ;; a lambda, expand its body.
                       `(function (%fl-one-form name (cadr f) l)))
                      ((member (cadr f) l)
                       ;; a lexically definied function.
                       (%fl-concat-f name (cadr f)))
                      (t
                       ;; a globally defined symbol.
                       f)))
               ((CASE CASE-USING)
                ;; expand  the value and the body forms.  Case values
                ;; are leave unmodified.
                `(,op ,(%fl-one-form name (cadr f) l)
                      ,@(%fl-expand-rest name (cddr f) l)))
               ((COND)
                `(,op ,@(mapcar (lambda (x)
                                   (%fl-expand-rest name x l))
                                (cdr f))))
               ((LET LET* DYNAMIC-LET)
                ;; expand initialization values and body.
                `(,op ,(%fl-expand-rest name (cadr f) l)
                      ,@(%fl-body name (cddr f) l)))
               ((DO DO* FOR)
                ;; TO BE CHECKED!!! expand initialization values and body.
                `(,op ,(%fl-expand-rest name (cadr f) l)
                      ,@(%fl-body name (cddr f) l)))
               ((WITH-OPEN-INPUT-FILE WITH-OPEN-OUTPUT-FILE WITH-OPEN-IO-FILE)
                ;; stream variable should not be converted.
                `(,op ,(car (%fl-expand-rest name (list (cadr f)) l))
                      ,@(%fl-body name (cddr f) l)))
               (t
                ;; no  special  case,  leave  the function unmodified
                ;; and expand the body.
                `(,op ,@(%fl-body name (cdr f) l))))))))

(provide "flet")
