;;;; -*-Mode:LISP; Package:LISP; Base:10; Syntax:ISLISP -*-
;;;; Title:     pretty.lsp
;;;; Author:    C. Jullien
;;;; License:   New BSD license
;;;; CVS:       "$Id: pretty.lsp,v 1.48 2018/07/29 13:16:39 jullien Exp $"

;;; Trivial pretty-printer

(require "setf")

(in-package #:openlisp)

(export '(pretty pprint))

(defpackage #:pretty
   (:use    #:openlisp))

(in-package #:pretty)

(defglobal *max-pos*  79)
(defglobal *pos*       0)
(defglobal *ppstream* ())

(set-property 2 'lambda                'special)
(set-property 2 'tagbody               'special)
(set-property 4 'case-using            'special)
(set-property 6 'unwind-protect        'special)
(set-property 6 'return-from           'special)
(set-property 7 'dynamic-let           'special)
(set-property 4 'with-open-input-file  'special)
(set-property 4 'with-open-output-file 'special)
(set-property 4 'with-open-io-file     'special)
(set-property 4 'with-handler          'special)
(set-property 4 'with-standard-input   'special)
(set-property 4 'with-standard-output  'special)

;; Main functions

(defun pretty (object &rest stream)
   ;; pretty print a function or any list expression.
   (when (symbolp object)
         (let ((def (function-definition object)))
              (when (consp def)
                    (pprint def (if stream (car stream) (standard-output))))))
   object)

(defun pprint (x &rest stream)
   ;; pretty print an expression.
   (setq *ppstream* (if stream (car stream) (standard-output)))
   (setq *pos*      0)
   (%pp-object x ())
   (%pp-newline)
   (%pp-newline)
   t)

;; Implementation

(defun %pp-char (s)
   ;; pretty print a single ASCII char.
   (format *ppstream* "~A" s)
   (incf *pos*))

(defun %pp-atom (s)
   ;; pretty print an atom.
   (cond
         ((stringp s)
          (%pp-string s t))
         ((symbolp s)
          (%pp-string (format () "~S" s) ()))
         ((characterp s)
          (%pp-string (format () "~S" s) ()))
         ((functionp s)
          (%pp-string (format () "~A" s) t))
         ((vectorp s)
          (format *ppstream* "~S" s)
          (incf *pos* (%pp-flat-size s)))
         (t
          (format *ppstream* "~S" s)
          (incf *pos* (%pp-flat-size s)))))

(defun %pp-string (s flag)
   ;; pretty print a strings.
   (format *ppstream* (if flag "~S" "~A") s)
   (incf *pos* (length s)))

(defun %pp-flat-size (s)
   ;; computes the length of an expression.
   (let ((str (create-string-output-stream))
         (res ()))
        (format str "~S" s)
        (setq res (get-output-stream-string str))
        (close str)
        (length res)))

(defun %pp-object (x cst)
   ;; pretty print a single object
   (cond
         ((atom x)
          (%pp-atom x))
         ((and (eq (car x) 'QUOTE) (eq (length x) 2))
          (%pp-char #\')
          (%pp-object (cadr x) t))
         ((and (eq (car x) 'FUNCTION) (eq (length x) 2))
          (%pp-char #\#)
          (%pp-char #\')
          (%pp-object (cadr x) cst))
         ((> (%pp-flat-size x) (- *max-pos* *pos*))
          ;; does not fit in a single line
          (%pp-list x cst t))
         (t
          (%pp-list x cst ()))))

(defun %pp-list (l cst vertical-p)
   ;; pretty print a list of objects. Take care of Special forms.
   (%pp-char #\( )
   (let ((pos1 *pos*)
         (pos2 nil)
         (key  nil)
         (tag  nil)
         (flag nil))
        (%pp-object (car l) cst)
        (when (and vertical-p (consp (car l)) (cdr l))
              (%pp-indent (+ pos1 1)))
        (setf pos2 (+ *pos* 1))
        (setf key  (car l))
        (cond
              ((or cst (= (length l) 1))
               (setq l (cdr l)))
              ((and (member key
                            '(DEFUN DEFMACRO DEFGENERIC DEFMETHOD DEFINLINE))
                    (>= (length l) 3)
                    (listp (caddr l)))
               (%pp-char #\space)
               (%pp-object (cadr l) cst)
               (%pp-char #\space)
               (%pp-arglist (caddr l) cst)
               (setq l (cdddr l))
               (setq pos2 3)
               (%pp-indent (- pos2 1)))
              (t
               (case key
                     ((TAGBODY)
                      (%pp-indent (+ pos1 (property key 'special) -1))
                      (setq vertical-p t)
                      (setq l (cdr l))
                      (setq tag t))
                     ((CASE)
                      (%pp-char #\space)
                      (%pp-object (cadr l) cst)
                      (%pp-indent (- pos2 1))
                      (setq vertical-p t)
                      (setq l (cddr l)))
                     ((CASE-USING)
                      (%pp-char #\space)
                      (%pp-object (cadr l) cst)
                      (%pp-char #\space)
                      (%pp-object (caddr l) cst)
                      (%pp-indent (+ pos1 (property key 'special) -1))
                      (setq vertical-p t)
                      (setq l (cdddr l)))
                     ((LET LET* FOR DO DO*)
                      (%pp-char #\space)
                      (cond
                            ((consp (cadr l))
                             (%pp-char #\()
                             (let ((lst (cadr l)))
                                  (while (consp lst)
                                         (%pp-object (car lst) cst)
                                         (and (setq lst (cdr lst))
                                              (%pp-indent (+ pos2 1)))))
                             (%pp-char #\)))
                            (t
                             (%pp-object (cadr l) cst)))
                      (%pp-indent (- pos2 1))
                      (setq vertical-p t)
                      (setq l (cddr l)))
                     (t
                      (setq l (cdr l))))))
        (while (consp l)
               (when flag
                     (%pp-indent (if (and (symbolp key)
                                          (property key 'special))
                                     (+ pos1 (property key 'special) -1)
                                     (- pos2 1))))
               (when (and tag (consp (car l)))
                     (%pp-spaces 4))
               (%pp-char #\space)
               (%pp-object (car l) cst)
               (setq flag vertical-p)
               (setq l (cdr l)))
        (when l
              ;; dotted pair
              (%pp-char #\space)
              (%pp-char #\.)
              (%pp-char #\space)
              (if flag
                  (%pp-indent pos2))
              (%pp-object l cst))
        (%pp-char #\) )))

(defun %pp-arglist (args cst)
   ;; print function argument list (adding &rest when needed)
   (if (not (listp args))
       (%pp-object (list '&rest args) cst)
       (let ((b (copy-seq args))
             (d ())
             (l ()))
            (setq l b)
            (while (consp l)
                   (setq d l)
                   (setq l (cdr l)))
            (when l
                  (setf (cdr d) (list '&rest l)))
            (%pp-object b cst))))

(defun %pp-indent (x)
   ;; intent up to position x.
   (%pp-newline)
   (%pp-spaces x))

(defun %pp-spaces (x)
   ;; add x spaces.
   (for ((i 0 (+ i 1)))
        ((>= i x))
        (%pp-char #\space)))

(defun %pp-newline ()
   ;; insert a newline.
   (format *ppstream* "~%")
   (setq *pos* 0))

(provide "pretty")
