;;;; -*-Mode:LISP; Package:LISP; Base:10; Syntax:ISLISP -*-
;;;; Title:    setf.lsp
;;;; Author:    C. Jullien
;;;; License:   New BSD license
;;;; CVS:      "$Id: setf.lsp,v 1.48 2018/07/29 13:16:39 jullien Exp $"

;;; Generalized set function

(in-package #:openlisp)

(export '(defsetf incf decf pop push pushnew))

(defmacro setf (forms val)
   (cond
         ((symbolp forms)
          `(setq ,forms ,val))
         ((consp forms)
          (let ((expand (if (and (symbolp (car forms))
                                 (macro-function (car forms)))
                            (macroexpand-all forms)
                            forms))
                (exp    ())
                (form   ()))
               (cond
                     ((symbolp expand)
                      `(setq ,expand ,val))
                     ((property (car forms) 'setf)
                      ;; defined by a DEFSETF
                      (setq form   (car forms))
                      (setq exp    (cadr forms))
                      (setq expand forms)
                      (system::setf-macro-expander form val exp expand))
                     (t
                      (setq form   (if (symbolp expand) expand (car expand)))
                      (setq exp    (cadr expand))
                      (system::setf-macro-expander form val exp expand)))))
         (t
           (error "SETF: unknown form : ~A~%" forms))))

(defun system::setf-macro-expander (form val exp expand)
   (case form
         ;; Standard for ISLISP

         ((car)             `(set-car ,val ,exp))
         ((cdr)             `(set-cdr ,val ,exp))
         ((elt)             `(set-elt ,val ,exp ,(caddr expand)))
         ((aref)            `(set-aref ,val ,exp ,@(cddr expand)))
         ((garef)           `(set-garef ,val ,exp ,@(cddr expand)))
         ((dynamic)         `(set-dynamic ,val ,exp))
         ((property)        `(set-property ,val ,exp ,(caddr expand)))

         ;; Extension for compatibility with other implementations.

         ((first)           `(set-car ,val ,exp))
         ((second)          `(set-car ,val (nthcdr 1 ,exp)))
         ((third)           `(set-car ,val (nthcdr 2 ,exp)))
         ((fourth)          `(set-car ,val (nthcdr 3 ,exp)))
         ((fifth)           `(set-car ,val (nthcdr 4 ,exp)))
         ((sixth)           `(set-car ,val (nthcdr 5 ,exp)))
         ((seventh)         `(set-car ,val (nthcdr 6 ,exp)))
         ((eighth)          `(set-car ,val (nthcdr 7 ,exp)))
         ((ninth)           `(set-car ,val (nthcdr 8 ,exp)))
         ((tenth)           `(set-car ,val (nthcdr 9 ,exp)))
         ((rest)            `(set-cdr ,val ,exp))
         ((char)            `(set-char ,val ,exp ,(caddr expand)))
         ((caar)            `(set-car ,val (car ,exp)))
         ((cadr)            `(set-car ,val (cdr ,exp)))
         ((cdar)            `(set-cdr ,val (car ,exp)))
         ((cddr)            `(set-cdr ,val (cdr ,exp)))
         ((caaar)           `(set-car ,val (caar ,exp)))
         ((caadr)           `(set-car ,val (cadr ,exp)))
         ((cadar)           `(set-car ,val (cdar ,exp)))
         ((caddr)           `(set-car ,val (cddr ,exp)))
         ((cdaar)           `(set-cdr ,val (caar ,exp)))
         ((cdadr)           `(set-cdr ,val (cadr ,exp)))
         ((cddar)           `(set-cdr ,val (cdar ,exp)))
         ((cdddr)           `(set-cdr ,val (cddr ,exp)))
         ((caaaar)          `(set-car ,val (caaar ,exp)))
         ((caaadr)          `(set-car ,val (caadr ,exp)))
         ((caadar)          `(set-car ,val (cadar ,exp)))
         ((caaddr)          `(set-car ,val (caddr ,exp)))
         ((cadaar)          `(set-car ,val (cdaar ,exp)))
         ((cadadr)          `(set-car ,val (cdadr ,exp)))
         ((caddar)          `(set-car ,val (cddar ,exp)))
         ((cadddr)          `(set-car ,val (cdddr ,exp)))
         ((cdaaar)          `(set-cdr ,val (caaar ,exp)))
         ((cdaadr)          `(set-cdr ,val (caadr ,exp)))
         ((cdadar)          `(set-cdr ,val (cadar ,exp)))
         ((cdaddr)          `(set-cdr ,val (caddr ,exp)))
         ((cddaar)          `(set-cdr ,val (cdaar ,exp)))
         ((cddadr)          `(set-cdr ,val (cdadr ,exp)))
         ((cdddar)          `(set-cdr ,val (cddar ,exp)))
         ((cddddr)          `(set-cdr ,val (cdddr ,exp)))
         ((svref)           `(svset ,val ,exp ,(caddr expand)))
         ((gethash)         `(puthash ,exp ,(caddr expand) ,val))
         ((symbol-value)    `(set-symbol-value    ,val ,exp))
         ((symbol-global)   `(set-symbol-global   ,val ,exp))
         ((symbol-package)  `(set-symbol-package  ,val ,exp))
         ((symbol-plist)    `(set-symbol-plist    ,val ,exp))
         ((symbol-function) `(set-symbol-function ,val ,exp))
         ((macro-function)  `(set-macro-function  ,val ,exp))
         ((find-class)      `(setq ,exp ,val))
         ((slot-value)      `(set-slot-value ,val ,exp ,(caddr expand)))
         ((file-mode)       `(set-file-mode  ,val ,exp))
         ((string-type)     `(string-type ,exp ,val))
         ((vector-type)     `(vector-type ,exp ,val))
         ((readtable-case)  `(set-readtable-case ,exp ,val))
         ;; a defsetf or a slot-access ?
         (t
           (if (eq (function-type form) 'slot-access)
               ;; slot-access
               `(,form ,exp ,val)
               (let ((inv (property form 'setf)))
                    (cond
                          ((null inv)
                           (error "SETF: unknown form : ~A~%" form))
                          ((and (symbolp inv)
                                (string-index "%%setf-" (symbol-name inv)))
                           ;; don't change %%gf-setf- name !!!
                           ;; or see also OLSETF_PREFIX in class.c
                           `(,inv ,val ,@(cdr expand)))
                          ((symbolp inv)
                           ;; new value is the last argument
                           `(,inv ,@(cdr expand) ,val))
                          (t
                           (funcall (apply inv (cdr expand)) val))))))))

;;; Extension to ISLISP.

(defmacro defsetf (access &rest rest)
   ;; create a new setf form.
   (cond
         ((not (symbolp access))
          (error "DEFSETF : not a symbol ~S~%" access))
         ((listp (car rest))    
          ;; complex case : (defsetf foo lambda-list (new-value) . body)
          (if (listp (cadr rest))
              `(set-property (lambda ,(car rest)
                                (lambda ,(cadr rest) ,@(cddr rest)))
                           ',access
                           'setf)
              (error "DEFSETF : bad definition ~A~%" (cadr rest))))
         ((symbolp (car rest))
          ;; simple case : (defsetf f g)
          `(set-property ',(car rest) ',access 'setf))
         (t
          (error "DEFSETF : bad definition ~A~%" (car rest)))))

;;; Warning  !!  the  following  macros are globally false when place
;;; has side effect

(defmacro incf (place &rest delta)
   (if (consp delta)
      `(setf ,place (+ ,place ,(car delta)))
      `(setf ,place (1+ ,place))))

(defmacro decf (place &rest delta)
   (if (consp delta)
      `(setf ,place (- ,place ,(car delta)))
      `(setf ,place (1- ,place))))

(defmacro pop (place)
   `(when ,place
          (prog1 (car ,place)
                 (setf ,place (cdr ,place)))))

(defmacro push (item place)
   `(setf ,place (cons ,item ,place)))

(defmacro pushnew (item place &rest options)
   (cond
         ((null options)
          `(setf ,place (adjoin ,item ,place)))
         ((or (member :test options)
              (member :test-not options))
          `(setf ,place (adjoin ,item ,place ,@options)))
         (t
          (error "PUSHNEW : unsupported options ~A~%" options))))

#|
(defun bar (x y)
   (format t "~a ~a~%" x y))

(defsetf foo bar)
(setf (foo 'A) 8)

(defsetf get (symbol indicator) (new-val)
  `(format t "putprop ~a ~a ~a~%"))
|#

(provide "setf")
