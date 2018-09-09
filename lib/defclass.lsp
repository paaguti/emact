;;;; -*-Mode:LISP; Package:LISP; Base:10; Syntax:ISLISP -*-
;;;; Title:     defclass.lsp
;;;; Author:    C. Jullien
;;;; License:   New BSD license
;;;; CVS:       "$Id: defclass.lsp,v 1.48 2018/07/29 13:16:39 jullien Exp $"

;;; DEFCLASS package.

;;; This  module implements the defclass macro that expand the object
;;; system  with  user classes.  All user objects inherit directly or
;;; indirectly  from  <standard-object>  class.  This  implementation
;;; fully  conforms  to  ISO/IEC  13816 ISLISP Standard.  Most of the
;;; code is done in C (class.c)

(require "setf")

(in-package #:openlisp)

(export '(*initarg-as-keywords* slot-unbound slot-missing))

(defpackage #:defclass
  (:use #:openlisp))

(in-package #:defclass)

(defglobal *initarg-as-keywords* ())    ;; ISLISP standard (CLtL is t)

;;; The DEFCLASS macro.

(defmacro islisp:defclass (name sc-list slot-spec &rest class-opt)
   ;; Define a new class,  just a wrapper to the real %expand-defclass
   ;; function
   (%expand-defclass name sc-list slot-spec class-opt))

(defgeneric slot-unbound (cl instance slot)
   ;; Default method for unbound slots.
   (:method ((cl <standard-class>) (instance <object>) (slot <object>))
            (error "unbound slot ~a for ~a~%" slot instance)))

(defgeneric slot-missing (cl instance slot caller)
   ;; Default method for unbound slots.
   (:method ((cl       <standard-class>)
             (instance <object>)
             (slot     <object>)
             (caller   <symbol>))
            (error "~a : slot missing ~a : ~a~%" caller slot instance)))

;;; Helper functions

(defconstant +slot-option-keywords+
  ;; slot keywords that define generic functions
  '(:accessor :reader :writer :boundp))

(defun %expand-defclass (name sc-list slot-spec class-opt)
   ;; Check the name
;   (unless (and (symbolp name) (not (special-form-p name)))
;           (error "defclass: illegal-class-name ~a~%" name))
   (when (instancep (find-class name nil) (class <built-in-class>))
         (error "defclass: cannot-redefine a built-in-class ~a~%" name))
   ;; Check super-classes arguments
   (%check-superclasses sc-list)
   (let ((initform  ())
         (initarg   ())
         (fields    ())
         (abstractp :abstractp)
         (options   ())
         (slots     ()))
        (setq slots (%compute-slot-declarations sc-list slot-spec))
        (dolist (opt class-opt)
           (case (and (consp opt) (car opt))
                 ((:abstractp)
                  (unless (and (eq (length opt) 2)
                               (member (cadr opt) '(t nil)))
                          (error "defclass: wrong :abstractp value '~s'~%" opt))
                  (unless (or (eq abstractp :abstractp)
                              (eq abstractp (cadr opt)))
                          (error "defclass: more than one :abstractp~%"))
                  (setq abstractp (cadr opt)))
                 ((:metaclass)
                  (unless (and (eq (length opt) 2)
                               (eq (cadr opt) '<standard-class>))
                          (error "defclass: wrong metaclass: '~s'~%" opt)))
                 (t
                  (error "defclass: wrong option: '~s'~%" opt))))
        (setq fields    (mapcar (lambda (x)
                                   (if (consp x)
                                       (car x)
                                       x))
                                slots))
        (setq options   (let ((i -1))
                             (mapcar (lambda (x)
                                        (%compute-slot-options name x (incf i)))
                                     slots)))
        (setq initform  (cons 'list (mapcar #'car options)))
        (setq initarg   (mapcar #'cadr options))
        (setq slots     (apply #'append (mapcar #'caddr options)))
#+(eq *initarg-as-keywords* t)
        (setq keywords  (mapcar (lambda (x)
                                   (when x `(defconstant ,x ',x)))
                                initarg))
        (when (eq abstractp :abstractp)
              (setf abstractp nil))
        (when (and (null sc-list)
                   ;; avoid bootstrap infinie loop:
                   (not (eq name '<object>)))
              (setq sc-list '(<standard-object>)))
        `(progn
               (system::internal-build-class ',name
                                             ',sc-list
                                             ,abstractp
                                             ;; next is for create-class-info
                                             ',fields
                                             ,initform
                                             ',initarg
                                             ',slot-spec
                                             ',sc-list
                                             '<general-vector>)
#+(eq *initarg-as-keywords* t)
               ,@keywords
               ;; define generic functions for slots access.
               ,@slots
               ',name)))

(defun %find-class (cl)
   ;; find-class compiler helper.
   (system::get-class-info cl))

(defun %check-superclasses (sc-list)
   ;; validate superclasses.
   (dolist (x sc-list)
      (when (and (not (eq x '<standard-object>))
                 (instancep (%find-class x) (class <built-in-class>)))
            (error "defclass: cannot inherit from built-in-class ~a~%" x))
      (let ((l sc-list))
           (while (consp l)
                  (cond
                        ((member (car l) (cdr l))
                         (error "defclass: duplicated superclass ~a~%" x))
                        ((not (eq (car l) x))
                         (%check-common-classes x (car l))))
                  (pop l)))))

(defun %check-common-classes (c1 c2)
   ;; The only superclasses that C1 and C2 may have in common are
   ;; <standard-object> or <object> (See: chap. 10, p 13).
   (let ((common nil)
         (pl1 (class-precedence-list (%find-class c1)))
         (pl2 (class-precedence-list (%find-class c2))))
        (while (and (null common) (consp pl1))
               (setq common (member (car pl1) pl2))
               (if common
                   (setq common (car common))
                   (setq pl1 (cdr pl1))))
        (unless (or (null common)
                    (eq common (class <standard-object>))
                    (eq common (class <object>)))
                (error "defclass: wrong sc-list, ~s is common to ~s and ~s~%"
                       common c1 c2))
        common))

(defun %compute-slot-declarations (sc-list slot-spec)
   ;; Compute slot descriptions (deal with multiple inheritance)
   ;; NOTE: this function is too complex and probably wrong.
   (let ((supers  (%compute-super-classes  sc-list))
         (direct  (%compute-direct-classes sc-list))
         (gfnames ())
         (slots   ())
         (sslots  ()))
        ;; Check slots arguments
        (unless (every #'consp slot-spec)
                (error "defclass: bad-slot-descriptions ~a~%" slot-spec))
        ;; Build  slot  declarations  for  direct inherited slots,  no
        ;; accessors here.
        (dolist (x direct)
                (let ((inherited (system::compute-class-descriptions x)))
                     ;; Remove common base classes, it should not appends.
                     (setq supers (remove x supers :test #'equal))
                     ;; Compute unmodified inherited slots (with gf accessors).
                     (setq sslots (append sslots inherited))
                     ;; Compute inherited slots (without gf accessor).
                     (setq slots
                           (append (mapcar #'%compute-direct-slot-declarations
                                           inherited)
                                   slots))))
        ;; Add slots that belongs to the class
        (setq slots  (append slots slot-spec))
        ;; compute gf accessors either declared or inherited.
        (setq sslots (append sslots slot-spec))
        (dolist (x sslots)
                (setq gfnames
                      (append gfnames (%compute-generic-functions x nil))))
        ;; Add slots from multiple inheritance (if any) and remove
        ;; gf conflitcs from declared or inherited classes.
        (dolist (x supers)
                (let ((new-slots (system::compute-class-descriptions x)))
                     (setq new-slots
                           (mapcar (lambda (s)
                                      (%compute-slot-no-conflitcs s gfnames))
                                   new-slots))
                     (setq slots (append slots new-slots))))
        slots))

(defun %compute-direct-slot-declarations (slot)
   ;; Remove  generic  function  declarations  inherited  from  direct
   ;; ancestors.  That  is,  build  a  new  slot  declaration  without
   ;; :accessor :reader :writer or :boundp options.
   (cond
         ((null slot)
          ())
         ((member (car slot) +slot-option-keywords+)
          (%compute-direct-slot-declarations (cddr slot)))
         (t (cons (car slot) (%compute-direct-slot-declarations (cdr slot))))))

(defun %compute-generic-functions (slot gf)
   ;; Compute generic  function  declarations  inherited  from  direct
   ;; ancestors.
   (cond
         ((null slot)
          gf)
         ((member (car slot) +slot-option-keywords+)
          (%compute-generic-functions (cddr slot) (cons (cadr slot) gf)))
         (t (%compute-generic-functions (cdr slot) gf))))

(defun %compute-slot-no-conflitcs (slot gf)
   ;; compute slot decription removing gf accessors already defined.
   (cond
         ((null slot)
          ())
         ((and (member (car slot) +slot-option-keywords+)
               (member (cadr slot) gf))
          (%compute-slot-no-conflitcs (cddr slot) gf))
         (t (cons (car slot) (%compute-slot-no-conflitcs (cdr slot) gf)))))

(defun %compute-direct-classes (sc-list)
   ;; Given  a super class list 'sc-list',  returns the list of direct
   ;; super classes.
   (cond
         ((symbolp sc-list)
          sc-list)
         (t
          (setq sc-list (car sc-list))
          (cons
            (%compute-direct-classes sc-list)
            (%compute-direct-classes (system::compute-class-super sc-list))))))

(defun %compute-super-classes (sc-list)
   ;; Given  a  super  class  list 'sc-list',  returns the list of all
   ;; inherited slots.
   (cond
         ((null sc-list)
          ())
         ((symbolp sc-list)
          (cons (%compute-direct-classes sc-list)
                (%compute-super-classes (system::compute-class-super sc-list))))
         (t
          (append (%compute-super-classes (car sc-list))
                  (%compute-super-classes (cdr sc-list))))))

(defun %compute-slot-options (name slot index)
   ;; Compute the slot options.
   ;; Valid keywords are :
   ;;
   ;;   :ACCESSOR accessor-name
   ;;   :INITARG  symbol
   ;;   :INITFORM form
   ;;   :READER   reader-name
   ;;   :WRITER   writer-name
   ;;   :BOUNDP   boundp-name
   ;;
   (let ((accessors ())
         (initform  ())
         (initarg   ())
         (key       ())
         (slot-name ())
         (val       ()))
        (cond
              ((consp slot)
               ;; skip slot name
               (setq slot-name (car slot))
               (setq slot      (cdr slot)))
              (t
               (error "defclass: invalid slot argument list ~a~%" slot)))
        ;; parse slot options
        (while (consp slot)
               (unless (consp (cdr slot))
                       (error "defclass: invalid slot argument list ~a~%" slot))
               (setq key  (car  slot))
               (setq val  (cadr slot))
               (setq slot (cddr slot))
               (case key
                     ((:initarg)
                      (if (and (null initarg) (variablep val))
                          (setq initarg val)
                          (error "defclass: syntax error ~a~%" slot)))
                     ((:initform)
                      (if (null initform)
                          (setq initform (%compute-initform val))
                          (error "defclass: syntax error ~a~%" slot)))
                     ((:boundp)
                      (push `(defmethod ,val ((obj ,name))
                                (slot-boundp obj ',slot-name))
                            accessors))
                     ((:reader)
                      (push `(defmethod ,val ((obj ,name))
                                (system::get-slot-index obj ,index))
                            accessors))
                     ((:writer)
                      (push `(defmethod ,val
                                 ((val <object>) (obj ,name))
                                (system::set-slot-index val obj ,index))
                            accessors))
                     ((:accessor)
                      (push `(defmethod ,val ((obj ,name))
                                (system::get-slot-index obj ,index))
                            accessors)
                      (push `(defmethod ,(%compute-name "%setf-" val)
                                 ((obj ,name) (val <object>))
                                (system::set-slot-index val obj ,index))
                            accessors)
                      (push `(defsetf ,val ,(%compute-name "%setf-" val))
                            accessors))
                     (t (error "defclass: invalid slot option ~a~%" key))))
        (list (or initform ''system::|#<unbound-value>|)
              initarg
              (append
                      (%compute-missing-generic-functions accessors)
                      accessors))))

(defun %compute-name (prefix name)
   (convert (string-append prefix (convert name <string>)) <symbol>))

(defun generic-name-p (name)
   (and (fboundp name) (generic-function-p (symbol-function name))))

(defun %assure-defgeneric (name arg)
  `(unless (generic-name-p ',name)
           (defgeneric ,name ,arg)))

(defun %compute-missing-generic-functions (accessors)
   ;; Automatically create generic functions for accessors.
   (let ((l ()))
        (dolist (x accessors)
                (if (eq (car x) 'defmethod)
                    (let ((y (cadr x)))
                         (case (length (caddr x))
                               ((1) (push (%assure-defgeneric y '(x)) l))
                               ((2) (push (%assure-defgeneric y '(x y)) l))
                               (t   (error "defclass: syntax error ~s." x))))))
        l))

;;; Common slot initforms.

(defun %zero-initform  () 0)
(defun %one-initform   () 1)
(defun %false-initform () nil)
(defun %true-initform  () t)

(defun %compute-initform (initform)
   ;; Returns  the  appropriate  init  function  text  for  the  given
   ;; initform. Does some minimal code sharing for very common cases.
   (cond
         ((eq initform nil) '(function %false-initform))
         ((eq initform t)   '(function %true-initform))
         ((eq initform 0)   '(function %zero-initform))
         ((eq initform 1)   '(function %one-initform))
         ((and (consp initform) (null (cdr initform)))
          ;; It's a no argument function call: (xxx)
          `(function ,(car initform)))
         (t `(lambda () ,initform))))

;;; Add feature

(provide "defclass")
