;;;; -*-Mode:LISP; Package:LISP; Base:10; Syntax:ISLISP -*-
;;;; Title:     defstruct.lsp
;;;; Author:    C. Jullien
;;;; License:   New BSD license
;;;; CVS:       "$Id: defstruc.lsp,v 1.27 2010-06-20 12:43:43 jullien Exp $"

;;; DEFSTRUCT package.

;;; CommonLisp convention is used,  (defstruct foo x y) will generate
;;; foo-x,  foo-y,  foo-p, copy-foo and make-foo functions and :x and
;;; :y  keywords.

(require "setf")

(in-package "openlisp")

(export '("defstruct" "structurep" "make"))

(defpackage "defstruct"
   (:use    "openlisp"))

(in-package "defstruct")

;;; The DEFSTRUCT macro.

(defmacro defstruct (name &rest slots)
   ;; driver to real defstruct expander
   (%expand-defstruct name slots))

;;; User interface

(defmacro structurep (struct)
   ;; the structure predicate
  `(instancep ,struct (class <standard-structure>)))

(defun make (name &rest keys)
   ;; generic structure object creation.
   (let ((cl (find-class name))
         (obj nil))
        (setf obj (system::allocate-object cl))
        (if (consp obj)
            (system::default-initialize-object obj keys cl)
            (initialize-object
                       (system::allocate-object (find-class name)) keys))))

;;; Helper functions

(defun %expand-defstruct (name slots)
   ;; define a new structure
   (let ((initform  (mapcar #'%compute-slot-init-value slots))
         (index     -1)
         (options   ())
         (std-name  ())
         (predicate '%use-default-name)
         (conc-name '%use-default-name)
         (make      '%use-default-name)
         (copy      '%use-default-name)
         (initarg   ())
         (struct    ())
         (printer   ())
         (boa-make  ())
         (unique    ())
         (named     ())
         (type      ())
         (super     '<standard-structure>)
         (slot      ()))
        (when (consp name)
              ;; CLtL   syntax,   check   for   :include,  :predicate,
              ;; :constuctor, :print-function, :copier and :conc-name.
              ;; Ignore other options.
              (setq options (cdr name))
              (setq name    (car name))
              (if (not (symbolp name))
                  (error "defstruct: structure name is not a symbol ~s.~%"
                         name))
              (for ((option options (cdr option)))
                   ((null option))
                   (let ((key nil)
                         (val nil))
                        (cond
                              ((consp (car option))
                               (setf key (caar option))
                               (setf val (cadar option)))
                              (t
                               (setf key (car option))))
                        (case key
                              ((:INCLUDE)
                               (if (not (symbolp val))
                                   (error "defstruct: not a symbol ~a~%" val))
                               (setq super val)
                               (setq initform
                                     (append
                                        (%update-inherited-initforms
                                           (system::compute-class-slots     val)
                                           (system::compute-class-initforms val)
                                           (cddar option))
                                        initform))
                               (setq slots
                                     (append
                                        (system::compute-class-slots val)
                                        slots)))
                              ((:CONSTRUCTOR)
                               (setq make val)
                               ;; boa-constructor test.
                               (when (and (consp (cddar option))
                                          (consp (caddar option)))
                                     (setq boa-make (caddar option))))
                              ((:PREDICATE)
                               (setq predicate val))
                              ((:CONC-NAME)
                               (setq conc-name val))
                              ((:COPIER)
                               (setq copy      val))
                              ((:PRINT-FUNCTION)
                               (setq printer   val))
                              ((:TYPE)
                               (setq predicate nil)
                               (setq type      val))
                              ((:NAMED)
                               (setq named     t))
                              (t
                               (error "defstruct: invalid option ~a~%" key))))))
        (when (and type printer)
              (error "defstruct: :print-function can't be used with :type.~%"))
        (when (and type predicate)
              (error "defstruct: :predicate can't be used with :type.~%"))
        (when (and type named)
              (error "defstruct: :named option not yet supported.~%"))
        ;; undecorated structure name.
        (setq std-name (%normalize-name name))
        (if (eq predicate '%use-default-name)
            (setq predicate '-p))
        (if (eq conc-name '%use-default-name)
            (setq conc-name (concat std-name "-")))
        (if (eq make '%use-default-name)
            (setq make 'make-))
        (if (eq copy '%use-default-name)
            (setq copy 'copy-))
        (setq slot (mapcar (lambda (x)
                             (let ((read-only ())
                                   (slot-name x))
                                  (when (consp x)
                                        (setq slot-name (car x))
                                        (setq read-only (member :read-only x))
                                        (when read-only
                                              (setq read-only
                                                    (cadr read-only)))
                                        (setq x (car x)))
                                  (setq index  (1+ index))
                                  (setq struct (cons x struct))
                                  (if (member slot-name unique)
                                      (error
                                         "defstruct: ~s duplicated slot '~s'~%"
                                         name slot-name)
                                      (setq unique (cons slot-name unique)))
                                  (if read-only
                                      (%make-slot-reader
                                            std-name conc-name type x index)
                                      (%make-slot-accessor
                                            std-name conc-name type x index))))
                           slots))
        (setq initarg (mapcar (lambda (x)
                                 ;; generate keyword for each field.
                                 (system::define-keyword (if (consp x)
                                                             (car x)
                                                             x)))
                               slots))
        (when super
              ;; inherit missing behavior from super
              (unless printer
                      (setq printer `(get-format ',super))))
        (setq struct (nreverse struct))
       `(progn
          (system::internal-build-class ',name
                                        '(,super)
                                        nil
                                        ;; next is for create-class-info
                                        ',struct
                                        ',initform
                                        ',initarg
                                        nil
                                        nil
                                        ',type)
          ,(when make
              (if boa-make
                  `(defun ,(%generate-symbol std-name make :constructor)
                          (&rest key)
                      (boa-constructor
                           (system::allocate-object (class ,name)) key))
                  `(defun ,(%generate-symbol std-name make :constructor)
                          (&rest key)
                      (system::default-initialize-object
                          (system::allocate-object (class ,name))
                          key
                         (class ,name)))))
          ,(when copy
             `(definline ,(%generate-symbol std-name copy :copier) (obj)
                 (if (instancep obj (class ,name))
                     (copy-seq obj)
                     (error "~A is not a '~A' object~%" obj ',name))))
          ,(when predicate
             `(definline ,(%generate-symbol std-name predicate :predicate) (x)
                 (instancep x (class ,name))))
          ,(when printer
                 `(add-format ',name
                              ,(if (consp printer)
                                   ;; assumes lambda
                                   printer
                                   (list 'quote printer))))
          ,@slot
          ',name)))

(defun %normalize-name (sym)
   ;; strip #\< and #\> around the name: <foo> -> foo
   (let ((str (convert sym <string>)))
        (if (eql (elt str 0) #\<)
            (string-trim "<>" str)
            str)))

(defun %generate-symbol (struct conc-name name)
   ;; CommonLisp name is in use -> ship-x
   (case name
         ((:CONSTRUCTOR)
          (if (eq conc-name 'make-)
              (concat conc-name struct)
              conc-name))
         ((:COPIER)
          (if (eq conc-name 'copy-)
              (concat conc-name struct)
              conc-name))
         ((:PREDICATE)
          (if (eq conc-name '-p)
              (concat struct conc-name)
              conc-name))
         ((nil)
          nil)
         (t
          (if conc-name
              ;; generate a prefixed symbol (ship-x)
              (concat conc-name name)
              ;; no prefix must be used.
              name))))

(defun %compute-slot-init-value (slot)
   ;; ignore :read-only and :type options
   (cond
         ((symbolp slot)
          ())
         ((or (atom slot) (not (symbolp (car slot))))
          (error "defstruct: not a symbol ~A~%" slot))
         (t
          (when (consp (cdr slot))
                (when (member :type slot)
                      (format (error-output)
                         "defstruct: ~s :type option is ignored." (car slot)))
                (cadr slot)))))

(defun %update-inherited-initforms (slots initform option)
   (setq initform (copy-seq initform))
   (dolist (x option)
           (if (and (consp x) (consp (cdr x)))
               (let ((pos (position (car x) slots)))
                    (if pos
                        (set-elt (cadr x) initform pos)
                        (error "defstruct: ~a is not an inherited slot.~%"
                               (car x))))
               (error "defstruct: invalid inherited option :~a.~%" x)))
   initform)

(defun %make-slot-accessor (struct conc-name type slot index)
   ;; create  the  access  function  for  a  new slot.
   (let* ((get-slot (%generate-symbol struct conc-name slot))
          (set-slot (concat 'set- get-slot)))
         (if (eq type '<list>)
             `(progn
                 (proclaim '(inline ,get-slot ,set-slot))
                 (defun ,get-slot (obj) (elt obj ,index))
                 (defun ,set-slot (obj val) (set-elt val obj ,index))
                 (defsetf ,get-slot ,set-slot))
             `(progn
                 (proclaim '(inline ,get-slot ,set-slot))
                 (defun ,get-slot (obj) (svref obj ,index))
                 (defun ,set-slot (obj val) (svset val obj ,index))
                 (defsetf ,get-slot ,set-slot)))))

(defun %make-slot-reader (struct conc-name type slot index)
   ;; create  the  reader  function  for  a  new slot.
   (let ((get-slot (%generate-symbol struct conc-name slot)))
         (if (eq type '<list>)
             `(progn
                     (proclaim '(inline ,get-slot))
                     (defun ,get-slot (obj) (elt obj ,index)))
             `(progn
                     (proclaim '(inline ,get-slot))
                     (defun ,get-slot (obj) (svref obj ,index))))))

;;; Add defstruct feature.

(provide "defstruc")
