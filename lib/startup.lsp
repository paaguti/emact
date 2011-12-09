;;;; -*-Mode:LISP; Package:LISP; Base:10; Syntax:ISLISP -*-
;;;; Title:     startup.lsp
;;;; Author:    C. Jullien
;;;; License:   New BSD license
;;;; CVS:       "$Id: startup.lsp,v 1.66 2010-06-20 12:43:43 jullien Exp $"

;;;
;;; Startup for standard environnment.
;;;

(in-package "openlisp")

;;;
;;; Define  backconstant as the very first function in startup.lsp so
;;; that we can use its function type to know if this file is running
;;; in compiled or in interpreted mode (see system::*compiled-env* below).
;;;

(defun system::backconstant (s)
   ;; t if expression is like (QUOTE xxx).
   ;; NOTE, this function is also used to detect if we are in compiled mode.
   (and (consp s) (eq (car s) 'quote) (null (cddr s))))

(defglobal system::*compiled-env*
   ;; t when OpenLisp library is in compiled mode.
   (if (member (function-type 'system::backconstant) '(subr1 subrn))
       t))

(if (and (not *quiet-mode*) (not system::*compiled-env*))
    (let ((info (machine-info))
          (st   (standard-output)))
         (format st ";; Copyright (c) 1988-2010.~%")
         (format st ";; System '~a' (~dbit, ~a CPU) on '~a', ~a.~%"
                 (system-name)
                 (* (system::pointer-size) 8)
                 (if (> (length info) 24) (svref info 24) 1) ;; nb CPUs
                 (svref info 3) ;; machine name
                 (if (= (system::character-size) 1) "ASCII" "UNICODE"))
         (format (standard-output) ";; Reading startup ..~%")))

;;;
;;; ISLISP Level : ISO/IEC 13816:2007(E) ISLISP Programing Language.
;;;

(export '("*islisp-version*"))

(defconstant *islisp-version* 200710) ;; 10 October 2007

;;;
;;; Installation directory
;;;

(export '("*system-directory*" "*system-path*"))

(defdynamic *system-directory*
   ;; can be something like "/usr/local/openlisp"
   (if (and (boundp '*system-directory*) (dynamic *system-directory*))
       (dynamic *system-directory*)
       (if (getenv "OPENLISP")
           (getenv "OPENLISP")
           ".")))

(defdynamic *system-path*
   ;; list of default directories.
   (cons "."
         (mapcar (lambda (x)
                    (string-append (dynamic *system-directory*) x))
                 '("/lib" "/net" "/contrib" "/bench" "/tst"))))

;;;
;;; We need backquote macro early in bootstrap process.
;;;

(defun system::backquotify (s)
   ;; backquotify an expression.
   (cond
         ((null s)
          ())
         ((vectorp s)
          ;; ISLISP section 8: ` or quasiquote constructs a list structure.
          ;; This case is CLtL compatible OpenLisp extension.
          (list 'CONVERT
                (system::backquotify (convert s <list>))
                '<general-vector>))
         ((atom s)
          ;; `s -> 's
          (list 'QUOTE s))
         ((eq (car s) 'system::|,|)
          ;; `,s -> s
          (cdr s))
         ((or (eq (car s) 'system::|,@|)
              (eq (car s) 'system::|,.|))
          (error "` syntax error: ~s~%" s))
         ((and (consp (car s))
               (eq (caar s) 'system::|,@|))
          (let ((a (cdar s))
                (d (system::backquotify (cdr s))))
               (if d
                   (list 'APPEND a d)
                   a)))
         ((and (consp (car s))
               (eq (caar s) 'system::|,.|))
          (if (cdr s)
              (list 'openlisp::NCONC (cdar s) (system::backquotify (cdr s)))
              (cdar s)))
         (t
          (let ((a (system::backquotify (car s)))
                (d (system::backquotify (cdr s))))
               (cond
                     ((null d)
                      (if (system::backconstant a)
                          ;; (cons 'a ()) -> '(a)
                          (list 'QUOTE (list (cadr a)))
                          ;; (cons a ())  -> (list a)
                          (list 'LIST a)))
                     ((and (system::backconstant a) (system::backconstant d))
                      ;; (cons 'a 'd) -> '(a . d)
                      (list 'QUOTE (cons (cadr a) (cadr d))))
                     ((and (consp d) (eq (car d) 'list))
                      ;; (cons a (list d1 .. dN)) -> (list a d1 .. dN)
                      (cons 'LIST (cons a (cdr d))))
                     (t
                      (list 'CONS a d)))))))

(defun system::backquote-dispatcher (stream char)
   (case char
         ((#\`)
          (system::backquotify (read stream)))
         ((#\@)
          (list 'system::|@| (read stream)))
         ((#\,)
          (case (preview-char stream)
              ((#\@) (read-char stream)
                     (cons 'system::|,@| (read stream)))
              ((#\.) (read-char stream)
                     (cons 'system::|,.| (read stream)))
              (t     (cons 'system::|,|  (read stream)))))
         (t
          (error "backquote-dispatcher: unsupported character ~s." char))))

(set-macro-character #\` #'system::backquote-dispatcher)
(set-macro-character #\, #'system::backquote-dispatcher)
(set-macro-character #\@ #'system::backquote-dispatcher)

(export '("macroexpand" "macroexpand-1" "macroexpand-all"))

(defun macroexpand-1 (x)
   (if (and (consp x) (symbolp (car x)) (macro-function (car x)))
       (funcall (macro-function (car x)) x)
       x))

(defun macroexpand (x)
   (while (and (consp x) (symbolp (car x)) (macro-function (car x)))
          (setq x (funcall (macro-function (car x)) x)))
   x)

(defun macroexpand-all (x)
   (cond
         ((atom x)
          x)
         ((and (symbolp (car x)) (macro-function (car x)))
          (macroexpand-all (funcall (macro-function (car x)) x)))
         ((and (symbolp (car x)) (member (car x) '(case case-using)))
          (cons (car x)
                (cons (macroexpand-all (cadr x))
                      (mapcar (lambda (l)
                                 (cons (car l) (macroexpand-all (cdr l))))
                              (cddr x)))))
         (t
          (let ((l ()))
               (while (consp x)
                      (setq l (cons (macroexpand-all (car x)) l))
                      (setq x (cdr x)))
               (prog1 (nreverse l)
                      (set-cdr x l))))))

;;;
;;; The eval-when Special Form written as macro
;;;

(export '("eval-when"))

(defmacro eval-when (situations &rest body)
   ;; conditionally execute code from a situation list.
   ;; Valid situation are COMPILE EVAL LOAD
   (if (member (dynamic *situation*) situations)
      `(progn ,@body)))

;;;
;;; Declarations
;;;

(export '("proclaim" "declaim" "declare" "inline" "definline"))

(defun proclaim (decl-spec)
   (identity decl-spec))

(defmacro declare (decl-spec)
   `(identity '(declare ,decl-spec)))

(defmacro declaim (&rest decl-list)
   `(eval-when (compile load eval)
       ,@(mapcar (lambda (decl-spec) `(proclaim ',decl-spec)) decl-list)))

(defmacro definline (name args &rest body)
  `(progn
          (declaim (inline ,name))
          (defun ,name ,args ,@body)))

;;;
;;; Stream extensions
;;;

(export '("with-output-to-string" "with-input-from-string"))

(defmacro with-output-to-string (stream &rest body)
   `(let ((,(car stream) (create-string-output-stream)))
         (unwind-protect (progn ,@body)
                         (close ,(car stream)))))

(defmacro with-input-from-string (stream &rest body)
   `(let ((,(car stream) (create-string-input-stream ,(cadr stream))))
         (unwind-protect (progn ,@body)
                         (close ,(car stream)))))

(export '("with-open-input-pipe" "with-open-output-pipe"))

(defmacro with-open-input-pipe (stream &rest body)
   `(let ((,(car stream) (open-input-pipe ,@(cdr stream))))
         (unwind-protect (progn ,@body)
                         (close ,(car stream)))))

(defmacro with-open-output-pipe (stream &rest body)
   `(let ((,(car stream) (open-output-pipe ,@(cdr stream))))
         (unwind-protect (progn ,@body)
                         (close ,(car stream)))))

;;;
;;; Define generic functions for CREATE and INITIALIZE-OBJECT
;;;

(defgeneric create (cl &rest init)
   ;; Create is used by both 'defstruct' and 'defclass' define forms.
   (:generic-function-class <generic-function>)
   (:method ((cl <object>) &rest init)
            (identity init)
            (error "CREATE : not a <standard-class> object ~a~%" cl))
   (:method ((cl <standard-class>) &rest init)
            (let ((obj (system::allocate-object cl)))
                 (initialize-object obj init)
                 obj)))

(defgeneric initialize-object (instance initvalues)
   (:generic-function-class <generic-function>)
   (:method ((instance <standard-object>) initvalues)
            ;; ISLISP standard object
            (system::default-initialize-object instance initvalues nil))
   (:method ((instance <standard-structure>) initvalues)
            ;; CLtL structure
            (system::default-initialize-object instance initvalues nil)))

;;;
;;; Interface to class information.
;;;

(defglobal system::*compiler-struct-ht*
    ;; Hash table to store computed DEF<CLASS/DEFSTRUCT.
    (make-hash-table :test #'eq))

(defun system::internal-build-class (name sc-list abstractp &rest info)
   ;; create a new class object for 'name'.
   (cond
         ((eq (dynamic *situation*) 'compile)
          (setf (gethash name system::*compiler-struct-ht*)
                (apply #'system::create-class-info info)))
         (t
          ;; remove any previous associated format.
          (rem-format name)
          ;; add a new class to the object system.
          (system::create-class name sc-list abstractp)
          ;; define the class-info for this class.
          (system::set-class-info (apply #'system::create-class-info info)
                                  (find-class name))
          t)))

(defun system::get-class-info (name)
   ;; returns class vector information.
   (if (eq (dynamic *situation*) 'compile)
       (or (gethash name system::*compiler-struct-ht*) (find-class name))
       (find-class name)))

(defun system::compute-class-size (name)
   ;; returns slot length for class 'name'.
   (svref (system::get-class-info name) 4)) ;; class-size

(defun system::compute-class-slots (name)
   ;; returns slot names for class 'name'.
   (svref (system::get-class-info name) 5)) ;; class-slots

(defun system::compute-class-initforms (name)
   ;; returns slot initforms for class 'name'.
   (svref (system::get-class-info name) 6)) ;; class-initforms

(defun system::compute-class-initargs (name)
   ;; returns slot initargs for class 'name'.
   (svref (system::get-class-info name) 7)) ;; class-initargs

(defun system::compute-class-descriptions (name)
   ;; returns info descriptions for class 'name'.
   (svref (system::get-class-info name) 8)) ;; class-descriptions

(defun system::compute-class-super (name)
   ;; returns super for class 'name'.
   (svref (system::get-class-info name) 9)) ;; class-super

(defun system::compute-class-implementation-type (name)
   ;; returns implementation type.
   (svref (system::get-class-info name) 10)) ;; class-implementation-type

;;;
;;; Control structures (extensions).
;;;

(export '("when" "unless" "dotimes" "dolist" "until" "repeat" "typecase"))

(defmacro when (test &rest body)
   (if (and (consp body) (null (cdr body)))
      `(if ,test ,(car body))
      `(if ,test (progn ,@body))))

(defmacro unless (test &rest body)
   (if (and (consp body) (null (cdr body)))
      `(if ,test () ,(car body))
      `(if ,test () (progn ,@body))))

(defmacro dotimes (%loop &rest %body)
   ;; (DOTIMES (var count [res]) exp1 .. expN)
   (if (constantp (cadr %loop))
       `(FOR ((,(car %loop) 0 (1+ ,(car %loop))))
             ((>= ,(car %loop) ,(cadr %loop))
              ,(if (consp (cddr %loop)) (caddr %loop)))
             ,@%body)
       (let ((g (gensym)))
            `(FOR ((,(car %loop) 0 (1+ ,(car %loop)))
                   (,g ,(cadr %loop)))
                  ((>= ,(car %loop) ,g)
                   ,(if (consp (cddr %loop)) (caddr %loop)))
                  ,@%body))))

(defmacro dolist (%loop &rest %body)
   ;; (DOLIST (var listform [res]) exp1 .. expN)
   (let ((%dolist-var (gensym)))
        `(FOR ((,%dolist-var ,(cadr %loop) (cdr ,%dolist-var))
               (,(car %loop) ()))
              ((NULL ,%dolist-var) ,(if (consp (cddr %loop)) (caddr %loop)))
              (SETQ ,(car %loop) (car ,%dolist-var))
              ,@%body)))

(defmacro until (test &rest body)
  `(progn (while (null ,test) ,@body) t))

(defmacro repeat (n &rest body)
   (let ((repeat (gensym)))
       `(for ((,repeat (1- ,n) (1- ,repeat)))
             ((minusp ,repeat) t)
             ,@body)))

(defmacro typecase (type &rest body)
   `(case (class-name (class-of ,type)) ,@body))

;;;
;;; Environment
;;;

(export '("defvar" "defkeyword" "get-function-name"))

(synonym 'defvar 'defdynamic)

(defmacro defkeyword (l)
   `(system::define-keyword ',l))

(defun get-function-name (fn)
   ;; returns the function name (if any) associated to a function object.
   (unless (functionp fn)
           (error "get-function-name: ~a is not a function.~%" fn))
   (let ((name nil))
        (some (lambda (x)
                 (and (fboundp x)
                      (eq (symbol-function x) fn)
                      (setq name x)))
              (oblist))
        (cond
              (name name)
              ((eq (function-type fn) 'expr) 'lambda)
              (t nil))))

;;;
;;; Packages
;;;

(export '("defpackage" "do-symbols"))

(defmacro defpackage (name &rest options)
   ;; The defpackage macro.
   (unless (or (symbolp name) (stringp name))
           (error "DEFPACKAGE: invalid package name ~a~%" name))
   (let ((exp       nil)
         (p         nil)
         (nicknames nil)
         (l         nil))
        (setq name (string name))
        (dolist (x options)
                (if (not (consp x))
                    (error "DEFPACKAGE: invalid list option ~a~%" x))
                (case (car x)
                      ((:SIZE)
                       ;; Ignore size.
                       (assure <integer> (cadr x)))
                      ((:SHADOW)
                       ;; Symbols   with   the  specified  names  are
                       ;; created  as  shadows  in  the package being
                       ;; defined.
                       (if (atom (cdr x))
                           (setq l (list (cdr x)))
                           (setq l (cdr x)))
                       (push `(SHADOW ',(mapcar #'string l) ,name) exp))
                      ((:USE)
                       ;; The  package being defined is made to "use"
                       ;; the packages specified.
                       (push 
                             `(USE-PACKAGE ',(mapcar #'string (cdr x)) ,name)
                              exp))
                      ((:IMPORT-FROM)
                       ;; Symbols   with   the  specified  names  are
                       ;; located  in  the  specified package.  These
                       ;; symbols   are  imported  into  the  package
                       ;; being defined.
                       (setq p (string (cadr x)))
                       (if (atom (cddr x))
                           (setq l (list (cddr x)))
                           (setq l (cddr x)))
                       (push
                             `(IMPORT (LIST ,@(mapcar (lambda (s)
                                                        `(INTERN ,s ,p))
                                                      (mapcar #'string l)))
                                      ,name)
                              exp))
                      ((:NICKNAMES)
                       ;; The  specified  names  become  nicknames of
                       ;; the package being defined.
                       (setf nicknames (cdr x)))
                      ((:INTERN)
                       ;; Symbols   with   the  specified  names  are
                       ;; located  or  created  in  the package being
                       ;; defined, just as with the function intern.
                       (dolist (s (cdr x))
                               (push `(INTERN ,(string s) ,name) exp)))
                      ((:EXPORT)
                       ;; Symbols   with   the  specified  names  are
                       ;; located  or  created  in  the package being
                       ;; defined  and  then  exported,  just as with
                       ;; the function export.
                       (push `(EXPORT ',(cdr x) ,name) exp))
                      (t
                       (error "DEFPACKAGE: invalid option ~a~%" x))))
        `(PROGN (CREATE-PACKAGE ,name ',nicknames)
                ,@(nreverse exp))))

(defmacro do-symbols (loop &rest body)
   ;; (DO-SYMBOLS (var [init [res]]) exp1 .. expN)
   ;; do-symbols  provides straightforward iteration over the symbols
   ;; of  a  package.  The  body  is  performed  once for each symbol
   ;; accessible  in  the package,  in no particular order,  with the
   ;; variable  var  bound to the symbol.  Then result-form (a single
   ;; form,  not  an implicit progn) is evaluated,  and the result is
   ;; the value of the do-symbols form.
   (let ((var (nth 0 loop))
         (pkg (nth 1 loop))
         (res (nth 2 loop))
         (it  (gensym)))
       `(for ((,it ,(if pkg `(oblist ',pkg) '(oblist)) (cdr ,it))
              (,var () ()))
             ((null ,it) ,res)
             (setq ,var (car ,it))
             ,@body)))

;;;
;;; Missing sequence functions from standard lib
;;;

(export '("remove-duplicates" "delete-duplicates"))

(defun remove-duplicates (seq)
   ;; This is a dispatcher for the correct algorithm.
   ;; Sequence is not physically modified.
   (cond
         ((consp seq)
          ;; Return a copy of sequence with redundant elements removed.
          (system::delete-duplicates-list (copy-seq seq)))
         ((or (vectorp seq) (stringp seq))
          (system::delete-duplicates-array seq))
         (t seq)))

(defun delete-duplicates (seq)
   ;; This is a dispatcher for the correct algorithm.
   (cond
         ((consp seq)
          (system::delete-duplicates-list seq))
         ((or (vectorp seq) (stringp seq))
          (system::delete-duplicates-array seq))
         (t seq)))

(defun system::delete-duplicates-list (list)
   ;; This is a specialized algorithm for lists.
   ;; Original list may be modified.
   (cond
         ((null list)
          nil)
         ((position (car list) (cdr list))
          (let ((res (system::delete-duplicates-list (cdr list))))
               (setf (car list) (car res))
               (setf (cdr list) (cdr res))))
         (t
          (setf (cdr list) (system::delete-duplicates-list (cdr list)))))
   list)

(defun system::delete-duplicates-array (seq)
   ;; This is a specialized algorithm for arrays.
   (cond
         ((> (length seq) 1)
          (let ((rest (subseq seq 1)))
               (cond
                     ((position (elt seq 0) rest)
                      (system::delete-duplicates-array rest))
                     (t
                      (append (subseq seq 0 1)
                              (system::delete-duplicates-array rest))))))
         (t seq)))

;;;
;;; Feature functions
;;;

(export '("list-features" "add-feature" "rem-feature" "featurep"))

(defun list-features ()
   *modules*)

(defun add-feature (package)
   (unless (featurep package)
           (unless (constantp package)
                   (set-symbol-global package package))
           (setq *modules* (cons (string package) *modules*))))

(defun rem-feature (package)
   (setq *modules* (remove (string package) *modules*)))

(defun featurep (package)
   (member (string package) *modules* :test #'equal))

;;;
;;; Standard functions (in ../lib/*)
;;;

(export '("autoload"))

(defglobal system::*autoload-required*
    ;; A-list (fn . "file") used by the compiler
    ())

(defmacro autoload (file &rest fun)
   `(progn
           ,@(mapcar (lambda (f)
                       ;; remember this function to help compiler
                       (setq system::*autoload-required*
                             (cons (cons f file) system::*autoload-required*))
                       (export f "openlisp")
                       `(defmacro ,f (&rest body)
                           (require ,file)
                           (cons ',f body)))
                     fun)
           ',file))

(autoload "defclass" defclass)
(autoload "flet"     flet labels)
(autoload "setf"     setf defsetf incf decf push pop pushnew)

;;; Define report-condition generic function.

(defgeneric report-condition (condition stream)
   (:method ((condition <error>) (stream <stream>))
            (if (eq (svref condition 2) '<unbound-variable>)
                (format stream "** ~a : ~a~%"
                        (svref condition 1)
                        (svref condition 3))
                (format stream "** ~a : ~a : ~a~%"
                        (svref condition 1)
                        (svref condition 3)
                        (svref condition 2))))
   (:method ((condition <simple-error>) (stream <stream>))
            (when (svref condition 0)
                  (format-object stream (svref condition 0) nil))
            (apply #'format stream (svref condition 4) (svref condition 5)))
   (:method ((condition <serious-condition>) (stream <stream>))
            (format stream "** ~a : ~a~%"
                    (svref condition 1)
                    (svref condition 3))))

;;;
;;; Loader for standard environment.
;;;

(export '("libload" "load-stdlib"))

(defun libload (file &rest verbose)
   ;; load a library file (from *system-path*)
   (setq file (convert file <string>))
   (let ((f (search-in-path file)))
        (if f
            (load f verbose)
            (error "LIBLOAD: invalid filename '~a'~%" file))))

(defun load-stdlib (compiled &rest verbose)
   ;; Load standard environment. Order is important if compiled = t.
   (let ((quiet *quiet-mode*))
        (setq *quiet-mode* t)
        (mapc (lambda (file)
                 (load (string-append (getenv "OPENLISP")
                                      "/lib/"
                                      (string file)
                                      (if compiled ".lap" ".lsp"))
                       verbose))
              '(;; "startup"
                "opcode"
                "loader"
                "codegen"
                "mangling"
                "compiler"
                "setf"
                "defstruc"
                "defclass"
                "datetime"))
        (setq *quiet-mode* quiet)))

(set-macro-character #\control-l
     ;; to load file
     (lambda (stream char) `(libload ',(read-line stream))))

#-(eq (system-name) 'mvs)
(set-macro-character #\control-t
     ;; to launch a test file.
     (lambda (stream char)
        `(run-test ,(string-append (getenv "OPENLISP")
                                   "/tst/" (read-line stream) ".lsp"))))

;;;
;;; Shell argument $0, $1, .. $n. $# = arg. count.
;;;

(export '("shift"))

(defglobal system::*command-shift* 0)

(defun system::argument-dispatcher (stream char)
   (let ((c (preview-char stream)))
        (identity char)
        (case c
              ((#\0)
               ;; program name.
               (read-char stream)
              '(line-argument 0))
              ((#\*)
               ;; compute as a vector
               (read-char stream)
              '(subseq (line-argument) (1+ system::*command-shift*)))
              ((#\@)
               ;; compute as a list
               (read-char stream)
              '(convert (subseq (line-argument) (1+ system::*command-shift*))
               <list>))
              ((#\#)
               ;; compute the number of arguments
               (read-char stream)
              `(- ,(1- (length (line-argument))) system::*command-shift*))
              ((#\$)
               ;; compute process ID may be 1000 if getpid is not available.
               (read-char stream)
               (getpid))
              (t
               ;; returns nth argument
              `(line-argument (+ ,(read stream) system::*command-shift*))))))

(set-macro-character #\$ #'system::argument-dispatcher)

(defun shift ()
   ;; shift command line argument.
   (if (< system::*command-shift* (1- (length (line-argument))))
       (setq system::*command-shift* (1+ system::*command-shift*))
       (error "shift: no more argument.~%")))

;;;
;;; Extended format
;;;

(export '("add-format" "rem-format" "get-format" "format-user-type"))

(defglobal system::*format-table* (make-hash-table :test #'eq))

(defun add-format (type fn)
   (when fn
         (puthash type system::*format-table* fn)))

(defun rem-format (type)
   (remhash type system::*format-table*))

(defun get-format (type)
   (gethash type system::*format-table*))

(defun format-user-type (obj st level)
   ;; When a structure of this type is to be printed, the function is
   ;; called on three arguments: the structure to be printed, a stream
   ;; to print to, and an integer indicating the current depth (to be
   ;; compared against *print-level*). The printing function should
   ;; observe the values of such printer-control variables as
   ;; *print-escape*.
   (let ((fn (gethash (class-name (class-of obj))
                      system::*format-table*
                      #'default-format-object)))
        (if (and fn (symbolp fn))
            (setq fn (symbol-function fn)))
        (if (not (functionp fn))
            (error "FORMAT-USER-TYPE: undefined function ~a~%" fn))
        (funcall fn obj st level))
   obj)

;;;
;;; Extented functions (in ./lib/)
;;;

(autoload "apropos"  apropos lhoblist)
(autoload "codegen"  ccompile)
(autoload "compiler" compile compile-file cp)
(autoload "datetime" current-date date-parse
                     make-date date= date/= date> date>= date< date<=)
(autoload "defstruc" defstruct make)
(autoload "describe" describe)
(autoload "disasm"   disassemble dump-lap)
(autoload "extern"   external-module)
(autoload "gcstats"  room)
(autoload "loader"   lap-loader lap-require)
(autoload "logfile"  log-format)
(autoload "mine"     mine mine-toplevel)
(autoload "mvalues"  values values-list multiple-value-setq)
(autoload "pretty"   pretty pprint)
(autoload "profile"  profile profile-all profile-log)
(autoload "runtest"  run-test)
(autoload "sort"     sort sortl sortn make-specialized-sort-function)
(autoload "sysinfo"  system-info)
(autoload "trace"    trace untrace)

;;;
;;; Tools and demos (in ./contrib/)
;;;

(set-macro-character #\control-v
     (lambda (stream char) (pretty (read stream)) t))

(unless (fboundp 'initty)
        (autoload "virtty" initty typrologue))

(when (fboundp 'edit)
      (defglobal system::*edit-name* "scratch")
      (add-feature 'edit))

(when (member (system-name) '(windows pocket-pc))
      (defun edit (file)
         (mine-toplevel (list file))))


#+(fboundp 'comline)
(progn
       (export '("spawn"))

       (unless (fboundp 'edit)
               (defun edit (&rest file)
                  (when file
                        (setq system::*edit-name* (search-in-path (car file))))
                  (comline (string-append (or (getenv "EDITOR") "emacs")
                                          " "
                                          system::*edit-name*))
                  system::*edit-name*))

       (set-macro-character #\!
          (lambda (stream char)
             (let ((cmd (string-trim #(#\Space #\Tab) (read-line stream))))
                  (eq (comline cmd) 0))))

       (defmacro spawn (cmd &rest args)
          `(comline (format () ,cmd ,@args)))
)

#+(or (fboundp 'comline) (fboundp 'edit))
(progn
       (defglobal system::*edit-name* "scratch.lsp")

#-(eq (system-name) 'mvs)
       (set-macro-character #\control-e
            (lambda (stream char)
               (if (member (preview-char stream) '(#\newline #\carriage-return))
                   (progn
                          (read-char stream)
                          (edit system::*edit-name*))
                   (edit (setq system::*edit-name* (string (read stream)))))
               (format (standard-output) ";; Edito cum Emacis, ergo sum.~%")
               system::*edit-name*))

       (defun ed (&rest name)
          (when name (setq name (car name)))
          (cond
                ((null name)
                 (edit system::*edit-name*))
                ((stringp name)
                 (edit (setq system::*edit-name* name)))
                ((symbolp name)
                 (let ((file "scratch.lsp"))
                      (with-open-output-file (st file)
                            (with-standard-output st
                                  (format st ";; ~a definition :~%~%" name)
                                  (pretty name)))
                      (edit (setq system::*edit-name* file))
                      (load file)
                      (delete-file file)))))
)

;;;
;;; Debugger
;;;

(export '("debug" "break"))

(defun debug (flag)
   ;; [de]activate the internal debugger on errors.
   (require "pretty")
   (when (featurep "lap")
         (require "disasm"))
   (system::internal-debug flag))

(defun break (msg &rest args)
   ;; display a message and directly goes to the debugger.
   (debug t)
   (apply #'cerror msg args))

;;;
;;; Socket
;;;

(export '("with-client-socket" "with-server-socket"))

(defmacro with-client-socket (socket &rest body)
   `(let ((,(car socket) (socket)))
         (when ,(car socket)
               (unwind-protect (when ,`(connect ,@socket)
                                     ,@body)
                               (close ,(car socket))))))

(defmacro with-server-socket (socket &rest body)
   `(let ((,(car socket) (socket)))
         (when ,(car socket)
               (unwind-protect (when ,`(listen ,@socket)
                                     ,@body)
                               (close ,(car socket))))))

;;;
;;; SQL interface
;;;

(when (fboundp 'dbexecute)
      (provide "sql"))

;;; Customization (those are default values)

(set-dynamic "? " *prompt*)                     ;; default prompt ?
(set-dynamic t    *read-level*)                 ;; print read level 2>
(set-dynamic 2    *warning-level*)              ;; OpenLisp warning level [0, 2]
(set-dynamic nil  *print-nil-as-list*)          ;; nil or ()
(set-dynamic 10   *print-base*)                 ;; integer default print base
(set-dynamic t    *print-escape*)               ;; control printer
(set-dynamic 256  *print-length*)               ;; max. list to print
(set-dynamic 256  *print-level*)                ;; max. recursive print level
(set-dynamic t    *print-stat*)                 ;; print evaluation time
(set-dynamic nil  *print-packages*)             ;; always print packages.
(set-dynamic nil  *continuable-system-error*)   ;; continuable errors

(when (featurep "emacs-pipe")
      ;; redefine some values when inside emacs.
      (set-dynamic "? " *prompt*)
      (set-dynamic nil  *read-level*)
      t)

(if (and (not *quiet-mode*) (not system::*compiled-env*))
    (format (standard-output)
            (if (member (system-name) '(pocket-pc))
                ";; OpenLisp is back again!~%"
                ";; God thank you, OpenLisp is back again!~%")))

(when (and system::*compiled-env* (probe-file "autorun.lsp"))
      (load "autorun.lsp"))

(when (member (system-name) '(pocket-pc))
      (change-directory (getenv "OPENLISP")))

(in-package "user")

;;;
;;; End of startup bootstrap
;;;
