;;;; -*-Mode:LISP; Package:LISP; Base:10; Syntax:ISLISP -*-
;;;; Title:     sort.lsp
;;;; Author:    C. Jullien
;;;; License:   New BSD license
;;;; CVS:       "$Id: sort.lsp,v 1.48 2018/07/29 13:16:39 jullien Exp $"

;;; SORT:  various sorting functions on sequences.

(defpackage #:sort
   (:use #:openlisp))

(in-package #:sort)

;;;
;;; Special sort if argument is a list.
;;;

(defun %sort-list (fn l len)
   ;; WARNING: l is physically modified.
   (if (< len 2)
       l
       (let ((tmp (div len 2))
             (l2  ())
             (l1  l))
            ;; split l in two sub-lists l1 l2 of about the same size.
            ;; NOTE: l2 and l1 have always at least 1 element.
            (setq l  (nthcdr (1- tmp) l))
            (setq l2 (cdr l))
            (set-cdr () l)
            ;; sort separately the two sub-lists.
            (setq l1 (%sort-list fn l1 tmp))
            (setq l2 (%sort-list fn l2 (- len tmp)))
            ;; fusion two sub-lists
            (when (funcall fn (car l2) (car l1))
                  ;; swap the two lists
                  (psetq l1 l2 l2 l1))
            (setq l l1)
            (while (cdr l1)
                   (when (funcall fn (car l2) (cadr l1))
                         (psetq tmp l2 l2 (cdr l1))
                         (set-cdr tmp l1))
                   (setq l1 (cdr l1)))
            (set-cdr l2 l1)
            l)))

;;;
;;; General Quicksort for any sequence object (CONS, VECTOR, STRING).
;;;

(defun %qsort (pred seq)
   (let ((len (length seq)))
        (if (> len 2)
            (%internal-qsort 0 (1- len) seq pred))
        seq))

(defun %internal-qsort (l r seq pred)
   ;; WARNING: seq is physically modified.
   (let ((i l)
         (j r)
         (x (elt seq (div (- (+ l r) 1) 2)))
         (s ()))
        (while (<= i j)
               (while (and (not (eq (elt seq i) x))
                           (funcall pred (elt seq i) x))
                      (setq i (1+ i)))
               (while (and (not (eq (elt seq j) x))
                           (funcall pred x (elt seq j)))
                      (setq j (1- j)))
               (when (<= i j)
                     ;; swap elements at i and j
                     (setq s (elt seq j))
                     (set-elt (elt seq i) seq j)
                     (set-elt s seq i)
                     (setq i (1+ i))
                     (setq j (1- j))))
        (if (< l j)
            (%internal-qsort l j seq pred))
        (if (< i r)
            (%internal-qsort i r seq pred))))

;;;
;;; Sort specializers
;;;

(defun %make-sort-function (sort-name compare-function)
   (let ((aux (intern (string-append "aux-" (symbol-name sort-name)) :sort)))
       `(progn

           (defun ,sort-name (l)
              ;; function dispatcher depending of sequence type.
              (cond
                    ((null l)
                     nil)
                    ((consp l)
                     (,aux l (length l)))
                    (t
                     (sort::%qsort (symbol-function ',sort-name) l))))

           (defun ,aux (l len)
              ;; WARNING: l is physically modified.
              (if (< len 2)
                  l
                  (let ((tmp (div len 2))
                        (l2  ())
                        (l1  l))
                       ;; split l in two sub-lists l1 l2 of about the same size.
                       ;; NOTE: l2 and l1 have always at least 1 element.
                       (setq l  (nthcdr (1- tmp) l))
                       (setq l2 (cdr l))
                       (set-cdr () l)
                       ;; sort separately the two sub-lists.
                       (setq l1 (,aux l1 tmp))
                       (setq l2 (,aux l2 (- len tmp)))
                       ;; fusion two sub-lists
                       (when (,compare-function (car l2) (car l1))
                             ;; swap the two lists
                             (psetq l1 l2 l2 l1))
                       (setq l l1)
                       (while (cdr l1)
                              (when (,compare-function (car l2) (cadr l1))
                                    (psetq tmp l2 l2 (cdr l1))
                                    (set-cdr tmp l1))
                              (setq l1 (cdr l1)))
                       (set-cdr l2 l1)
                       l)))
           )
    )
)

;;;
;;; Standard predefined sort functions:
;;;

(in-package #:openlisp)

(export '(sort make-specialized-sort-function sortn sortl))

(defun sort (l fn)
   ;; use %sort-list if argument is a list, %qsort otherwise.
   (cond
         ((null l)
          nil)
         ((consp l)
          (sort::%sort-list fn l (length l)))
         (t
          (sort::%qsort fn l))))

(defmacro make-specialized-sort-function (name fn)
   ;; generate a specialized sort function.
   ;; fn must be a symbol.
   (sort::%make-sort-function name fn))

;; Sort numbers

(make-specialized-sort-function sortn <)

;; Sort symbols

(make-specialized-sort-function sortl string<)

#|

;; benchs and checks.

(defglobal xx ())

(dotimes (c 500000)
   (setf xx (cons (random 32767) xx)))

(defun sort-check ()
   (let ((x ())
         (l ()))
        (dotimes (c 500000)
                 (setf x (cons (random 32767) x)))
        (setq l (sortn x))
        (while (and l (cdr l) (<= (car l) (cadr l)))
               (setf l (cdr l)))
        (< (length l) 2)))

|#

(provide "sort")
