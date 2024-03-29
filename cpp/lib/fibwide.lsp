;;;; -*-Mode: LISP; Package:LISP; Base:10; Syntax:ISLISP; Encoding: utf-16 -*-
;;;; Date:       2008/01/03
;;;; Title:      fibwide.lsp
;;;; Author:     C. Jullien
;;;; CVS:        $Id$

;;; Famous フィボナッチ (Fibonacci) Benchmark with Japanese characters

(defconstant fib-symbol 'フィボナッチ)
(defconstant fib-string "フィボナッチ")

;;; With Chinese characters: 费班纳赛 (fei ban nei ji)
;;; * Fibonacci method 费班纳赛法
;;; * Fibonacci number 费班纳赛数
;;; * Fibonacci search 费班纳赛检索

(defun fib (x)
   (labels ((フィボナッチ (n)
               ;; Standard definition
               (cond ((= n 1) 1)
                     ((= n 2) 1)
                     (t (+ (フィボナッチ (1- n)) (フィボナッチ (- n 2)))))))
           (フィボナッチ x)))
