;;; MIT License
;;; 
;;; Copyright (c) 2022 kzO8Xekb
;;; 
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.

;;; util.lisp
;;; 各種ユーティリティ関数・マクロ関数

(in-package :win-app-driver)

;;-------------------------------------------------------------
;; Utility Macros 
;;-------------------------------------------------------------
(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))

(defmacro mac-all (expr)
  `(pprint (trivial-macroexpand-all:macroexpand-all ',expr)))

(lol:defmacro! mac-eval (expr)
  `(let
     ((,g!expanded (mac ,expr)))
     (eval ,g!expanded)))

(lol:defmacro! mac-all-eval (expr)
  `(let
     ((,g!expanded (mac-all ,expr)))
     (eval ,g!expanded)))

;; with-gensyms
(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro aand (&rest args)
  (cond ((null args) t)
        ((null (rest args)) (first args))
        (t `(aif ,(first args) (aand ,@(rest args))))))

(defmacro acond (&rest clauses)
  (if (null clauses)
    nil
    (let ((cl1 (first clauses))
          (sym (gensym)))
      `(let ((,sym ,(first cl1)))
         (if ,sym
           (let ((it ,sym)) ,@(rest cl1))
           (acond ,@(rest clauses)))))))

(defmacro alambda (lambda-list &body body)
  "anaphoric lambda is able to define of recursive funcition."
  `(labels ((self ,lambda-list
                  ,@body))
     #'self))

(defmacro loop-cond (&body forms)
  (if (null forms) nil
    `(loop
       (cond ,@(loop for f in forms collect `(,@f))))))

(defmacro flet* (fs &body body)
  (if (null body) nil
    (do
      ((result `(progn ,@body) `(flet ,@(cons `(,itr) `(,result))))
       (connect (cdr (reverse fs)) (cdr connect))
       (itr (car (reverse fs)) (car connect)))
      ((null itr) `(,@result)))))

;; Utilities for Jonathan Object
(declaim (inline response-accessor))
(defun response-accessor (response key)
  (getf (jonathan:parse response) key))

(declaim (inline get-value))
(defun get-value (response)
  (response-accessor response :|value|))

(defun expand-pargs (list)
  (mapcar
    (lambda (itr)
      (if (atom itr)
        itr
        (car itr)))
    list))

(defun create-counter ()
  (let
    ((counter -1))
    (lol:dlambda
      (:get   ()    (incf counter))
      (:reset ()    (setf counter -1))
      (:set   (arg) (setf counter arg))
      (:show  ()    counter))))

(defun read-file (infile)
  (with-open-file (instream infile :direction :input :if-does-not-exist nil)
    (when instream 
      (let ((string (make-string (file-length instream))))
        (read-sequence string instream)
        string))))

(defmacro read-only-plambda (largs pargs &rest body)
  (let ((pargs (mapcar #'list pargs)))
    `(let (this self)
       (setq
         this (lambda ,largs ,@body)
         self (lol:dlambda
                (:pandoric-get (sym)
                               ,(lol:pandoriclet-get pargs))
                (t (&rest args)
                   (apply this args)))))))

