
;;; -*- Mode: LISP; Syntax: common-lisp; Package: persistent-variables; Base: 10 -*-

;;; Copyright (c) 2013, Warren Wilkinson.  All rights reserved.

;;; BEGIN_LICENSE:LGPL2
;;;
;;; This library is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Library General Public License as published by
;;; the Free Software Foundation; version 2.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public License
;;; along with this library; see the file COPYING.LIB.  If not, write to
;;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;;; Boston, MA 02110-1301, USA.
;;;
;;; END_LICENSE 

(defpackage :persistent-variables
  (:use :common-lisp)
  (:nicknames :pv)
  (:export defpvar pv-save pv-load *default-set* persist unpersist
           unloadable-variable skip-variable use-value name text expression))

(in-package :persistent-variables)

(defvar *persisted* (make-hash-table))
(defvar *default-set* :default)

(defun persist (name &optional (set *default-set*))
  "Add a variable to persistence serialization."
  (push name (gethash set *persisted* nil)))

(defun unpersist (name &optional (set *default-set*))
  "Remove a variable from persistence serialization."
  (setf (gethash set *persisted*)
        (remove name (gethash set *persisted*))))

(flet ((serialize (var)
         (list (package-name (symbol-package var))
               (symbol-name var)
               (let ((*package* (find-package :cl-user))
                     (*print-readably* t))
                 (prin1-to-string (symbol-value var))))))
  (defun pv-save (stream &optional (set *default-set*))
    "Save all defpvar values to stream."
    (dolist (var (remove-duplicates (gethash set *persisted*)))
      (when (and (symbol-package var) (boundp var))
        (prin1 (serialize var) stream)
        (terpri stream)))))

(define-condition unloadable-variable (error)
  ((name :initarg :name :reader name)
   (text :initarg :text :reader text)
   (expression :initarg :expression :reader expression)))

(defmethod print-object ((c unloadable-variable) stream)
  (format stream "Unloadable variable ~s: ~a in ~s"
          (let ((*package* (find-package :cl-user)))
            (prin1-to-string (name c)))
          (text c)
          (expression c)))

(defun pv-read (symbol value)
  "Attempt to read a saved value."
  (restart-case
      (handler-case (values 
                     (let ((*package* (find-package :cl-user)))
                       (read-from-string value))
                     t)
        (error (e) (let* ((msg (princ-to-string e))
                          (msg (subseq msg 0 (position #\Newline msg))))
                     (error 'unloadable-variable 
                            :name symbol :text msg :expression value))))
    (skip-variable ()
      :report "Skip loading this variable."
      (values nil nil))
    (use-value (value) 
      :report "Specify a value to use."
      :interactive (lambda ()
                     (format t "~&Value for ~s: " symbol)
                     (list (eval (read))))
      (values value t))))

(defvar *loaded* (make-hash-table)
  "Store loaded values that are missing their corresponding variables.")

(flet ((pv-set (package symbol value)
         "Attempt to set package:symbol to value. Return t if done right."
         (let* ((p (find-package package))
                (s (and p (find-symbol symbol p))))
           (multiple-value-bind (val found-p)
               (and s (pv-read s value))
             (when found-p (set s val) t)))))
  (defun pv-load (stream &optional (set *default-set*))
    "Load variable bindings from stream and set persistent-variables.."
    (loop for (package symbol value) = (read stream nil '(:eof :eof :eof))
       until (eq package :eof)
       for did-set? = (pv-set package symbol value)
       unless did-set?
       collect (list package symbol value) into bindings
       finally (setf (gethash (symbol-name set) *loaded*) bindings))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((load-eq (a b)
           (and (string-equal (first a) (first b))
                (string-equal (second a) (second b)))))
    (defun cached-string-forget (symbol
                                 &optional (set *default-set*))
      (setf (gethash (symbol-name set) *loaded*)
            (remove (list (package-name (symbol-package symbol))
                          (symbol-name symbol))
                    (gethash (symbol-name set) *loaded*)
                    :test #'load-eq)))
    
    (defun cached-string (package name
                          &optional (set *default-set*))
      (let ((bind (find (list (package-name (find-package package))
                              (symbol-name name))
                        (gethash (symbol-name set) *loaded*)
                        :test #'load-eq)))
        (values (third bind) (not (null bind)))))))

(defmacro defpvar (name 
                   &optional
                     (val ''unbind)
                     (doc nil doc-supplied-p)
                     (set '*default-set*))
  "Define persistent variable, it'll take it's cached value if available."
  (let ((pset (gensym))  (value (gensym))  (found-p (gensym)))
    `(let ((,pset ,set))
       (defvar ,name
         (multiple-value-bind (,value ,found-p)
             (cached-string *package* ',name ,pset)
           (if ,found-p (pv-read ',name ,value) ,val))
         ,@(if doc-supplied-p (list doc)))
       
       (when (eq (symbol-value ',name) 'unbind) (makunbound ',name))
       (persist ',name ,pset)
       (cached-string-forget ',name ,pset)
       ',name)))
