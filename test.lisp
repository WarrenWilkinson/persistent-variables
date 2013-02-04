
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: persister.test; Base: 10 -*-

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

(defpackage :persistent-variables.test
  (:use :common-lisp :persistent-variables)
  (:export run-tests expect *success*))

(in-package :persistent-variables.test)

(defvar *tests* nil)
(defvar *success*)
(defmacro deftest (name () &rest body)
  `(progn (defun ,name () ,@body) (pushnew ',name *tests*)))

(defmacro expect (code)
  `(or ,code
       (progn
         (setf *success* nil)
         (format t ,(format nil "~%   unexpected false in:~%    ~s" code)))))

(defstruct results
  (tests 0)
  (failures nil))
(defun results-failure-count (results)
  (length (results-failures results)))
(defun results-successes (results)
  (- (results-tests results)
     (results-failure-count results)))

(defun runtest (fun results)
  (let* ((success t)
         (output (with-output-to-string (*standard-output*)
                   (unwind-protect 
                        (setf success (handler-case (funcall fun)
                                        (error (e) (princ e) nil)))))))
    (make-results
     :tests (1+ (results-tests results))
     :failures (if success
                   (results-failures results)
                   (acons fun output (results-failures results))))))

(defun present-failures (results)
  (format t "~%PERSISTENT-VARIABLES FAILURES:~%")
  (loop for (fn . problems) in (results-failures results)
        do (format t "~%~a~a~%" fn problems)))
(defun present-results (results)
  (format t "~%PERSISTENT-VARIABLES TEST RESULTS:")
  (format t "~%     Tests: ~a~%   Success: ~a~%  Failures: ~a" 
          (results-tests results)
          (results-successes results)
          (results-failure-count results))
  (when (results-failures results)
    (present-failures results)))
  
(defun run-tests ()
  (format t "~%RUNNING PERSISTENT-VARIABLES TESTS...")
  (present-results 
   (reduce #'(lambda (results function) (runtest function results))
           *tests* :initial-value (make-results))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  
(defvar *temp-file* #p"/tmp/persister-test.lisp")
(defvar *temp-fasl* #p"/tmp/persister-test.fasl")

(defmacro defptest (name () &rest body)
  (let ((compile (assoc :compile body)))
    `(deftest ,name () 
       (remhash 'test persistent-variables::*persisted*)
       (remhash 'test-1 persistent-variables::*persisted*)
       (remhash 'test-2 persistent-variables::*persisted*)
       (when (find-package :persistent-variables.test.workspace) 
         (delete-package :persistent-variables.test.workspace))
       (let ((*default-set* 'test)
             (persistent-variables::*loaded* (make-hash-table :test #'equalp))
             (*success* t))
         (declare (special *default-set* *success*
                           persistent-variables::*loaded*))
         (unwind-protect 
              (progn
                ;; If there is compile time stuff, compile it.
                (with-open-file (s *temp-file* :direction :output :if-exists 
                                   :supersede :if-does-not-exist :create)
                  (write-sequence
                   ,(prin1-to-string
                     `(defpackage :persistent-variables.test.workspace
                        (:use :cl :persistent-variables
                              :persistent-variables.test))) s)
                  (write-sequence
                   ,(prin1-to-string
                     `(in-package :persistent-variables.test.workspace)) s)
                  ,@(mapcar #'(lambda (code) `(write-sequence
                                          ,(prin1-to-string code) s))
                            (cdr compile)))
                (compile-file *temp-file* :output-file *temp-fasl*)
                
                ;; Now load it and eval/run the execute statements
                ,@(mapcan
                   #'(lambda (execute)
                       `((eval
                          '(progn
                            (load *temp-fasl*)
                            (let ((*standard-output* *standard-output*)
                                  (*package*
                                   (find-package
                                    :persistent-variables.test.workspace)))
                              (eval (read-from-string
                                     ,(prin1-to-string
                                       `(progn ,@(cdr execute))))))))))
                   (remove :execute body :key #'car :test-not #'eq))
                *success*)
           (delete-package :persistent-variables.test.workspace))))))
  

(defptest p-vars-are-definable ()
  (:compile
   (defpvar *compile-time-unbound*)
   (defpvar *compile-time-bound* :bound)
   (defpvar *compile-time-documented* :documented "documentation"))
  (:execute
   ;; Still around after loading?
   (expect (handler-case *compile-time-unbound*
              (unbound-variable () t)))
   (expect (handler-case (eq *compile-time-bound* :bound)
              (error () nil)))
   (expect (handler-case (eq *compile-time-documented* :documented)
              (error () nil)))
   (expect (handler-case (null (boundp '*compile-time-unbound*))
             (error () nil)))
   (expect (handler-case (not (null (boundp '*compile-time-bound*)))
             (error () nil)))
   (expect (handler-case (not (null (boundp '*compile-time-documented*)))
             (error () nil)))
  
   ;; How about some new variables? 
   (defpvar *eval-time-unbound*)
   (defpvar *eval-time-bound* :ev-bound)
   (defpvar *eval-time-documented* :ev-documented "documentation")
   
   (expect (handler-case *eval-time-unbound*
             (unbound-variable () t)))
   (expect (handler-case (eq *eval-time-bound* :ev-bound)
             (error () nil)))
   (expect (handler-case (eq *eval-time-documented* :ev-documented)
             (error () nil)))
   (expect (handler-case (null (boundp '*eval-time-unbound*))
             (error () nil)))
   (expect (handler-case (not (null (boundp '*eval-time-bound*)))
             (error () nil)))
   (expect (handler-case (not (null (boundp '*eval-time-documented*)))
             (error () nil)))))

(defptest test-p-vars-can-be-saved-and-loaded ()
  (:compile
   (let ((*default-set* 'test-1))
     (declare (special *default-set*))
     (defpvar *compile-time-1-unbound*)
     (defpvar *compile-time-1-bound* :bound-1)
     (defpvar *compile-time-1-documented* :documented-1 "documentation"))
   (defpvar *compile-time-1-packaged* :packaged-1 "documentation" 'test-1)
   
   (let ((*default-set* 'test-2))
     (declare (special *default-set*))
     (defpvar *compile-time-2-unbound*)
     (defpvar *compile-time-2-bound* :bound-2)
     (defpvar *compile-time-2-documented* :documented-2 "documentation"))
   (defpvar *compile-time-2-packaged* :packaged-2 "documentation" 'test-2))
  (:execute

   (let ((*default-set* 'test-1))
     (declare (special *default-set*))   
     (defpvar *eval-time-1-unbound*)
     (defpvar *eval-time-1-bound*      :ev-bound-1)
     (defpvar *eval-time-1-documented* :ev-documented-1 "documentation"))
   (defpvar *eval-time-1-packaged*   :ev-packaged-1 "documentation" 'test-1)

   (let ((*default-set* 'test-2))
     (declare (special *default-set*))
     (defpvar *eval-time-2-unbound*)
     (defpvar *eval-time-2-bound*      :ev-bound-2)
     (defpvar *eval-time-2-documented* :ev-documented-2 "documentation"))
   (defpvar *eval-time-2-packaged*   :ev-packaged-2 "documentation" 'test-2)
   
   (let ((saved-1 (with-output-to-string (saved-1)
                    (pv-save saved-1 'test-1)))
         (saved-2 (with-output-to-string (saved-2)
                    (pv-save saved-2 'test-2))))
     (setf *compile-time-1-unbound* :a-new-value
           *compile-time-1-bound* :a-new-value
           *compile-time-1-documented* :a-new-value
           *compile-time-1-packaged* :a-new-value)
     (setf *eval-time-1-unbound* :a-new-value
           *eval-time-1-bound* :a-new-value
           *eval-time-1-documented* :a-new-value
           *eval-time-1-packaged* :a-new-value)
     (setf *compile-time-2-unbound* :a-new-value
           *compile-time-2-bound* :a-new-value
           *compile-time-2-documented* :a-new-value
           *compile-time-2-packaged* :a-new-value)
     (setf *eval-time-2-unbound* :a-new-value
           *eval-time-2-bound* :a-new-value
           *eval-time-2-documented* :a-new-value
           *eval-time-2-packaged* :a-new-value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(expect (and (eq *compile-time-1-unbound* :a-new-value)
             (eq *compile-time-1-bound* :a-new-value)
             (eq *compile-time-1-documented* :a-new-value)
             (eq *compile-time-1-packaged* :a-new-value)))
(expect (and (eq *eval-time-1-unbound* :a-new-value)
             (eq *eval-time-1-bound* :a-new-value)
             (eq *eval-time-1-documented* :a-new-value)
             (eq *eval-time-1-packaged* :a-new-value)))
(with-input-from-string (s saved-1) (pv-load s 'test-1))
(expect (and (eq *compile-time-1-unbound* :a-new-value)
             (eq *compile-time-1-bound* :bound-1)
             (eq *compile-time-1-documented* :documented-1)
             (eq *compile-time-1-packaged* :packaged-1)))
(expect (and (eq *eval-time-1-unbound* :a-new-value)
             (eq *eval-time-1-bound* :ev-bound-1)
             (eq *eval-time-1-documented* :ev-documented-1)
             (eq *eval-time-1-packaged* :ev-packaged-1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(expect (and (eq *compile-time-2-unbound* :a-new-value)
             (eq *compile-time-2-bound* :a-new-value)
             (eq *compile-time-2-documented* :a-new-value)
             (eq *compile-time-2-packaged* :a-new-value)))
(expect (and (eq *eval-time-2-unbound* :a-new-value)
             (eq *eval-time-2-bound* :a-new-value)
             (eq *eval-time-2-documented* :a-new-value)
             (eq *eval-time-2-packaged* :a-new-value)))
(with-input-from-string (s saved-2) (pv-load s 'test-2))
(expect (and (eq *compile-time-2-unbound* :a-new-value)
             (eq *compile-time-2-bound* :bound-2)
             (eq *compile-time-2-documented* :documented-2)
             (eq *compile-time-2-packaged* :packaged-2)))
(expect (and (eq *eval-time-2-unbound* :a-new-value)
             (eq *eval-time-2-bound* :ev-bound-2)
             (eq *eval-time-2-documented* :ev-documented-2)
             (eq *eval-time-2-packaged* :ev-packaged-2))))))
   

(defptest test-p-vars-can-load-from-different-packages ()
  (:compile
   (defpvar *compile-time-1-bound* (intern "A-SYMBOL" :cl-user)))
  (:execute
   (let ((saved-1 (with-output-to-string (saved-1) (pv-save saved-1))))
     (setf *compile-time-1-bound* :a-new-value)
     (expect (eq *compile-time-1-bound* :a-new-value))
     (with-input-from-string (s saved-1) (pv-load s))
     (expect (eq *compile-time-1-bound* (intern "A-SYMBOL" :cl-user))))))

(defptest test-p-vars-can-be-late-bound ()
  (:compile
   (let ((*default-set* 'test-1))
     (declare (special *default-set*))
     (defpvar *compile-time-1-unbound*)
     (defpvar *compile-time-1-bound* :bound-1)
     (defpvar *compile-time-1-documented* :documented-1 "documentation"))
   (defpvar *compile-time-1-packaged* :packaged-1 "documentation" 'test-1)

   (let ((*default-set* 'test-2))
     (declare (special *default-set*))
     (defpvar *compile-time-2-unbound*)
     (defpvar *compile-time-2-bound* :bound-2)
     (defpvar *compile-time-2-documented* :documented-2 "documentation"))
   (defpvar *compile-time-2-packaged* :packaged-2 "documentation" 'test-2))

  (:execute
   (format t "~%ROUND ONE")
   (let ((*default-set* 'test-1))
     (declare (special *default-set*))   
     (defpvar *eval-time-1-unbound*)
     (defpvar *eval-time-1-bound*      :ev-bound-1)
     (defpvar *eval-time-1-documented* :ev-documented-1 "documentation"))
   (defpvar *eval-time-1-packaged*   :ev-packaged-1 "documentation" 'test-1)

   
 (let ((*default-set* 'test-2))
   (declare (special *default-set*))
   (defpvar *eval-time-2-unbound*)
   (defpvar *eval-time-2-bound*      :ev-bound-2)
   (defpvar *eval-time-2-documented* :ev-documented-2 "documentation"))
 (defpvar *eval-time-2-packaged*   :ev-packaged-2 "documentation" 'test-2)
   
 (setf *compile-time-1-unbound*    :updated-compile-time-1-unbound
       *compile-time-1-bound*      :updated-compile-time-1-bound
       *compile-time-1-documented* :updated-compile-time-1-documented
       *compile-time-1-packaged*   :updated-compile-time-1-packaged)
 (setf *eval-time-1-unbound*       :updated-eval-time-1-unbound
       *eval-time-1-bound*         :updated-eval-time-1-bound
       *eval-time-1-documented*    :updated-eval-time-1-documented
       *eval-time-1-packaged*      :updated-eval-time-1-packaged)
 (setf *compile-time-2-unbound*    :updated-compile-time-2-unbound
       *compile-time-2-bound*      :updated-compile-time-2-bound
       *compile-time-2-documented* :updated-compile-time-2-documented
       *compile-time-2-packaged*   :updated-compile-time-2-packaged)
 (setf *eval-time-2-unbound*       :updated-eval-time-2-unbound
       *eval-time-2-bound*         :updated-eval-time-2-bound
       *eval-time-2-documented*    :updated-eval-time-2-documented
       *eval-time-2-packaged*      :updated-eval-time-2-packaged)
 
 (let ((saved-1 (with-output-to-string (saved-1)
                  (pv-save saved-1 'test-1)))
       (saved-2 (with-output-to-string (saved-2)
                  (pv-save saved-2 'test-2))))
   
   ;; Delete the test package...
   (let* ((*package* (find-package :persistent-variables.test)))
     (delete-package :persistent-variables.test.workspace)
   
     ;; Then load the saved data
     (with-input-from-string (s saved-1) (pv-load s 'test-1))
     (with-input-from-string (s saved-2) (pv-load s 'test-2)))))
   
(:execute
   (format t "~%ROUND TWO: ~a" *package*)
   
   ;; Redeclare these guys, as if late-loaded at the REPL.
   (let ((*default-set* 'test-1))
     (declare (special *default-set*))   
     (defpvar *eval-time-1-unbound*)
     (defpvar *eval-time-1-bound*      :ev-bound-1)
     (defpvar *eval-time-1-documented*
         :ev-documented-1 "documentation"))
   (defpvar *eval-time-1-packaged*
       :ev-packaged-1 "documentation" 'test-1)
   
   (let ((*default-set* 'test-2))
     (declare (special *default-set*))
     (defpvar *eval-time-2-unbound*)
     (defpvar *eval-time-2-bound*      :ev-bound-2)
     (defpvar *eval-time-2-documented*
         :ev-documented-2 "documentation"))
   (defpvar *eval-time-2-packaged* 
       :ev-packaged-2 "documentation" 'test-2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(format t "~%ROUND TWO first round of tests...")
  
(expect (and (eq *compile-time-1-unbound*    
                 :updated-compile-time-1-unbound)
             (eq *compile-time-1-bound*      
                 :updated-compile-time-1-bound)
             (eq *compile-time-1-documented* 
                 :updated-compile-time-1-documented)
             (eq *compile-time-1-packaged*   
                 :updated-compile-time-1-packaged)))
  
(expect (and (eq *eval-time-1-unbound*       
                 :updated-eval-time-1-unbound)
             (eq *eval-time-1-bound*         
                 :updated-eval-time-1-bound)
             (eq *eval-time-1-documented*    
                 :updated-eval-time-1-documented)
             (eq *eval-time-1-packaged*      
                 :updated-eval-time-1-packaged)))
  
(expect (and (eq *compile-time-2-unbound*    
                 :updated-compile-time-2-unbound)
             (eq *compile-time-2-bound*     
                 :updated-compile-time-2-bound)
             (eq *compile-time-2-documented* 
                 :updated-compile-time-2-documented)
             (eq *compile-time-2-packaged*  
                 :updated-compile-time-2-packaged)))
  
(expect (and (eq *eval-time-2-unbound* 
                 :updated-eval-time-2-unbound)
             (eq *eval-time-2-bound*  
                 :updated-eval-time-2-bound)
             (eq *eval-time-2-documented*  
                 :updated-eval-time-2-documented)
             (eq *eval-time-2-packaged*   
                 :updated-eval-time-2-packaged)))
  
;; Redeclare these guys (as if a second time, 
;; explicitly with slime -- or as if deleting package and
;; reloading without reloading the data...
(makunbound '*eval-time-1-unbound*)
(makunbound '*eval-time-1-bound*)
(makunbound '*eval-time-1-documented*)
(makunbound '*eval-time-1-packaged*)
(let ((*default-set* 'test-1))
  (declare (special *default-set*))   
  (defpvar *eval-time-1-unbound*)
  (defpvar *eval-time-1-bound*      :ev-bound-1)
  (defpvar *eval-time-1-documented* :ev-documented-1 "documentation"))
(defpvar *eval-time-1-packaged*   :ev-packaged-1 "documentation" 'test-1)

  
(makunbound '*eval-time-2-unbound*)
(makunbound '*eval-time-2-bound*)
(makunbound '*eval-time-2-documented*)
(makunbound '*eval-time-2-packaged*)
(let ((*default-set* 'test-2))
  (declare (special *default-set*))
  (defpvar *eval-time-2-unbound*)
  (defpvar *eval-time-2-bound*      :ev-bound-2)
  (defpvar *eval-time-2-documented* :ev-documented-2 "documentation"))
(defpvar *eval-time-2-packaged*   :ev-packaged-2 "documentation" 'test-2)
  
(format t "~%ROUND TWO second round of tests.")
(expect (and (not (boundp '*eval-time-1-unbound*))
             (eq *eval-time-1-bound*         :ev-bound-1)
             (eq *eval-time-1-documented*    :ev-documented-1)
             (eq *eval-time-1-packaged*      :ev-packaged-1)))
  
(expect (and (not (boundp '*eval-time-2-unbound*))
             (eq *eval-time-2-bound*         :ev-bound-2)
             (eq *eval-time-2-documented*    :ev-documented-2)
             (eq *eval-time-2-packaged*      :ev-packaged-2)))))
  
 
(defptest test-p-vars-ignores-unbound-variables ()
  (:execute
   (defpvar *unbound-variable*)
   
   ;; Unbound variables don't case saving errors.
   (let ((saved (with-output-to-string (s) (pv-save s))))
      
     (setf *unbound-variable* :a-value)
  
     ;; But neither do they cause bound variables to unbind 
     ;; upon loading.
     (with-input-from-string (s saved) (pv-load s))
     (expect (eq *unbound-variable* :a-value)))))

(defptest test-p-vars-errors-on-unprintable-variables ()
  (:execute
   (defpvar *unprintable-variable* #'identity)
   (expect (eq 'error (handler-case (with-output-to-string (s) (pv-save s))
                        (print-not-readable () 'error))))))

(defptest test-p-vars-ignores-deleted-variables ()
  (:execute
   (when (find-package :persistent-variables.test.temporary)
     (delete-package (find-package :persistent-variables.test.temporary)))
   (let* ((var (let ((*package*
                      (make-package
                       :persistent-variables.test.temporary
                       :use (list (find-package :persistent-variables)))))
                 (prog1 (eval 
                         (read-from-string
                          "(defpvar *non-existant-variable* \"a-value\")"))
                   (delete-package *package*))))
          (saved (with-output-to-string (s) (pv-save s))))
     (expect (string-equal (symbol-value var) "a-value"))
     (setf (symbol-value var) "a-different-value")
     (expect (string-equal (symbol-value var) "a-different-value"))
     (with-input-from-string (s saved) (pv-load s))
     
     ;; However, the deleted variables are no longer saved.
     ;; as evidence by it's failure to lead.
     (expect (string-equal (symbol-value var) "a-different-value"))

   (let* ((var (let ((*package*
                      (make-package
                       :persistent-variables.test.temporary
                       :use (list (find-package :persistent-variables)))))
                 (eval
                  (read-from-string
                   "(defpvar *non-existant-variable* \"a-third-value\")")))))
     (expect (string-equal (symbol-value var) "a-third-value"))
     (with-input-from-string (s saved) (pv-load s))
     ;; Still no change, because the variable was never saved.
     (expect (string-equal (symbol-value var) "a-third-value"))
     (delete-package *package*)))))

#+sbcl 
(defptest test-p-vars-provides-loading-restarts ()
  (:compile
   (defpvar *bad-var-1* #'identity)
   (defpvar *bad-var-2* #'identity)
   (defvar *storage*
     (handler-bind ((print-not-readable
                     #'(lambda (e) (declare (ignore e)) 
                          (invoke-restart
                           'sb-ext:print-unreadably))))
       (with-output-to-string (s) (pv-save s)))))
  (:execute
   (makunbound '*bad-var-1*)
   (makunbound '*bad-var-2*)
   (handler-bind ((unloadable-variable
                   #'(lambda (e)
                       (if (eq (name e) '*bad-var-1*)
                           (invoke-restart
                            'use-value :a-new-value)
                           (invoke-restart
                            'skip-variable)))))
     (with-input-from-string (s *storage*) (pv-load s))
     (expect (eq *bad-var-1* :a-new-value))
     (expect (not (boundp '*bad-var-2*))))))
