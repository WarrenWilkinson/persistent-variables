
;;; -*- Mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
  
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
  
(defsystem :persistent-variables
  :name "persistent-variables"
  :version "1.0.0"
  :author "Warren Wilkinson <warrenwilkinson@gmail.com>"
  :license "lgpl2"
  :description "A library for persistent global variables."
  :components ((:file "persistent-variables"))
  :in-order-to ((test-op (load-op persistent-variables.test))))

(defsystem :persistent-variables.test
  :name "persistent-variables.test"
  :version "1.0.0"
  :author "Warren Wilkinson <warrenwilkinson@gmail.com>"
  :description "Testing code for the persistent-variables library"
  :licence "LGPL2"
  :depends-on (:persistent-variables)
  :components ((:file "test")))
    
(defmethod perform ((op asdf:test-op) (system (eql (find-system :persistent-variables))))
  (funcall (intern "RUN-TESTS" :persistent-variables.test)))
