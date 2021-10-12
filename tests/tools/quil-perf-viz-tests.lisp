;;;; quil-perf-viz-tests.lisp
;;;;
;;;; Author: Mark David

(in-package #:cl-quil.tools-tests)

(defparameter *perf-viz-test-info*
  '((cl-quil.tools::bell/fully-connected-baseline-vs-new-walker
     "2021-10-09-benchmark-nq-data-perf-viz-1")
    (cl-quil.tools::qft/fully-connected-baseline-vs-new-walker
     "2021-10-09-benchmark-nq-data-perf-viz-2")))

(defun get-perf-viz-json-pathname (base-file-name)  
  (make-pathname
   :name base-file-name
   :type "json"
   :defaults (cl-quil.tools::get-tests-tools-system-relative-pathname)))

(defun get-json-of-test-function (fn)
  (let ((plot-op-json-string (funcall fn :mode ':immediate)))
    (cl-json:decode-json-from-string plot-op-json-string)))

(defun get-json-of-test-file (file-name)
  (let ((pathname (get-perf-viz-json-pathname file-name)))
    (with-open-file (in pathname) 
      (cl-json:decode-json in))))
  

(deftest perf-viz-test ()
  (loop :for (fn file-name) :in *perf-viz-test-info*
        :as x := (get-json-of-test-function fn)
        :as y := (get-json-of-test-file file-name)
        :do (is (equalp x y))))
