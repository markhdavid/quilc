;;;; quil-perf-viz.lisp
;;;;
;;;; Author: Mark David

(in-package #:cl-quil.tools)

;;; This file implements charts for visualization of benchmark-nq
;;; performance data. This is closely tied to quilc module QUILC-PERF
;;; (benchmarking/quilc-perf.lisp). The chart code is based on
;;; Plotsdam package, which is in turn based on vega-lite. Plotsdam is
;;; an open source Lisp system, but it's not currently in Quicklisp --
;;; main repo here: https://github.com/kilimanjaro/plotsdam



;;;; Reading Benchmarking CSV Data

;;; Function READ-ALL-OPT-TIMINGS-FROM-FILE (see below) reads
;;; benchmarking data from a file in the format of benchmarking CSV
;;; data and returns it as a Lisp data structure in the format used by
;;; the chart generation code below.

;;; `Benchmarking CSV data' is a file format with data groups
;;; containing M timing rows (where M is the number of nQ values
;;; tested in the benchmark). The group is a series of lines of the
;;; form
;;;
;;;   opt-name
;;;   header-row
;;;   nq-timing-row 1
;;;   ...
;;;   nq-timing-row M
;;;
;;; where
;;;
;;;   opt-name the name of the optimization for this group, which
;;;   should be unique among all the opt names of this file. It should
;;;   be a series of alphanumeric and possibly other characters other
;;;   than comma (,);
;;;
;;;   header-row is a comma-separated series of Y config names (Y
;;;   being the number of config names, being the the number of
;;;   program types times the number of chip types; 
;;;
;;;   nq-timing-row is a comma-separated series of nq (an integer
;;;   number of qubits) followed by Y floating point numbers, each a
;;;   timing (in seconds) for that run in the benchmark.
;;;
;;; Each data group is delimited by beginning or end of file and any
;;; number of blank lines.

;;; Example of CSV data format:
;;;
;;;   baseline
;;;   nQ, static/fully-connected, bell/fully-connected, ...
;;;   10, 2.799, 4.893, ...
;;;   30, 21.034, 30.242, ...
;;;   50, 56.657, 73.837, ...
;;;   70, 107.445, 138.979, ...
;;;
;;;   baseline-2
;;;   nQ, static/fully-connected, bell/fully-connected, ...
;;;   10, 0.234, 0.517, ...
;;;   ...

;; TODO: currently we lack a method to auto create these files. The
;; opt names are hand created with the csv date hand copy/pasted from
;; the output of benchmark-nq. Try adding more automatiion.
;;
;; TODO: consider adding additional info after opt-name, such as date
;; of run and environment information (hardware/software
;; configuration, Lisp version, etc.). These can be added as one or
;; more comma-separated values.

(defun ez-read-csv-name-symbol (line &optional start)
  (let* ((start (or start 0))
         (end (position #\, line :start start))
         (substring (subseq line start end))
         (trimmed (string-trim '(#\space #\tab) substring))
         (hyphenated (nsubstitute #\- #\space trimmed))
         (upcased (nstring-upcase hyphenated)))
    (if (> (length upcased) 0)
        (values (intern upcased :keyword) 
                (and end (1+ end)))
        nil)))

(defun read-non-blank-line (stream)
  (loop :for l := (read-line stream nil nil)
        :while l
        :do (setq l (string-trim '(#\space #\tab) l))
        :when (not (string= l ""))
          :return l))

(defun read-opt-name (stream)
  "Call when STREAM is positioned on line with opt name, or any number
   of blank lines before such a line or end of file. Reads in from
   STREAM all such lines. If this had been before end of file, returns
   nil. Otherwise, returns an opt name (symbol in keyword package) or
   nil if unable to read."
  (let ((line (read-non-blank-line stream)))
    (and line (ez-read-csv-name-symbol line))))

(defun read-header-row (stream)
  "Call when STREAM is positioned on line with header. Returns list of
  header name symbols"
  (loop :with line 
          := (or (read-line stream nil nil) 
                 (return '()))
        :with start := 0
        :with name-symbol
        :do (multiple-value-setq (name-symbol start)
              (ez-read-csv-name-symbol line start))
        :collect name-symbol :into result
        :while start
        :finally (return result)))

(defun read-data-row (stream)
  "Call when STREAM is positioned either on a line with data or a
  blank line.  Reads in the entire line from STREAM. If on a non-blank
  line, returns true. Otherwise, if the line is well-formed, returns
  list of an int (nQ) followed by floats from the line (times in
  seconds). If not well-formed, returns three values: (1) nil; (2)
  result collected so far before problem encountered; and (3) the last
  item read (which had a problem), if any, and otherwise either nil
  or, in case of a Lisp read error, the error condition."
  (let* ((*read-eval* t)   ; do NOT eval #.(inject-stg)
         (line             ; 1st non-empty, trimmed line or nil at EOF
           (read-line stream nil nil)))
    (when line
      (setq line (string-trim '(#\space #\tab) line))
      (setq line (nsubstitute #\space #\, line)))
    (when (and line (not (string= line "")))
      (with-input-from-string (in line)
        (loop :for first-time := t :then nil
              :as next = (multiple-value-bind
                               (read-thing condition)
                             (ignore-errors (read in nil nil))
                           (if condition
                               (return (values nil result condition))
                               read-thing))
              :while next
              :do (or (if first-time
                          (integerp next)
                          (floatp next))
                      (return (values nil result next)))
              :collect next :into result
              :finally (return result))))))

(defun read-data-rows (stream)
  (loop :for data-row 
          := (read-data-row stream)
        :while data-row
        :collect data-row))

(defun read-opt-timings (stream)
  (let ((opt-name (read-opt-name stream)))
    (when opt-name
      `(,opt-name
        ,(read-header-row stream)
        ,@(read-data-rows stream)))))


(defun read-all-opt-timings-from-file (filename)
  "Read from file a list of lists of the form

    (opt-name header-row . data-rows)"
  (with-open-file (stream filename)
    (loop :for timings := (read-opt-timings stream)
          :while timings
          :collect timings)))





;;;; Defining Charts

;;; Macros DEF-CHART and DEFINE-ALL-CHARTS (defined below) are for
;;; defining chart functions for displaying benchmark timing data.

(eval-when (:compile-toplevel :load-toplevel :execute)
(defparameter *opt-name-mappings*
  '((1 . :baseline)
    (2 . :baseline-2)
    (3 . :new-walker)
    (4 . :new-walker-2)
    (5 . :cached-addresser-state)
    (6 . :cached-addresser-state-2)))

(defparameter *config-mappings*
  '((1 . :static/fully-connected)
    (2 . :bell/fully-connected)
    (3 . :qft/fully-connected)
    (4 . :hadamard/fully-connected)
    (5 . :static/linear)
    (6 . :bell/linear)
    (7 . :qft/linear)
    (8 . :hadamard/linear)))

(defun config-type-name-to-code (config-type-name)
  (or (car (rassoc config-type-name *config-mappings*))
      (error "bad config-type name (~s) => code mapping"
             config-type-name)))

(defun opt-name-to-title (opt-name)
  (string-capitalize
   (or (cdr (rassoc opt-name *opt-name-mappings*)) ; just make sure there
       (error "no mapping for opt name ~a in *opt-name-mappings*"
              opt-name))))

(defun opt-name-to-code (opt-name)
  (or (car (rassoc opt-name *opt-name-mappings*))
      (error "no mapping for opt name ~a in *opt-name-mappings*"
             opt-name)))

(defun opt-names-calculator ()
  (labels ((expr (opt-names)
             (if opt-names
                 (format nil "datum.optName == ~a ? '~a' : ~a"
                         (opt-name-to-code (first opt-names))
                         (opt-name-to-title (first opt-names))
                         (expr (rest opt-names)))
                 "'UNKNOWN'")))
    (format nil "~a" (expr (mapcar 'cdr *opt-name-mappings*)))))


(defconstant +n-opt-names-max+ 6
  "Maximum number of opt names."
  ;; Note: this can change but should be kept in sync with
  ;; *opt-name-mappings* and colors-for-opt-names.
  )

(defun colors-for-opt-names ()
  #("#2776dd"                           ; blue
    "#0e283e"                           ; black
    "#bababa"                           ; gray
    "#a60c0b"                           ; red
    "#87bd23"                           ; green
    "#ff6a00"))                         ; orange

(defun program-and-chip-to-config (program-type chip-type)
  (intern (format nil "~a/~a" program-type chip-type) :keyword))

(defun filter-config-and-opt (program-type chip-type opt-names)
  (let* ((config-type-name (program-and-chip-to-config program-type chip-type))
         (config-type-code (config-type-name-to-code config-type-name))
         (config-filter (format nil "datum.configType == ~d" config-type-code))
         (opt-names-filter
           (with-output-to-string (out)
             (loop :for first-time := t :then nil
                   :as opt-name :in opt-names
                   :collect (format out "~adatum.optName == ~a"
                                    (if first-time "" " || ")
                                    (opt-name-to-code opt-name))))))
    (format nil "~a && (~a)" config-filter opt-names-filter)))




(defun get-all-configs ()
  ;; TODO: use *benchmark-program-types* &
  ;; *benchmark-chip-connectedness-types* !
  (loop 
    :for program-type :in '(:static :bell :qft :hadamard)
    :nconc (loop
             :for chip-type :in '(:fully-connected :linear)
             :collect (program-and-chip-to-config program-type chip-type))))

(defun get-all-config-pairs ()
  ;; TODO: use *benchmark-program-types* &
  ;; *benchmark-chip-connectedness-types* !
  (loop 
    :for program-type :in '(:static :bell :qft :hadamard)
    :nconc (loop
             :for chip-type :in '(:fully-connected :linear)
             :collect `(,program-type ,chip-type))))
  

(defparameter *interesting-opt-name-groups*
  '((:baseline :new-walker)
    (:baseline :cached-addresser-state)
    (:baseline :new-walker :cached-addresser-state)

    (:baseline-2 :new-walker-2)
    (:baseline-2 :cached-addresser-state-2)
    (:baseline-2 :new-walker-2 :cached-addresser-state-2)

    (:baseline-2 :new-walker-2 :cached-addresser-state-2 
     :baseline :new-walker :cached-addresser-state)))

(defun get-name-for-chart-function (program-type chip-type group)
  (let ((config (program-and-chip-to-config program-type chip-type)))
    (intern
     (format nil "~a-~a"
             config
             (with-output-to-string (out)
               (loop :for opt-name :in group
                     :as first-time := t then nil
                     :do (format out "~a~a"
                                 (if first-time "" "-VS-")
                                 opt-name)))))))

(defun get-all-chart-defs ()
  (let* ((function-names '())
         (function-defs
           (loop :for (program-type chip-type) :in (get-all-config-pairs)
                 :nconc 
                 (loop :for group :in *interesting-opt-name-groups*
                       :as function-name 
                         := (get-name-for-chart-function 
                             program-type chip-type group)
                       :do (push function-name function-names)
                       :collect `(def-chart
                                     (,function-name)
                                     ,program-type ,chip-type
                                     ,@group)))))
    (values function-defs function-names)))





;;;; Converting Timings CSV to Plot Data (Lisp Representation)

(defun timings-to-plot-data (timings)
  (loop
    :for (opt-name header-row . data-rows) :in timings
    :as opt-name-code := (opt-name-to-code opt-name)
    :nconc (loop
             :for config-type :in (cdr header-row)
             :as config-type-code
               := (or (car (rassoc config-type *config-mappings*))
                      (error 
                       "no mapping for config type ~a in *config-mappings*"
                       config-type))
             :as index :from 1
             :nconc (loop :for data-row :in data-rows
                          :as nq := (first data-row)
                          :as time := (nth index data-row)
                          :collect `((:opt-name . ,opt-name-code)
                                     (:config-type . ,config-type-code)
                                     (:n-q . ,nq)
                                     (:time . ,time))))))





;;; CSV Data Locations and Default Location

(defparameter *default-csv-data-location*
  '(:tests/tools "2021-10-17-benchmark-nq-data"))

(defun get-tests-tools-system-relative-pathname ()
  (asdf:system-relative-pathname :cl-quil/tools "tests/tools/"))

(defun csv-location-to-pathname (spec)
  (cond
    ((atom spec)
     spec)
    (t
     ;; SPEC a list of the form (symbolic-location file-name), where
     ;; symbolic-location is one of (:tests/tools) [more later?], and
     ;; file-name is used as the NAME arg to make-pathname.
     (let ((name (second spec)))
       (ecase (first spec)
         (:tests/tools
          (make-pathname
           :name name
           :type "csv"
           :defaults (get-tests-tools-system-relative-pathname))))))))
    

(defvar *default-data*)

(defun reset-default-data ()
  (setq *default-data*
        (timings-to-plot-data
         (read-all-opt-timings-from-file
          (csv-location-to-pathname *default-csv-data-location*)))))

(defun get-default-data ()
  (unless (boundp '*default-data*)
    (reset-default-data))
  *default-data*)
  





;;; Define-all-charts

(defvar *all-chart-function-names*)

(defmacro define-all-charts ()
  (multiple-value-bind (defs names)
      (get-all-chart-defs)
    `(progn
       ,@defs
       (setq *all-chart-function-names* ',names)
       ',names)))

(defun get-chart-spec (program-type chip-type opt-names)
  (assert (<= (length opt-names) +n-opt-names-max+) 
          ()
          "Too many OPT-NAMES (~d)" (length opt-names))
  `(:|$schema| "https://vega.github.io/schema/vega-lite/v5.json"
    :title (:text ,(program-and-chip-to-config program-type chip-type)
            :anchor :middle)
    :transform #((:filter 
                  ,(filter-config-and-opt program-type chip-type opt-names))
                 (:calculate ,(opt-names-calculator) :as :|optName|))
    :width (:step 12)
    :mark :bar
    :encoding (:column (:field :|nQ| :type :ordinal 
                               :spacing 10 :header (:orient :bottom))
	       :y (:aggregate :sum :field :time
                   :title "Time (Sec)" :axis (:grid false))
               :x (:field :|optName| :axis null)
	       :color (:field :|optName|
                              :scale (:range ,(colors-for-opt-names))))
    :config (:view (:stroke :transparent) :axis (:|domainWidth| 1))))



(defvar *chart-defs*
  (make-hash-table :test 'equal)
  "Hash-table for looking up chart defs. Do not access this directly,
   but rather through LOOK-UP-CHART-DEF. Keys are compared using equal
   and are lists of the form

     (program-type chip-type . opt-names-in-order)

   Values are names of functions defined via DEF-CHART for the
   parameters specified in the key. LOOK-UP-CHART-DEF manages the
   proper ordering of keys.")


(defun make-chart-def-lookup-key (program-type chip-type opt-names)
  (let ((normalized-opt-names (sort (copy-list opt-names) #'string<)))
    (list* program-type chip-type normalized-opt-names)))

(defun look-up-chart-def (program-type chip-type opt-names)
  "Look up name of function for producing the chart for PROGRAM-TYPE,
   CHIP-TYPE, and OPT-NAMES.  Note that OPT-NAMES a list of opt names
   that appear in a benchmarking CSV data file, which are keyword
   symbols and may given here in any order."
  (gethash 
   (make-chart-def-lookup-key program-type chip-type opt-names)
   *chart-defs*))

(defsetf look-up-chart-def (program-type chip-type opt-names)
    (function-name)
  `(setf (gethash 
           (make-chart-def-lookup-key ,program-type ,chip-type ,opt-names)
           *chart-defs*)
     ,function-name))
    

(defmacro def-chart ((name) program-type chip-type &rest opt-names)
  (let ((spec (get-chart-spec program-type chip-type opt-names)))
    (let ((data-var '#:data))
      `(progn 
         (defun ,name (&key data mode)
           (let ((,data-var (or data (get-default-data))))
             (case mode
               ((nil :http)
                (plotsdam:plot (,data-var :mode :http) ,@spec))
               (otherwise
                (plotsdam:plot (,data-var :mode :immediate) ,@spec)))))
         (setf (look-up-chart-def ',program-type ',chip-type ',opt-names)
               ',name)
         ',name))))


(defun call-chart (program-type chip-type opt-names 
                   &key (data nil data-specified)
                        (mode nil mode-specified))
  "Call for chart based on PROGRAM-TYPE, CHIP-TYPE, and OPT-NAMES with
   optional keyword args :DATA (defaults to data specified by
   *default-csv-data-location*) and :MODE (one of :HTTP or :IMMEDIATE,
   and defaults to :HTTP)."
  (let ((fn 
          (or (look-up-chart-def program-type chip-type opt-names)
              (error "Chart fn lookup failed."))))
    (apply fn `(,(and data-specified `(:data ,data))
                ,(and mode-specified `(:mode ,mode))))))
     

)                                       ; end (eval-when ...)





;;;; All Charts Defined

(define-all-charts)

