;;;; some-benchmarking-bar-charts.lisp

;; This is kind of meant as a "gist" for code that puts several charts
;; on a single web page and goes beyond some of the current exported
;; functionality of Plotsdam but still uses some of its
;; functionality. However, some of that functionality is
;; internal-only, e.g., plotsdam::make-javascript-literal -- that is
;; not really central to Plotsdam, but it's nice to have.  This also
;; copies and specializes some Plotsdam's HTML boilerplate. Discuss
;; with EJD about how to best divide the labor correctly between this
;; kind of stuff and Plotsdam. -mhdavid, 10/19/21

(defparameter *template*
"
<!DOCTYPE html>
<html>
  <head>
    <title>Vega-Lite Plot</title>
    <script src=\"https://cdn.jsdelivr.net/npm/vega@5.15.0\"></script>
    <script src=\"https://cdn.jsdelivr.net/npm/vega-lite@4.15.0\"></script>
    <script src=\"https://cdn.jsdelivr.net/npm/vega-embed@6.11.1\"></script>
  </head>
  <body>
    <table>
~a    </table>
    <script type=\"text/javascript\">
~a    </script>
  </body>
</html>")

    

(defun plot-baseline-vs-new-walker ()
  (loop :with indent = (make-string 6 :initial-element #\space)
        :with divs := '()
        :with embeds := '()
        :with i := 1
        :for p :in '(:static :bell :qft :hadamard)
        :do (loop :for c :in '(:fully-connected :linear)
                  :do (push (format nil "<br/><div id=\"vis~d\"/>" i) divs)
                      (push (format nil "vegaEmbed('#vis~d', JSON.parse(~a));"
                                    i
                                    (plotsdam::make-javascript-literal
                                     (cl-quil.tools:call-chart
                                      p c '(:baseline :new-walker)
                                      :mode ':immediate)))
                            embeds)
                      (incf i))
        :finally
           (setq divs (nreverse divs))
           (setq embeds (nreverse embeds))
           (return
             (format
              nil *template*
              (with-output-to-string (out)
                (loop :for (l r) :on divs :by #'cddr
                      :do (format out "~a<tr>~%" indent)
                          (format out "~a  <td>~a</td>~%" indent l)
                          (when r
                            (format out "~a  <td>~a</td>~%" indent r))
                          (format out "~a</tr>~%" indent)))
              (with-output-to-string (out)
                (loop for embed in embeds 
                      do (format out "~a~a~%" indent embed)))))))

(defun write-plot-baseline-vs-new-walker (file-name)
  (with-open-file (out file-name
                       :direction :output
                       :if-exists :supersede)
    (format out "~a" (plot-baseline-vs-new-walker))))
