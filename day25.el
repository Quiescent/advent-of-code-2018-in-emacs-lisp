;;; day25 --- My solution to day25 -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day25

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defvar run-from-batch nil
  "Whether this was run froom batch.

We only want long running executions to be done from the terminal
so that Emacs doesn't hang.")

(require 'subr-x)

(defun parse-points (input-file)
  "Parse points from INPUT-FILE."
  (thread-last (split-string input-file "\n" t " ")
    (cl-mapcar (lambda (line) (thread-last (split-string line "[^0-9-]" t " ")
                                (cl-mapcar #'string-to-number))))))

(defun manhattan-distance (this that)
  "Produce the manhattan distance btween THIS and THAT."
  (pcase (cons this that)
    (`((,x1 ,y1 ,z1 ,t1) . (,x2 ,y2 ,z2 ,t2))
      (+ (abs (- x1 x2))
         (abs (- y1 y2))
         (abs (- z1 z2))
         (abs (- t1 t2))))))

(defun in-constellation (point constellation)
  "Produce t if POINT is in CONSTELLATION."
  (cl-find-if (lambda (other-point) (< (manhattan-distance point other-point)
                                       3))
              constellation))

(defun initial-constellations (points)
  "Produce an initial set of constellations from POINTS."
  (let* ((constellations `((,(car points)))))
    (cl-loop for point in (cdr points)
       for constellation = (cl-find-if (apply-partially #'in-constellation point) constellations)
       if constellation
         do (push point constellation)
       else
         do (push (list point) constellations)
       finally return constellations)))

(defun constellations-can-merge (this other)
  "Produce t if THIS contains a point close enough to a point in OTHER."
  (cl-some (lambda (other-point)
             (cl-some (lambda (this-point)
                        (<= (manhattan-distance this-point other-point)
                            3))
                      this))
           other))

(defun day25-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (let* ((points         (parse-points input-file))
         (constellations (cl-mapcar #'list points)))
    (cl-loop
       with merged-two = nil
       with next       = '()
       for  this       = (pop constellations)
       for  merges     = (cl-remove-if-not (lambda (other) (constellations-can-merge this other))
                            constellations)
       if merges
         do (progn
              (push (apply #'append (cons this merges)) next)
              (setq constellations (cl-remove-if (lambda (constellation)
                                                   (memq constellation merges))
                                                 constellations))
              (setq merged-two t))
       else
         do (push this next)
       when (null constellations)
         do (progn
              (setq constellations next
                    next           nil)
              (when (not merged-two)
                (cl-return))
              (setq merged-two nil)))
    (length constellations)))

(let* ((test-input    "-1,2,2,0
0,0,2,-2
0,0,0,-2
-1,2,0,0
-2,-2,-2,2
3,0,2,-1
-1,3,2,2
-1,0,-1,0
0,2,1,-2
3,0,0,0")
       (test-computed (day25-part-1 test-input))
       (test-ans      4))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; Run the solution:

(when run-from-batch
  (progn
    (message "\n********** OUTPUT **********")
    (let ((input-1 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day25-part-1")
                       (buffer-substring (point-min)
                                         (point-max))))))
      (message "Part 1: %s" (day25-part-1 input-1)))))

(provide 'day25)
;;; day25 ends here
