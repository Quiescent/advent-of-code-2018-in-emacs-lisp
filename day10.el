;;; day10 --- My solution to day10 -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day10

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defvar run-from-batch nil
  "Whether this was run froom batch.

We only want long running executions to be done from the terminal
so that Emacs doesn't hang.")

(defun parse-line (line)
  "Parse LINE into (x, y, vx, vy)."
  (let* ((coord-start    (1+ (cl-position ?< line)))
         (coord-end      (cl-position ?> line))
         (velocity-start (1+ (cl-position ?< line :start coord-end)))
         (velocity-end   (cl-position ?> line :start (1+ coord-end))))
    (append (mapcar #'string-to-number
                    (split-string (substring line
                                             coord-start
                                             coord-end)
                                  ","
                                  t
                                  " "))
            (mapcar #'string-to-number
                    (split-string (substring line
                                             velocity-start
                                             velocity-end)
                                  ","
                                  t
                                  " ")))))

(defun parse-points (input-file)
  "Parse points from the INPUT-FILE as (x, y, vx, vy)."
  (mapcar #'parse-line (split-string input-file "\n" t " ")))

(require 'seq)

(defun min-x (points)
  "Produce the minimum value of X in POINTS."
  (seq-min (mapcar #'car points)))

(defun min-y (points)
  "Produce the minimum value of Y in POINTS."
  (seq-min (mapcar #'cadr points)))

(defun max-x (points)
  "Produce the maximum value of X in POINTS."
  (seq-max (mapcar #'car points)))

(defun max-y (points)
  "Produce the maximum value of Y in POINTS."
  (seq-max (mapcar #'cadr points)))

(defun move-points (points)
  "Produce the new POINTS given one tick."
  (cl-loop for (x y vx vy) in points
     collect `(,(+ x vx) ,(+ y vy) ,vx ,vy)))

(defun print-points (points)
  "Print the given POINTS."
  (let* ((min-x (min-x points))
         (max-x (max-x points))
         (len-x (- max-x min-x))
         (max-y (max-y points))
         (min-y (min-y points))
         (grid  (make-hash-table :test #'eq)))
    (cl-loop for (i j) in points
       do (puthash (+ (- i min-x) (* (- j min-y) len-x)) ?# grid))
    (cl-loop for j from min-y to max-y
       do (cl-loop for i from min-x to max-x
             do (princ (char-to-string (gethash (+ (- i min-x) (* (- j min-y) len-x))
                                                grid
                                                ?.))))
       do (princ "\n"))))

(defun day10-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (cl-loop
     with points      = (parse-points input-file)
     with count       = 0
     with prev-dist   = 100000000
     with prev-points = points
     do (setq prev-points points)
     do (setq points      (move-points points))
     for max-x     = (max-x points)
     for min-x     = (min-x points)
     for next-dist = (- max-x min-x)
     until (> next-dist prev-dist)
     do (setq prev-dist next-dist)
     do (cl-incf count)
     finally do (progn
                  (message "\n\nAfter %s iterations:\n\n" count)
                  (print-points prev-points))))

;; Wrong: AJSNXHKE
;; Right: AJZNXHKE

(let* ((test-input    "position=< 9,  1> velocity=< 0,  2>
position=< 7,  0> velocity=<-1,  0>
position=< 3, -2> velocity=<-1,  1>
position=< 6, 10> velocity=<-2, -1>
position=< 2, -4> velocity=< 2,  2>
position=<-6, 10> velocity=< 2, -2>
position=< 1,  8> velocity=< 1, -1>
position=< 1,  7> velocity=< 1,  0>
position=<-3, 11> velocity=< 1, -2>
position=< 7,  6> velocity=<-1, -1>
position=<-2,  3> velocity=< 1,  0>
position=<-4,  3> velocity=< 2,  0>
position=<10, -3> velocity=<-1,  1>
position=< 5, 11> velocity=< 1, -2>
position=< 4,  7> velocity=< 0, -1>
position=< 8, -2> velocity=< 0,  1>
position=<15,  0> velocity=<-2,  0>
position=< 1,  6> velocity=< 1,  0>
position=< 8,  9> velocity=< 0, -1>
position=< 3,  3> velocity=<-1,  1>
position=< 0,  5> velocity=< 0, -1>
position=<-2,  2> velocity=< 2,  0>
position=< 5, -2> velocity=< 1,  2>
position=< 1,  4> velocity=< 2,  1>
position=<-2,  7> velocity=< 2, -2>
position=< 3,  6> velocity=<-1, -1>
position=< 5,  0> velocity=< 1,  0>
position=<-6,  0> velocity=< 2,  0>
position=< 5,  9> velocity=< 1, -2>
position=<14,  7> velocity=<-2,  0>
position=<-3,  6> velocity=< 2, -1>")
       (test-computed (day10-part-1 test-input))
       (test-ans      "......................
......................
......................
......................
......#...#..###......
......#...#...#.......
......#...#...#.......
......#####...#.......
......#...#...#.......
......#...#...#.......
......#...#...#.......
......#...#..###......
......................
......................
......................
......................"))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; # PART 2:

;; Part 2 is computed already in part 1

;; Run the solution:

(when run-from-batch
  (progn
    (message "\n********** OUTPUT **********")
    (let ((input-1 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day10-part-1")
                       (buffer-substring (point-min)
                                         (point-max))))))
      (message "Part 1: %s" (day10-part-1 input-1)))))

(provide 'day10)
;;; day10 ends here
