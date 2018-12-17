;;; day17 --- My solution to day17 -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day17

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defvar run-from-batch nil
  "Whether this was run froom batch.

We only want long running executions to be done from the terminal
so that Emacs doesn't hang.")

(require 'subr-x)

(defun place-line-of-clay (map line)
  "Place onto MAP, the LINE of clay."
  (pcase (thread-last (split-string line "[^0-9]" t " ")
           (mapcar #'string-to-number ))
    (`(,a ,b ,c)
      (progn
        (if (eq ?x (aref line 0))
            (cl-loop for y from b to c
               do (puthash (cons a y) t map))
            (cl-loop for x from b to c
               do (puthash (cons x a) t map)))
        map))))

(defvar min-y 0)
(defvar max-y 0)
(defvar min-x 0)
(defvar max-x 0)
(defvar filled-tracker nil)

(defun fill (x y map water)
  "Fill in the row at X Y in MAP on WATER."
  (let* ((fell-left  nil)
         (right-edge nil)
         (fell-right nil)
         (left-edge  nil))
    (cl-labels ((fill-row (x y)
                  (let* ((y-below (1+ y)))
                    (progn
                      (setq fell-left nil
                            left-edge (cl-loop for l from x downto min-x
                                         when (gethash (cons l y) map)
                                           return (1+ l)
                                         when (and
                                                ;; Map to the bottom right of us
                                               (gethash (cons (1+ l) y-below) map)
                                               ;; No water to the left of us
                                               (not (gethash (cons (1- l) y)  water))
                                               ;; No blocks beneath us
                                               (not (gethash (cons l y-below) map))
                                               ;; Not where we started
                                               (not (eq x l)))
                                           do (progn
                                                (setq fell-left t)
                                                (cl-return l)))
                            fell-right nil
                            right-edge (cl-loop for l from x to max-x
                                          when (gethash (cons l y) map)
                                            return (1- l)
                                          when (and
                                                 ;; Map to the bottom left of us
                                                (gethash (cons (1- l) y-below) map)
                                                ;; No water to the right of us
                                                (not (gethash (cons (1+ l) y) water))
                                                ;; No block beneath us
                                                (not (gethash (cons l y-below) map))
                                                ;; Not where we started
                                                (not (eq x l)))
                                            do (progn
                                                 (setq fell-right t)
                                                 (cl-return l))))))))
      (while (not (or fell-right fell-left))
        (fill-row x y)
        (when (not (or fell-right fell-left))
          (cl-loop for l from left-edge to right-edge
             do (puthash (cons l y) t water)
             do (puthash (cons l y) t filled-tracker))
          (cl-decf y))))
    (let* ((start (if fell-left  (1+ left-edge)  left-edge))
           (end   (if fell-right (1- right-edge) right-edge)))
      (cl-loop for l from start to end
         do (puthash (cons l y) t water)))
    (and fell-left  (trace left-edge  y map water))
    (and fell-right (trace right-edge y map water))
    water))

(defun trace (x y map water)
  "Trace the path of the water from X Y in MAP.

Fill in blocks on WATER as we go."
  (if (or (> y max-y)
          (gethash (cons x y) water))
      water
      (while (let* ((coord-below (cons x (1+ y))))
               (and (not (or (gethash coord-below map)
                             (gethash coord-below water)))
                    (not (> y max-y))))
        (puthash (cons x y) t water)
        (cl-incf y))
      (progn
        (when (not (> y max-y))
          (puthash (cons x y) t water)
          (if (gethash (cons x (1+ y)) water)
              (fill x (+ 2 y) map water)
              (fill x y map water)))
        water)))

(defun print-map (map water)
  "Print MAP with WATER overlaid."
  (progn
    (cl-loop for y from min-y to max-y
       do (cl-loop for x from min-x to max-x
             for is-water = (gethash (cons x y) water)
             for is-clay  = (gethash (cons x y) map)
             when is-clay
               do (princ "#")
             when is-water
               do (princ "w")
             when (not (or is-water is-clay))
               do (princ " "))
       do (princ "\n"))))

(defun day17-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (let* ((filled-tracker (make-hash-table :test #'equal))
         (map   (cl-reduce #'place-line-of-clay
                           (split-string input-file "\n" t " ")
                           :initial-value (make-hash-table :test #'equal)))
         (max-y (thread-last (hash-table-keys map)
                  (mapcar #'cdr)
                  (funcall (lambda (seq) (cl-sort seq #'>)))
                  (car)))
         (min-y (thread-last (hash-table-keys map)
                  (mapcar #'cdr)
                  (funcall (lambda (seq) (cl-sort seq #'<)))
                  (car)))
         (max-x (thread-last (hash-table-keys map)
                  (mapcar #'car)
                  (funcall (lambda (seq) (cl-sort seq #'>)))
                  (car)
                  (+ 2)))
         (min-x (thread-last (hash-table-keys map)
                  (mapcar #'car)
                  (funcall (lambda (seq) (cl-sort seq #'<)))
                  (car)
                  (+ -2)))
         (water (make-hash-table :test #'equal))
         (max-lisp-eval-depth 100000)
         (max-specpdl-size    100000))
    (thread-last (trace 500 0 map water)
      (hash-table-keys)
      (cl-remove-if (lambda (key) (or (< (cdr key) min-y) (> (cdr key) max-y))))
      (length))))

;; Too low: 3616

;; Correct: 36171

(let* ((test-input    "x=495, y=2..7
y=7, x=495..501
x=501, y=3..7
x=498, y=2..4
x=506, y=1..2
x=498, y=10..13
x=504, y=10..13
y=13, x=498..504")
       (test-computed (day17-part-1 test-input))
       (test-ans      57))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; # PART 2:

(defun day17-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (let* ((filled-tracker (make-hash-table :test #'equal))
         (map   (cl-reduce #'place-line-of-clay
                           (split-string input-file "\n" t " ")
                           :initial-value (make-hash-table :test #'equal)))
         (max-y (thread-last (hash-table-keys map)
                  (mapcar #'cdr)
                  (funcall (lambda (seq) (cl-sort seq #'>)))
                  (car)))
         (min-y (thread-last (hash-table-keys map)
                  (mapcar #'cdr)
                  (funcall (lambda (seq) (cl-sort seq #'<)))
                  (car)))
         (max-x (thread-last (hash-table-keys map)
                  (mapcar #'car)
                  (funcall (lambda (seq) (cl-sort seq #'>)))
                  (car)
                  (+ 2)))
         (min-x (thread-last (hash-table-keys map)
                  (mapcar #'car)
                  (funcall (lambda (seq) (cl-sort seq #'<)))
                  (car)
                  (+ -2)))
         (water (make-hash-table :test #'equal))
         (max-lisp-eval-depth 100000)
         (max-specpdl-size    100000))
    (trace 500 0 map water)
    (thread-last filled-tracker
      (hash-table-keys)
      (cl-remove-if (lambda (key) (or (< (cdr key) min-y) (> (cdr key) max-y))))
      (length))))

(let* ((test-input    "x=495, y=2..7
y=7, x=495..501
x=501, y=3..7
x=498, y=2..4
x=506, y=1..2
x=498, y=10..13
x=504, y=10..13
y=13, x=498..504")
       (test-computed (day17-part-2 test-input))
       (test-ans      25))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; Run the solution:

(when run-from-batch
  (progn
    (message "\n********** OUTPUT **********")
    (let ((input-1 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day17-part-1")
                       (buffer-substring (point-min)
                                         (point-max)))))
          (input-2 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day17-part-1")
                       (buffer-substring (point-min)
                                         (point-max))))))
      (message "Part 1: %s" (day17-part-1 input-1))
      (message "Part 2: %s\n" (day17-part-2 input-2)))))

(provide 'day17)
;;; day17 ends here
