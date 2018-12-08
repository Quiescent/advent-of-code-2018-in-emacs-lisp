;;; day3 --- My solution to day3 -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day3

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defvar run-from-batch nil
  "Whether this was run froom batch.

We only want long running executions to be done from the terminal
so that Emacs doesn't hang.")

(require 'subr-x)

(defun day3-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (cl-loop for line in (split-string input-file "\n" t " ")
     with grid = (make-hash-table :test #'equal)
     do (pcase (split-string line " " t " ")
          (`(,_ "@" ,coord ,dim)
            (pcase (cons (mapcar #'string-to-number
                                 (split-string
                                  (substring coord
                                             0
                                             (1- (length coord)))
                                  "," t " "))
                         (mapcar #'string-to-number (split-string dim "x" t " ")))
              (`((,x ,y) . (,w ,h))
                (cl-loop for i from x below (+ x w)
                   do (cl-loop for j from y below (+ y h)
                         for current-count = (gethash (cons i j) grid 0)
                         do (puthash (cons i j) (1+ current-count) grid)))))))
     finally return (cl-count-if (lambda (x) (>= x 2)) (hash-table-values grid))))


;; (defun day3-part-1 (input-file)
;;   "Run my solution to part one of the problem on the input in INPUT-FILE."
;;   (cl-loop for line in (split-string input-file "\n" t " ")
;;      with grid = (make-hash-table :test #'equal)
;;      do (pcase (split-string line " " t " ")
;;           (`(,_ "@" ,coord ,dim)
;;             (pcase (cons (mapcar #'string-to-number
;;                                  (split-string
;;                                   (substring coord
;;                                              0
;;                                              (1- (length coord)))
;;                                   "," t " "))
;;                          (mapcar #'string-to-number (split-string dim "x" t " ")))
;;               (`((,x ,y) . (,w ,h))
;;                 (cl-loop for i from x below (+ x w)
;;                    do (cl-loop for j from y below (+ y h)
;;                          for current-count = (gethash (cons i j) grid 0)
;;                          do (puthash (cons i j) (1+ current-count) grid)))))))
;;      finally (cl-loop for i below 1000
;;                 do (cl-loop for j below 1000
;;                       do (princ (pcase (gethash (cons j i) grid 0)
;;                                   (0 "0")
;;                                   (1 "1")
;;                                   (_ "X"))))
;;                 do (princ "\n"))))

;; # PART 2:

;; #1 @ 1,3: 4x4
;; #2 @ 3,1: 4x4
;; #3 @ 5,5: 2x2

(defun day3-intersects (rect1 rect2)
  "Produce t if RECT1 intersects with RECT2."
  (pcase (if (> (car rect1) (car rect2))
             (cons rect1 rect2)
             (cons rect2 rect1))
    (`((,x1 ,y1 ,w1 ,h1) . (,x2 ,y2 ,w2 ,h2))
      (and (< x1        (+ x2 w2))
           (> (+ x1 w1) x2)
           (< y1        (+ y2 h2))
           (> (+ y1 h1) y2)))))

(defun day3-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (let ((rects (cl-loop for line in (split-string input-file "\n" t " ")
                  collect (pcase (split-string line " " t " ")
                            (`(,id "@" ,coord ,dim)
                              (pcase (cons (mapcar #'string-to-number
                                                   (split-string
                                                    (substring coord
                                                               0
                                                               (1- (length coord)))
                                                    "," t " "))
                                           (mapcar #'string-to-number (split-string dim "x" t " ")))
                                (`((,x ,y) . (,w ,h))
                                  `(,id ,x ,y ,w ,h))))))))
    (cl-loop for rect1 in rects
       when (cl-loop for rect2 in rects
               always (or (equal rect1 rect2)
                          (not (day3-intersects (cdr rect1) (cdr rect2)))))
         collect (car rect1))))

;; 350 isn't correct
;; 415 was correct... now to make it so in the code...

;; Run the solution:

(when run-from-batch
  (progn
    (message "\n********** OUTPUT **********")
    (let ((input-1 (with-temp-buffer
                     (find-file-literally "day3-part-1")
                     (buffer-substring (point-min)
                                       (point-max))))
          (input-1 (with-temp-buffer
                     (find-file-literally "day3-part-1")
                     (buffer-substring (point-min)
                                       (point-max)))))
      (message "Part 2: %s" (day3-part-1 input-1))
      (message "Part 2: %s\n" (day3-part-2 input-1)))))

(provide 'day3)
;;; day3 ends here
