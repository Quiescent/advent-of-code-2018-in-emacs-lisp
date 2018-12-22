;;; day22 --- My solution to day22 -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day22

;;; Code:

;; # PART 1:

(require 'cl-lib)

(require 'subr-x)

(defun parse-input (input-file)
  "Parse a tuple of the depth and target from INPUT-FILE."
  (let* ((lines (split-string input-file "\n" t " ")))
    (cons (thread-last (split-string (car lines) " " t)
            (cadr)
            (string-to-number))
          (thread-last (split-string (cadr lines) "[^0-9]" t " ")
            (cl-mapcar #'string-to-number)))))

(require 'map)

(defun index (x y target-x target-y depth index-map)
  "Produce the geological index of X Y.

TARGET-X, TARGET-Y has an index of 0.

There is a cave system depth of DEPTH.

If the result is already in the INDEX-MAP then use it, putting
results there when found."
  (let* ((index-in-map (map-elt index-map (cons x y))))
    (or index-in-map
        (setf (map-elt index-map (cons x y))
              (pcase (cons x y)
                ('(0  . 0)  0)
                (`(0  . ,_) (* y 48271))
                (`(,_ . 0)  (* x 16807))
                (`(,_ . ,_) (if (and (eq x target-x)
                                     (eq y target-y))
                                0
                                (* (erosion-level (1- x)
                                                  y
                                                  target-x
                                                  target-y
                                                  depth
                                                  index-map)
                                   (erosion-level x
                                                  (1- y)
                                                  target-x
                                                  target-y
                                                  depth
                                                  index-map)))))))))

(defun erosion-level (x y target-x target-y depth index-map)
  "Produce a regions erosion level at X Y.

The target is at TARGET-X, TARGET-Y.

There is a cave system depth of DEPTH.

The index is cached in the INDEX-MAP."
  (thread-first (index x y target-x target-y depth index-map)
    (+ depth)
    (mod 20183)))

(defun tile-type (x y target-x target-y depth index-map)
  "Produce the type of tile at X, Y.

The target is at TARGET-X, TARGET-Y.

There is a cave system depth of DEPTH.

Index is cached in the INDEX-MAP."
  (pcase (thread-first (erosion-level x y target-x target-y depth index-map)
           (mod 3))
    (0 'ROCKY)
    (1 'WET)
    (2 'NARROW)))

(defun tile-score (tile-type)
  "Produce the score assigned to TILE-TYPE."
  (pcase tile-type
    ('ROCKY  0)
    ('WET    1)
    ('NARROW 2)))

(defun day22-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (pcase (parse-input input-file)
    (`(,depth ,target-x ,target-y)
      (let* ((index-map (make-hash-table :test #'equal)))
        (cl-loop for x from 0 to target-x
           sum (cl-loop for y from 0 to target-y
                  sum (thread-last (tile-type x y target-x target-y depth index-map)
                        (tile-score))))))))

(let* ((test-input    "depth: 510
target: 10,10")
       (test-computed (day22-part-1 test-input))
       (test-ans      114))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; Wrong: 9838

(defvar run-from-batch nil
  "Whether this was run froom batch.

We only want long running executions to be done from the terminal
so that Emacs doesn't hang.")

;; # PART 2:

(defun remove-slower (search-states found-state)
  "Remove states from SEARCH-STATES whose time cannot be better than FOUND-STATE."
  (if found-state
      (cl-remove-if (lambda (search-state) (> (car search-state)
                                              (car found-state)))
                    search-states)
      search-states))

(defun find-best-found (search-states found-state target-coord)
  "Produce the best found state in SEARCH-STATES and FOUND-STATE.

The target is at TARGET-COORD."
  (cl-loop for candidate in search-states
     with best = found-state
     when (and (eq 'TORCH (cadr candidate))
               (equal target-coord (cddr candidate))
               (or (null best)
                   (and best
                        (< (car candidate)
                           (car best)))))
       do (setq best candidate)
     finally return best))

(defun adjacent-coords (coord)
  "Produce the coordinates around COORD."
  (pcase coord
    (`(,x . ,y) (cl-remove nil
                           (list (when (> x 0)
                                   (cons (1- x) y))
                                 (when (> y 0)
                                   (cons x      (1- y)))
                                 (cons (1+ x) y)
                                 (cons x      (1+ y)))))))

(defun get-tile-type-cached (tile-map target-coord depth index-map coord)
  "Produce the type of tile at COORD."
  (map-elt tile-map coord (tile-type (car coord)
                                     (cdr coord)
                                     (car target-coord)
                                     (cdr target-coord)
                                     depth
                                     index-map)))

(defun allowed (gear tile)
  "Produce t if GEAR is allowed on TILE."
  (pcase (cons gear tile)
    ('(TORCH   . ROCKY)  t)
    ('(TORCH   . NARROW) t)
    ('(GEAR    . ROCKY)  t)
    ('(GEAR    . WET)    t)
    ('(NEITHER . WET)    t)
    ('(NEITHER . NARROW) t)))

(defun move (time equipped current-coord index-map tile-map target-coord depth)
  "Move from the current coord."
  (let* ((adjacent-coords (adjacent-coords current-coord))
         (adjacent-tiles  (cl-mapcar (apply-partially #'get-tile-type-cached
                                                      tile-map
                                                      target-coord
                                                      depth
                                                      index-map)
                                     adjacent-coords)))
    (thread-last (cl-mapcar (lambda (coord tile) (when (allowed equipped tile)
                                                   (cons (1+ time) (cons equipped coord))))
                            adjacent-coords adjacent-tiles)
      (cl-remove nil))))

(defun expand-search (search-state index-map tile-map target-coord depth)
  "Expand the seacrh from SEARCH-STATE."
  (pcase search-state
    (`(,time ,equipped . ,current-coord)
      (let* ((tile (get-tile-type-cached tile-map target-coord depth index-map current-coord)))
        (cons (pcase (cons equipped tile)
                ('(TORCH   . ROCKY)  (cons (+ time 7) (cons 'GEAR    current-coord)))
                ('(TORCH   . NARROW) (cons (+ time 7) (cons 'NEITHER current-coord)))
                ('(GEAR    . ROCKY)  (cons (+ time 7) (cons 'TORCH   current-coord)))
                ('(GEAR    . WET)    (cons (+ time 7) (cons 'NEITHER current-coord)))
                ('(NEITHER . WET)    (cons (+ time 7) (cons 'GEAR    current-coord)))
                ('(NEITHER . NARROW) (cons (+ time 7) (cons 'TORCH   current-coord))))
              (move time equipped current-coord index-map tile-map target-coord depth))))))

(defun remove-seen (seen search-states)
  "Remove the states which have been SEEN from SEARCH-STATES.

Update seen for the new states."
  (cl-remove-if (lambda (search-state)
                  (let* ((seen-state  (map-elt seen (cdr search-state)))
                         (faster-than (and seen-state
                                           (< (car search-state)
                                              (car seen-state)))))
                    (when (or (not seen-state)
                              faster-than)
                      (setf (map-elt seen (cdr search-state)) search-state))
                    (and seen-state (not faster-than))))
                search-states))

(defun manhattan-distance (this that)
  "Produce the manhattan distance between THIS and THAT."
  (+ (abs (- (car this) (car that)))
     (abs (- (cdr this) (cdr that)))))

(defun closer (target-coord this-coord that-coord)
  "If TARGET-COORD is closer to THIS-COORD than THAT-COORD produce t."
  (< (manhattan-distance this-coord target-coord)
     (manhattan-distance that-coord target-coord)))

(defun find-closest (heads target)
  "Find the head in HEADS which is closest to the TARGET."
  (cl-loop for head in (cdr heads)
     with closest          = (car heads)
     with closest-distance = (manhattan-distance (cddr closest) target)
     with closest-time     = (car closest)
     for  next-distance    = (manhattan-distance (cddr head) target)
     for  next-time        = (car head)
     when (and (<= next-distance closest-distance)
               (<  next-time     closest-time))
       do (setq closest          head
                closest-distance next-distance
                closest-time     next-time)
     finally return closest))

(defun find-target (depth target-x target-y)
  "Find the target in the cave of DEPTH at TARGET-X TARGET-Y."
  (let* ((index-map           (make-hash-table :test #'equal))
         (tile-map            (make-hash-table :test #'equal))
         (seen                (make-hash-table :test #'equal))
         (target-coord        (cons target-x target-y))
         (max-lisp-eval-depth 10000)
         (max-specpdl-size    32000)
         (gc-cons-threshold   most-positive-fixnum))
    (cl-loop
       with found = nil
       with heads = '((0 TORCH . (0 . 0)))
       while heads
       for search-head = (find-closest heads target-coord)
       do (setq heads (delq search-head heads))
       when (and (or (null found)
                     (and found
                          (< (car search-head)
                             (car found)))))
         do (let* ((new-heads (expand-search search-head
                                             index-map
                                             tile-map
                                             target-coord
                                             depth))
                   (next-found (find-best-found new-heads
                                                found
                                                target-coord)))
              
              (when (not (eq next-found found))
                (setq found next-found)
                (setq heads (cl-remove found heads)))
              (setq heads (append (remove-seen seen new-heads) heads)))
       finally return (car found))))

(defun day22-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (apply #'find-target (parse-input input-file)))

(let* ((test-input    "depth: 510
target: 10,10")
       (test-computed (day22-part-2 test-input))
       (test-ans      45))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; Too slow to run as I go...
;; (when (not run-from-batch)
;;   (message "Results: %s"
;;            (progn
;;              (profiler-start 'cpu+mem)
;;              (benchmark-run 100 (let* ((test-input    "depth: 510
;; target: 10,10")
;;                                        (test-computed (day22-part-2 test-input))
;;                                        (test-ans      45))
;;                                   (message "Expected: %s\n    Got:      %s" test-ans test-computed)))
;;              (profiler-report)
;;              (profiler-stop))))

;; Run the solution:

(when run-from-batch
  (progn
    (message "\n********** OUTPUT **********")
    (let ((input-1 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day22-part-1")
                       (buffer-substring (point-min)
                                         (point-max)))))
          (input-2 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day22-part-1")
                       (buffer-substring (point-min)
                                         (point-max))))))
      (message "Part 1: %s" (day22-part-1 input-1))
      (message "Part 2: %s\n" (day22-part-2 input-2))
      ;; (message "Results: %s"
      ;;                (benchmark-run 100 (let* ((test-input    "depth: 510
      ;; target: 10,10")
      ;;                                          (test-computed (day22-part-2 test-input))
      ;;                                          (test-ans      45))
      ;;                                     (message "Expected: %s\n    Got:      %s" test-ans test-computed))))
      )))

(provide 'day22)
;;; day22 ends here
