;;; day23 --- My solution to day23 -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day23

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defvar run-from-batch nil
  "Whether this was run froom batch.

We only want long running executions to be done from the terminal
so that Emacs doesn't hang.")

(require 'subr-x)

(defun parse-bots (input-file)
  "Parse the bots from INPUT-FILE."
  (thread-last (split-string input-file "\n" t " ")
    (cl-mapcar (lambda (line) (thread-last (split-string line "[^0-9-]" t " ")
                                (cl-mapcar #'string-to-number))))))

(defun manhattan-distance-within (bot-1 bot-2)
  "Produce t if BOT-1's signal extends to include BOT-2."
  (<= (+ (abs (- (car bot-1) (car bot-2)))
         (abs (- (cadr bot-1) (cadr bot-2)))
         (abs (- (caddr bot-1) (caddr bot-2))))
      (cadddr bot-1)))

(require 'map)
(require 'seq)

(defun day23-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (let* ((bots           (parse-bots input-file))
         (largest-radius (thread-first (seq-subseq bots 0)
                           (cl-sort #'> :key #'cadddr)
                           (car))))
    (cl-loop for bot in bots
       count (manhattan-distance-within largest-radius bot))))

(let* ((test-input    "pos=<0,0,0>, r=4
pos=<1,0,0>, r=1
pos=<4,0,0>, r=3
pos=<0,2,0>, r=1
pos=<0,5,0>, r=3
pos=<0,0,3>, r=1
pos=<1,1,1>, r=1
pos=<1,1,2>, r=1
pos=<1,3,1>, r=1")
       (test-computed (day23-part-1 test-input))
       (test-ans      7))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; 932: incorrect

;; # PART 2:

(defun spheres-intersect (sphere-1 sphere-2)
  "Produce t if SPHERE-1 intersects with SPHERE-2."
  (let* ((distance-between-centres (+ (abs (- (car sphere-1) (car sphere-2)))
                                      (abs (- (cadr sphere-1) (cadr sphere-2)))
                                      (abs (- (caddr sphere-1) (caddr sphere-2))))))
    (<= distance-between-centres
        (+ (cadddr sphere-1)
           (cadddr sphere-2)))))

(defun count-intersections (spheres sphere)
  "Produce the number of SPHERES which SPHERE interests with."
  (cl-count-if (apply-partially #'spheres-intersect sphere) spheres))

(defun most-intersections (spheres)
  "Produce the sphered which intersects with the most in SPHERES.

Also produce the count of spheres which it intersected with."
  (cl-loop for sphere in (cdr spheres)
     with best-count  = (count-intersections spheres (car spheres))
     with best-sphere = (car spheres)
     for  next-count  = (count-intersections spheres sphere)
     when (> next-count best-count)
       do (setq best-count  next-count
                best-sphere sphere)
     finally return (cons best-count best-sphere)))

(defun manhattan-distance (point-1 point-2)
  "Produce the manhattan distance between POINT-1 and POINT-2."
  (+ (abs (- (car   point-1) (car   point-2)))
     (abs (- (cadr  point-1) (cadr  point-2)))
     (abs (- (caddr point-1) (caddr point-2)))))

(defun sphere-contains (sphere point)
  "Produce t if SPHERE contains POINT."
  (<= (+ (abs (- (car sphere) (car point)))
         (abs (- (cadr sphere) (cadr point)))
         (abs (- (caddr sphere) (caddr point))))
      (cadddr sphere)))

(defun count-containing-spheres (spheres point)
  "Produce the number of SPHERES which contain POINT."
  (cl-count-if (lambda (sphere) (sphere-contains sphere point)) spheres))

(defun distance-to-origin (point)
  "Produce the distance to the origin from POINT."
  (+ (abs (car   point))
     (abs (cadr  point))
     (abs (caddr point))))

(defun all-intersecting-spheres (spheres sphere)
  "Produce the SPHERES which intersect with SPHERE."
  (cl-remove-if-not (apply-partially #'spheres-intersect sphere) spheres))

(defun one-further-from-zero (x)
  "Produce the X which is one closer to zero."
  (if (< x 0)
      (1- x)
      (if (eq 0 x)
          x
          (1+ x))))

(defun one-closer-to-zero (x)
  "Produce the X which is one closer to zero."
  (if (< x 0)
      (1+ x)
      (if (eq 0 x)
          x
          (1- x))))

(defun closer-heads (head)
  "Produe the points which are closer to the origin from HEAD."
  (pcase head
    (`(,dist ,x ,y ,z)
      (list (list (1- dist) (one-closer-to-zero x) y                      z)
            (list (1- dist) x                      (one-closer-to-zero y) z)
            (list (1- dist) x                      y                      (one-closer-to-zero z))
            (list (1+ dist) (one-further-from-zero x) y                         z)
            (list (1+ dist) x                         (one-further-from-zero y) z)
            (list (1+ dist) x                         y                         (one-further-from-zero z))))))

(defun expand-search (heads seen)
  "Produce the new search points from HEADS.

Points which have been SEEN are not added."
  (cl-loop for head in heads
     for new-heads    = (closer-heads head)
     for without-seen = (cl-delete-if (lambda (new-head)
                                        (not (when (null (map-elt seen (cdr new-head)))
                                               (setf (map-elt seen (cdr new-head))
                                                     (car new-head)))))
                           new-heads)
     append without-seen))

(defun find-most-central-sphere (spheres)
  "Produce the sphere which is most centred in SPHERES."
  (let* ((best-intersection (most-intersections spheres))
         (intersecting      (all-intersecting-spheres spheres (cdr best-intersection))))
    (if (eq (length spheres) 1)
        (car spheres)
        (find-most-central-sphere (cl-delete (cdr best-intersection) intersecting)))))

(defun day23-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (let* ((bots              (parse-bots input-file))
         (best-intersection (most-intersections bots))
         (containing-sphere (cdr best-intersection))
         (intersecting      (all-intersecting-spheres bots containing-sphere))
         (most-central      (find-most-central-sphere (cl-subseq intersecting 0)))
         (min-x             (thread-first (cl-mapcar #'car   intersecting)
                              (cl-sort #'<)
                              (car)))
         (max-x             (thread-first (cl-mapcar #'car   intersecting)
                              (cl-sort #'>)
                              (car)))
         (min-y             (thread-first (cl-mapcar #'cadr  intersecting)
                              (cl-sort #'<)
                              (car)))
         (max-y             (thread-first (cl-mapcar #'cadr  intersecting)
                              (cl-sort #'>)
                              (car)))
         (min-z             (thread-first (cl-mapcar #'caddr intersecting)
                              (cl-sort #'<)
                              (car)))
         (max-z             (thread-first (cl-mapcar #'caddr intersecting)
                              (cl-sort #'>)
                              (car)))
         (starting-point    (list (/ (+ min-x max-x) 2)
                                  (/ (+ min-y max-y) 2)
                                  (/ (+ min-z max-z) 2))))
    (message "Best intersection: %s" best-intersection)
    (message "Most central: %s" most-central)
    (cl-loop repeat 5
       with best-count = (count-containing-spheres intersecting starting-point)
       with seen       = (make-hash-table :test #'equal)
       with heads      = (list (cons (distance-to-origin starting-point) starting-point))
       do (setq heads (expand-search heads seen))
       do (setq heads
                (cl-loop for head in heads
                   for  next-count = (count-containing-spheres intersecting head)
                   when (> next-count best-count)
                     do (setq best-count next-count)
                   collect (cons next-count head)))
       do (setq heads
                (cl-loop for (count . head) in heads
                   when (>= count best-count)
                     collect head))
          ;; do (setq heads
          ;;          (cl-sort heads #'< :key (lambda (head)
          ;;                                    (manhattan-distance (cdr head)
          ;;                                                        starting-point))))
       do (message "Heads: %s" heads)
       while (> (length heads) 1)
       finally return (distance-to-origin (car heads)))))

(let* ((test-input    "pos=<10,12,12>, r=2
pos=<12,14,12>, r=2
pos=<16,12,12>, r=4
pos=<14,14,14>, r=6
pos=<50,50,50>, r=200
pos=<10,10,10>, r=5")
       (test-computed (day23-part-2 test-input))
       (test-ans      0))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; Run the solution:

(when run-from-batch
  (progn
    (message "\n********** OUTPUT **********")
    (let ((input-1 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day23-part-1")
                       (buffer-substring (point-min)
                                         (point-max)))))
          (input-2 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day23-part-1")
                       (buffer-substring (point-min)
                                         (point-max))))))
      (message "Part 1: %s" (day23-part-1 input-1))
      (message "Part 2: %s\n" (day23-part-2 input-2)))))

(provide 'day23)
;;; day23 ends here
