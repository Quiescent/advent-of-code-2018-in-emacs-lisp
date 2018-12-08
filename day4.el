;;; day4 --- My solution to day4 -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day4

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defvar run-from-batch nil
  "Whether this was run froom batch.

We only want long running executions to be done from the terminal
so that Emacs doesn't hang.")

(defun compare-entries (this that)
  "Produce t if THIS is before THAT."
  (let ((zeros-this    (nth 4 this))
        (hundreds-this (* 60 (nth 3 this)))
        (days-this     (* 1440 (nth 2 this)))
        (month-this    (* 43200 (nth 1 this)))

        (zeros-that    (nth 4 that))
        (hundreds-that (* 60 (nth 3 that)))
        (days-that     (* 1440 (nth 2 that)))
        (month-that    (* 43200 (nth 1 that))))
    (< (+ zeros-this hundreds-this days-this month-this)
       (+ zeros-that hundreds-that days-that month-that))))

(defun parse-guard-sleep-data (input-file)
  "Produce a time organised list of the guard sleep data in INPUT-FILE."
  (cl-loop for line in (split-string input-file "\n" t " ")
     with time-organised-data = '()
     for next-entry = (mapcar #'string-to-number (split-string line "[^0-9]" t " "))
     do (setq time-organised-data
              (cl-merge 'list time-organised-data (list next-entry) #'compare-entries))
     finally return time-organised-data))

(require 'subr-x)

(defun get-time (entry)
  "Produce the time in ENTRY."
  (cons (nth 3 entry)
        (nth 4 entry)))

(defun time-diff (start end)
  "Produce the difference in minutes between START and END.

Times before the 00 hour are truncated to 00."
  (let ((adjusted-start (if (not (eq (car start) 0))
                            '(0 0)
                            start)))
    (pcase (cons start end)
      (`((,h1 . ,m1) . (,h2 . ,m2))
        (- m2 m1)))))

(defun day4-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (let ((time-organised (parse-guard-sleep-data input-file)))
    (cl-loop for entry in time-organised
       with guard-results = (make-hash-table :test #'eq)
       with guard-minutes = (make-hash-table :test #'eq)
       with sleeping      = nil
       with current-guard = nil
       with starting-time = nil
       if (eq (length entry) 6)
         do (progn
              (setq current-guard (car (last     entry)))
              (setq starting-time (get-time entry)))
       else if sleeping
              do (let ((current-time-asleep (gethash current-guard guard-results 0))
                       (current-minutes     (gethash current-guard guard-minutes
                                                     (make-hash-table :test #'eq)))
                       (current-time        (get-time entry)))
                   (puthash current-guard
                            (+ (time-diff starting-time current-time) current-time-asleep)
                            guard-results)
                   (cl-loop for i from (cdr starting-time) below (cdr current-time)
                      for current-minute-count = (gethash i current-minutes 0)
                      do (puthash i             (1+ current-minute-count) current-minutes)
                      do (puthash current-guard current-minutes           guard-minutes))
                   (setq sleeping nil))
       else
         do (progn
              (setq starting-time (get-time entry))
              (setq sleeping      t))
       finally return (let* ((guard-longest-asleep
                              (cl-loop for key in (hash-table-keys guard-results)
                                 with max            = nil
                                 with guard-id       = nil
                                 for  current-result = (gethash key guard-results)
                                 when (or (eq max nil)
                                          (> current-result max))
                                   do (progn
                                        (setq max      current-result)
                                        (setq guard-id key))
                                 finally return guard-id))
                             (minutes (gethash guard-longest-asleep guard-minutes)))
                        (cl-loop for i from 1 to 59
                           with max          = 0
                           with max-count    = (gethash 0 minutes 0)
                           for current-count = (gethash i minutes 0)
                           when (> current-count max-count)
                             do (progn
                                  (setq max       i)
                                  (setq max-count current-count))
                           finally return (* guard-longest-asleep max))))))

;; 55453 -- too high

;; # PART 2:

(require 'seq)

(defun find-max (table)
  "Find the maximum value in hash TABLE and produce it and it's index."
  (let ((keys (hash-table-keys table)))
    (cl-loop for key in (cdr keys)
       with max = (gethash (car keys) table)
       with idx = (car keys)
       for cur-max = (gethash key table)
       when (> cur-max max)
         do (progn
              (setq max cur-max
                    idx key))
       finally return (cons max idx))))

(defun day4-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (let ((time-organised (parse-guard-sleep-data input-file)))
    (cl-loop for entry in time-organised
       with guard-minutes = (make-hash-table :test #'eq)
       with sleeping      = nil
       with current-guard = nil
       with starting-time = nil
       if (eq (length entry) 6)
         do (progn
              (setq current-guard (car (last     entry)))
              (setq starting-time (get-time entry)))
       else if sleeping
              do (let ((current-minutes     (gethash current-guard guard-minutes
                                                     (make-hash-table :test #'eq)))
                       (current-time        (get-time entry)))
                   (cl-loop for i from (cdr starting-time) below (cdr current-time)
                      for current-minute-count = (gethash i current-minutes 0)
                      do (puthash i             (1+ current-minute-count) current-minutes)
                      do (puthash current-guard current-minutes           guard-minutes))
                   (setq sleeping nil))
       else
         do (progn
              (setq starting-time (get-time entry))
              (setq sleeping      t))
       finally return (let ((guards (hash-table-keys guard-minutes)))
                        (cl-loop for guard in (cdr guards)
                           with (max . max-idx) = (find-max (gethash (car guards) guard-minutes))
                           with max-guard = (car guards)
                           for (current-minutes . current-position) = (find-max (gethash guard guard-minutes))
                           when (> current-minutes max)
                             do (progn
                                  (setq max current-minutes)
                                  (setq max-idx current-position)
                                  (setq max-guard guard))
                           finally return (* max-guard max-idx))))))

;; 8082 is too low

;; Run the solution:

(when run-from-batch
  (progn
    (message "\n********** OUTPUT **********")
    (let ((input-1 (with-temp-buffer
                     (find-file-literally "day4-part-1")
                     (buffer-substring (point-min)
                                       (point-max))))
          (input-2 (with-temp-buffer
                     (find-file-literally "day4-part-1")
                     (buffer-substring (point-min)
                                       (point-max)))))
      (message "Part 1: %s" (day4-part-1 input-1))
      (message "Part 2: %s\n" (day4-part-2 input-2)))))

(provide 'day4)
;;; day4 ends here
