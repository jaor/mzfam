#!/bin/sh
#|
exec mzscheme -r "$0" "$@"
|#

;;; Code: -*- scheme -*-

(require (planet "fam-task.ss" ("jao" "mzfam.plt" 1 0))
         (lib "cmdline.ss"))

(define period 0.01)
(define recursive #f)

(command-line
 "xosd-monitor.ss" (current-command-line-arguments)
 (once-each
  (("-n" "--native") "Use native implementation" (use-native-fam? #t))
  (("-r" "--recursive") "Recursively monitor subdirs" (set! recursive #t)))
 (once-any
  (("-b" "--block") "Block on next event" (set! period 0))
  (("-p" "--period") p "Polling with given period" (set! period p))))

(define ft (fam-task-create period))

(unless (fam-task-start ft)
  (error "Could not start monitoring task"))

(printf "Monitoring using ~A started~%"
        (if (fam-use-native?) "scheme FAM" "FAM/Gamin daemon"))

(define (display-event event)
  (printf "* ~A: ~A (~A)~%"
          (fam-event-path event)
          (fam-event-type->string (fam-event-type event))
          (fam-event-monitored-path event)))

(define (read-op)
  (printf "(a)dd, (r)emove, (s)uspend, r(e)sume, (p)rint, (q)uit: ")
  (let ((op (read))) (read-line) op))

(define (read-path) (printf "Path: ") (read-line))

(let loop ((op (read-op)))
  (if (case op
        ((p) (display (fam-task-monitored-paths ft)) (newline) #t)
        ((a) (fam-task-add-path ft (read-path) display-event))
        ((r) (fam-task-remove-path ft (read-path)))
        ((s) (fam-task-suspend-monitoring ft (read-path)))
        ((e) (fam-task-resume-monitoring ft (read-path)))
        (else #f))
      (display "OK")
      (display "KO"))
  (newline)
  (if (eq? op 'q) (fam-task-stop ft) (loop (read-op))))

