#!/bin/sh
#|
exec mzscheme -r "$0" "$@"
|#

;;; Code: -*- scheme -*-

(require (planet "fam-task.ss" ("jao" "mzfam.plt" 1 1))
         (lib "cmdline.ss"))

(define recursive #f)

(command-line
 "xosd-monitor.ss" (current-command-line-arguments)
 (once-each
  (("-s" "--scheme") "Use scheme implementation" (fam-use-native? #f))
  (("-r" "--recursive") "Recursively monitor subdirs" (set! recursive #t)))
 (once-any
  (("-b" "--block") "Block on next event" (fam-task-default-period 0))
  (("-p" "--period") p "Polling with given period"
   (fam-task-default-period (string->number p)))))

(define es (fam-make-event-stream))
(define ft (fam-task-create es))

(unless (fam-task-start ft)
  (error "Could not start monitoring task"))

(printf "Monitoring using ~A started~%"
        (if (fam-use-native?) "scheme FAM" "FAM/Gamin daemon"))

(define (display-event event)
  (printf "* ~A: ~A (~A)~%"
          (fam-event-path event)
          (fam-event-type->string (fam-event-type event))
          (fam-event-monitored-path event)))

(define (show-events es)
  (let loop ((ev (es)))
    (when ev
      (display-event ev)
      (loop (es)))))

(define (read-op)
  (printf "(a)dd, (r)emove, (s)uspend, r(e)sume, (p)rint, e(v)ents, (q)uit: ")
  (let ((op (read))) (read-line) op))

(define (read-path) (printf "Path: ") (read-line))

(let loop ((op (read-op)))
  (if (case op
        ((p) (display (fam-task-monitored-paths ft)) (newline) #t)
        ((a) (fam-task-add-path ft (read-path)))
        ((r) (fam-task-remove-path ft (read-path)))
        ((s) (fam-task-suspend-monitoring ft (read-path)))
        ((e) (fam-task-resume-monitoring ft (read-path)))
        ((v) (show-events es))
        (else #f))
      (display "OK")
      (display "KO"))
  (newline)
  (if (eq? op 'q) (fam-task-stop ft) (loop (read-op))))

