#!/bin/sh
#|
exec mzscheme -r "$0" "$@"
|#

;;; Code: -*- scheme -*-

(require (planet "fam-task.ss" ("jao" "mzfam.plt" 1 0))
         (lib "xosd.ss" "ffi")
         (lib "cmdline.ss")
         (lib "date.ss"))

(define xosd-inst (xosd-create))


;; xosd config
(xosd-set-pos xosd-inst 'middle)
(xosd-set-align xosd-inst 'center)
(xosd-set-shadow-offset xosd-inst 7)
(xosd-set-outline-offset xosd-inst 2)
(xosd-set-colour xosd-inst "yellow")
(xosd-set-shadow-colour xosd-inst "black")
(xosd-set-outline-colour xosd-inst "blue")
(xosd-set-font xosd-inst "-adobe-courier-bold-r-*-*-20-*-*-*-*-*-*-*")

;; fam config
(define (display-event event)
  (let ((msg (format "~A: ~A (~A) - ~A"
                     (fam-event-path event)
                     (fam-event-type->string (fam-event-type event))
                     (fam-event-monitored-path event)
                     (date->string (seconds->date (fam-event-timestamp event))
                                   #t))))
    (xosd-display-string xosd-inst msg)
    (sleep 2)
    (xosd-hide xosd-inst)))

(define mfiles '())
(define recursive #f)

(command-line
 "xosd-monitor.ss" (current-command-line-arguments)
 (once-each
  (("-s" "--scheme") "Use Scheme implementation" (fam-use-native? #f))
  (("-r" "--recursive") n "Recursive monitor level"
   (set! recursive (string->number n))))
 (once-any
  (("-b" "--block") "Block on next event" (fam-task-default-period 0))
  (("-p" "--period") p "Polling with given period"
   (fam-task-default-period (string->number p))))
 (args files
       (when (null? files) (error "No monitored files/directories"))
       (set! mfiles files)))

(define fam-inst (fam-task-create display-event))

(for-each (lambda (path)
            (fam-task-add-path fam-inst path #f #f recursive))
          mfiles)

(fam-task-join fam-inst)
