#!/bin/sh
#|
exec mzscheme -r "$0" "$@"
|#

;;; Code: -*- scheme -*-

(require (planet "fam-task.ss" ("jao" "mzfam.plt" 1 0))
         (lib "xosd.ss" "ffi")
         (lib "cmdline.ss"))

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
  (let ((msg (format "~A: ~A (~A)"
                     (fam-event-path event)
                     (fam-event-type->string (fam-event-type event))
                     (fam-event-monitored-path event))))
    (xosd-display-string xosd-inst msg)
    (sleep 2)
    (xosd-hide xosd-inst)))

(define mfiles '())
(define period 0.01)
(define recursive #f)

(command-line
 "xosd-monitor.ss" (current-command-line-arguments)
 (once-each
  (("-n" "--native") "Use native implementation" (use-native-fam? #t))
  (("-r" "--recursive") "Recursively monitor subdirs" (set! recursive #t)))
 (once-any
  (("-b" "--block") "Block on next event" (set! period 0))
  (("-p" "--period") p "Polling with given period" (set! period (string->number p))))
 (args files
       (when (null? files) (error "No monitored files/directories"))
       (set! mfiles files)))

(define fam-inst (fam-task-create period))

(for-each (lambda (path)
            (fam-task-add-path fam-inst path display-event #f recursive))
          mfiles)

(fam-task-join fam-inst)
