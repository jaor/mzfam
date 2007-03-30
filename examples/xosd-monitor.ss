#!/bin/sh
#|
exec mzscheme -r "$0" "$@"
|#

;;; Code:

(require (lib "fam-task.ss" "fam")
         (lib "xosd.ss" "ffi"))

(define xosd-inst (xosd-create))
(define fam-inst (fam-task-create))

;; xosd config
(xosd-set-pos xosd-inst 'middle)
(xosd-set-align xosd-inst 'center)
(xosd-set-shadow-offset xosd-inst 7)
(xosd-set-outline-offset xosd-inst 2)
(xosd-set-colour xosd-inst "yellow")
(xosd-set-shadow-colour xosd-inst "black")
(xosd-set-outline-colour xosd-inst "blue")
(xosd-set-font xosd-inst "-adobe-courier-bold-r-*-*-34-*-*-*-*-*-*-*")

;; fam config
(define (display-event event)
  (let ((msg (format "~A: ~A (~A)"
                     (fam-event-path event)
                     (fam-event-type event)
                     (fam-event-monitored-path event))))
    (display msg) (newline)
    (xosd-display-string xosd-inst msg)
    (sleep 2)
    (xosd-hide xosd-inst)))

(for-each (lambda (p) (fam-task-add-path fam-inst
                                    p
                                    display-event
                                    '(FAMCreated FAMDeleted FAMChanged)))
          (vector->list (current-command-line-arguments)))

(fam-task-join fam-inst)
