;; fam-task.ss -- Utilities on top of fam

;; Copyright (C) 2007 by Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Fri Mar 23, 2007 23:44

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

(module fam-task "fam-base.ss"

  (provide fam-task-create
           fam-task-start
           fam-task-join
           fam-task-suspend
           fam-task-resume
           fam-task-stop
           fam-task-add-path
           fam-task-remove-path
           fam-task-suspend-monitoring
           fam-task-resume-monitoring
           fam-task-monitored-paths

           fam-event-path
           fam-event-type
           fam-event-monitored-path
           fam-event-type->string

           use-native-fam?)

  (require "fam.ss"
           "mz-fam.ss"
           (lib "list.ss")
           (lib "async-channel.ss"))

  (define use-native-fam? (make-parameter (not (fam-available?))
                                          (lambda (v) (or (not (fam-available?)) v))))

  (define-struct fam-task (thread channel fc fspecs period))

  (define (fspec-path fs) (path->string (path->complete-path (car fs))))
  (define fspec-proc cadr)
  (define (fspec-evs fs)
    (if (> (length fs) 2) (caddr fs) 'all-fam-events))

  (define (%check-fs fs)
    (when (< (length fs) 2) (error "Incomplete file-spec" fs))
    (when (not (string? (fspec-path fs)))
      (error "Pathname expected" (fspec-path fs)))
    (when (not (procedure? (fspec-proc fs)))
      (error "Procedure expected" (fspec-proc fs)))
    (when (and (> 2 (length fs)) (not (list? (fspec-evs fs))))
      (error "Event list expected" (fspec-evs fs))))

  (define (%process-events events fspecs)
    (let loop ((events events))
      (when (not (null? events))
        (let* ((event (car events))
               (type (fam-event-type event))
               (mp (fam-event-monitored-path event))
               (fs (assoc mp fspecs)))
          (when (and fs (or (eq? 'all-fam-events (fspec-evs fs))
                            (memq type (fspec-evs fs))))
            ((fspec-proc fs) event)))
        (loop (cdr events)))))

  (define (%process-msg msg fc fspecs)
    (case (car msg)
      ((add)
       (fam-monitor-path fc (fspec-path (cdr msg)))
       (cons (cdr msg) fspecs))
      ((remove)
       (remove (cdr msg) fspecs (lambda (it fs) (equal? it (fspec-path fs)))))
      ((suspend) (fam-suspend-path-monitoring fc (cdr msg)) fspecs)
      ((resume) (fam-resume-path-monitoring fc (cdr msg)) fspecs)
      (else fspecs)))

  (define (%process-msgs ft k)
    (let loop ((msg (async-channel-try-get (fam-task-channel ft))))
      (when msg
        (when (eq? msg 'exit)
          (%close ft)
          (k 'exit))
        (set-fam-task-fspecs! ft (%process-msg msg
                                               (fam-task-fc ft)
                                               (fam-task-fspecs ft)))
        (loop (async-channel-try-get (fam-task-channel ft))))))

  (define (%close ft)
    (set-fam-task-thread! ft #f)
    (fam-release (fam-task-fc ft))
    (set-fam-task-fc! ft #f))

  (define (%periodic-loop ft k)
    (let loop ()
      (sleep (fam-task-period ft))
      (%process-events (fam-pending-events (fam-task-fc ft))
                       (fam-task-fspecs ft))
      (%process-msgs ft k)
      (loop)))

  (define (%blocking-loop ft k)
    (define (next) (fam-next-event (fam-task-fc ft) #t))
    (let loop ((event (next)))
      (%process-events (list event) (fam-task-fspecs ft))
      (%process-msgs ft k)
      (loop (next))))

  (define (%monitor ft)
    (lambda ()
      (for-each (lambda (fspec) (fam-monitor-path (fam-task-fc ft) (fspec-path fspec)))
                (fam-task-fspecs ft))
      (call/cc
       (lambda (k)
         ((if (> (fam-task-period ft) 0) %periodic-loop %blocking-loop) ft k)))))

  (define fam-task-create
    (case-lambda
      (() (fam-task-create 0.01))
      ((period) (make-fam-task #f (make-async-channel) #f '() period))))

  (define (fam-task-start ft)
    (and (not (fam-task-thread ft))
         (let ((fc (or (and (not (use-native-fam?)) (make-fam))
                       (make-mz-fam))))
           (and fc
                (begin (set-fam-task-fc! ft fc)
                       (set-fam-task-thread! ft (thread (%monitor ft)))
                       #t)))))

  (define (fam-task-monitored-paths ft)
    (map fspec-path (fam-task-fspecs ft)))

  (define (%fam-task-proc proc)
    (lambda (ft)
      (and (thread? (fam-task-thread ft))
           (proc (fam-task-thread ft)))))

  (define fam-task-running? (%fam-task-proc thread-running?))
  (define fam-task-suspend (%fam-task-proc thread-suspend))
  (define fam-task-resume (%fam-task-proc thread-resume))

  (define (fam-task-dead? ft)
    (or (not (fam-task-thread ft))
        ((%fam-task-proc thread-dead?) ft)))

  (define (%send-msg ft msg)
    (async-channel-put (fam-task-channel ft) msg))

  (define (fam-task-stop ft)
    (when (not (fam-task-dead? ft))
      (%send-msg ft 'exit)
      (set-fam-task-thread! ft #f)))

  (define (fam-task-join ft)
    (when (or (fam-task-thread ft)
              (fam-task-start ft))
      (thread-wait (fam-task-thread ft))))

  (define (fam-task-suspend-monitoring ft path)
    (%send-msg ft (cons 'suspend path)))

  (define (fam-task-resume-monitoring ft path)
    (%send-msg ft (cons 'resume path)))

  (define (fam-task-add-path ft . fspec)
    (%check-fs fspec)
    (if (not (fam-task-thread ft))
        (set-fam-task-fspecs! ft (cons fspec (fam-task-fspecs ft)))
        (%send-msg ft (cons 'add fspec))))

  (define (fam-task-remove-path ft path)
    (%send-msg ft (cons 'remove path)))

)


;;; fam-task.ss ends here
