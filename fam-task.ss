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

           fam-use-native?)

  (require "fam-utils.ss"
           "fam.ss"
           "fam-mz.ss"
           (lib "async-channel.ss"))

  (define fam-use-native? (make-parameter (not (fam-available?))
                                          (lambda (v) (or (not (fam-available?)) v))))

  (define-struct fam-task (thread channel fc fspecs period))
  (define-struct fspec (proc evs rec))

  (define (%accepts-type fspec type)
    (and fspec
         (let ((evs (fspec-evs fspec)))
           (or (eq? evs 'all-fam-events)
               (memq type evs)))))

  (define (%process-events events ft)
    (let ((fc (fam-task-fc ft))
          (fspecs (fam-task-fspecs ft)))
      (let loop ((events events))
        (when (not (null? events))
          (let* ((event (car events))
                 (type (fam-event-type event))
                 (mp (fam-event-monitored-path event))
                 (path (fam-event-path event))
                 (fs (hash-table-get fspecs mp #f)))
            (when fs
              (when (%accepts-type fs type) ((fspec-proc fs) event))
              (when (and (fspec-rec fs)
                         (or (eq? type 'FAMExists) (eq? type 'FAMCreated))
                         (not (string=? path mp))
                         (not (is-file-path? path)))
                (fam-monitor-path fc path)
                (hash-table-put! fspecs path fs)))
            (loop (cdr events)))))))

  (define (%process-msg msg ft)
    (let ((fc (fam-task-fc ft))
          (fspecs (fam-task-fspecs ft)))
      (case (car msg)
        ((add)
         (when (fam-monitor-path fc (cadr msg))
           (hash-table-put! fspecs (cadr msg) (caddr msg))))
        ((remove) (hash-table-remove! fspecs (cdr msg)))
        ((suspend) (fam-suspend-path-monitoring fc (cdr msg)))
        ((resume) (fam-resume-path-monitoring fc (cdr msg)))
        (else (display (format "Unexpected message: ~A~%" msg))))))

  (define (%process-msgs ft k)
    (let loop ((msg (async-channel-try-get (fam-task-channel ft))))
      (when msg
        (when (eq? msg 'exit)
          (%close ft)
          (k 'exit))
        (%process-msg msg ft)
        (loop (async-channel-try-get (fam-task-channel ft))))))

  (define (%close ft)
    (set-fam-task-thread! ft #f)
    (fam-release (fam-task-fc ft))
    (set-fam-task-fc! ft #f))

  (define (%uniquify events)
    (define (eqev? a b)
      (and (eq? (fam-event-type a) (fam-event-type b))
           (string=? (fam-event-path a) (fam-event-path b))
           (string=? (fam-event-monitored-path a) (fam-event-monitored-path b))))
    (let loop ((events events) (result '()))
      (if (null? events)
          (reverse result)
          (loop (remove (car events) (cdr events) eqev?)
                (cons (car events) result)))))

  (define (%periodic-loop ft k)
    (let loop ()
      (sleep (fam-task-period ft))
      (%process-events (%uniquify (fam-pending-events (fam-task-fc ft))) ft)
      (%process-msgs ft k)
      (loop)))

  (define (%blocking-loop ft k)
    (define (next) (fam-next-event (fam-task-fc ft) #t))
    (let loop ((event (next)))
      (%process-events (list event) ft)
      (%process-msgs ft k)
      (loop (next))))

  (define (%monitor ft)
    (lambda ()
      (hash-table-for-each (fam-task-fspecs ft)
                           (lambda (path fspec)
                             (fam-monitor-path (fam-task-fc ft) path)))
      (call/cc
       (lambda (k)
         ((if (> (fam-task-period ft) 0) %periodic-loop %blocking-loop) ft k)))))

  (define fam-task-create
    (case-lambda
      (() (fam-task-create 0.01))
      ((period) (make-fam-task #f
                               (make-async-channel)
                               #f
                               (make-hash-table 'equal)
                               period))))

  (define (fam-task-start ft)
    (and (not (fam-task-thread ft))
         (let ((fc (or (and (not (fam-use-native?)) (make-fam))
                       (make-mz-fam))))
           (and fc
                (begin (set-fam-task-fc! ft fc)
                       (set-fam-task-thread! ft (thread (%monitor ft)))
                       #t)))))

  (define (fam-task-monitored-paths ft)
    (hash-table-map (fam-task-fspecs ft) (lambda (k v) k)))

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

  (define (fam-task-add-path ft path proc
                             &optional (events #f) (recursive #f))
    (let* ((path (path->string (path->complete-path path)))
           (recursive (and recursive (not (is-file-path? path))))
           (events (if (list? events) events 'all-fam-events))
           (fspec (make-fspec proc events recursive)))
      (if (not (fam-task-thread ft))
          (hash-table-put! (fam-task-fspecs ft) path fspec)
          (%send-msg ft (list 'add path fspec)))))

  (define (fam-task-remove-path ft path)
    (%send-msg ft (cons 'remove path)))

)


;;; fam-task.ss ends here
