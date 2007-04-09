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
           fam-task-running?
           fam-task-add-path
           fam-task-remove-path
           fam-task-suspend-monitoring
           fam-task-resume-monitoring
           fam-task-monitored-paths
           fam-task-default-period

           fam-event-path
           fam-event-type
           fam-event-monitored-path
           fam-event-timestamp
           fam-event-type->string
           fam-make-event-stream

           fam-use-native?)

  (require "fam-utils.ss"
           "fam.ss"
           "fam-mz.ss"
           (lib "async-channel.ss")
           (only (lib "list.ss" "srfi" "1") delete-duplicates!))

  (define fam-use-native?
    (make-parameter (fam-available?)
                    (lambda (v) (and (fam-available?) v))))

  (define fam-task-default-period
    (make-parameter 0.1 (lambda (v) (and (number? v) v))))

  (define-struct fam-task (thread channel fc fspecs period def-proc))
  (define-struct fspec (proc evs rec))

  (define (%accepts-type fspec type)
    (and fspec
         (let ((evs (fspec-evs fspec)))
           (or (eq? evs 'all-fam-events)
               (memq type evs)))))

  (define (%fspec-rec? fspec)
    (let ((rec (fspec-rec fspec)))
      (or (eq? rec #t)
          (and (number? rec) (> rec 0)))))

  (define (%fspec-next fs)
    (let ((rec (fspec-rec fs)))
      (cond ((eq? rec #t) fs)
            (else (make-fspec (fspec-proc fs) (fspec-evs fs) (- rec 1))))))

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
              (when (%accepts-type fs type)
                ((or (fspec-proc fs) (fam-task-def-proc ft)) event))
              (when (and (%fspec-rec? fs)
                         (or (eq? type 'fam-event-found)
                             (eq? type 'fam-event-created))
                         (not (string=? path mp))
                         (not (is-file-path? path)))
                (fam-monitor-path fc path)
                (hash-table-put! fspecs path (%fspec-next fs))))
            (loop (cdr events)))))))

  (define (%process-msg msg ft)
    (let ((fc (fam-task-fc ft))
          (fspecs (fam-task-fspecs ft)))
      (case (car msg)
        ((add)
         (when (fam-monitor-path fc (cadr msg))
           (hash-table-put! fspecs (cadr msg) (caddr msg))))
        ((remove)
         (let ((path (hash-table-get fspecs (cdr msg) #f)))
           (when path
             (fam-cancel-path-monitoring fc path)
             (hash-table-remove! fspecs path))))
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
           (string=? (fam-event-monitored-path a)
                     (fam-event-monitored-path b))))
    (delete-duplicates! events eqev?))

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
      (let/cc k
       ((if (> (fam-task-period ft) 0) %periodic-loop %blocking-loop) ft k))))

  (define fam-task-create
    (case-lambda
      (() (fam-task-create (fam-task-default-period)))
      ((x) (cond ((number? x) (fam-task-create (lambda (e) #t) x))
                 (else (fam-task-create x (fam-task-default-period)))))
      ((x y) (make-fam-task #f
                            (make-async-channel)
                            #f
                            (make-hash-table 'equal)
                            (if (number? y) y x)
                            (if (number? y) x y)))))

  (define (fam-task-start ft)
    (and (not (fam-task-thread ft))
         (let ((fc (or (and (fam-use-native?) (make-fam))
                       (make-mz-fam))))
           (and fc
                (begin (set-fam-task-fc! ft fc)
                       (set-fam-task-thread! ft (thread (%monitor ft)))
                       #t)))))

  (define (fam-task-monitored-paths ft)
    (hash-table-map (fam-task-fspecs ft) (lambda (k v) k)))

  (define (fam-task-running? ft)
    (and (fam-task-thread ft)
         (thread-running? (fam-task-thread ft))))

  (define (%send-msg ft msg)
    (async-channel-put (fam-task-channel ft) msg))

  (define (fam-task-stop ft)
    (when (fam-task-running? ft)
      (%send-msg ft 'exit)
      (set-fam-task-thread! ft #f)))

  (define (fam-task-join ft)
    (when (or (fam-task-thread ft)
              (fam-task-start ft))
      (thread-wait (fam-task-thread ft))))

  (define (fam-task-suspend-monitoring ft path)
    (%send-msg ft (cons 'suspend (absolute-pathname path))))

  (define (fam-task-resume-monitoring ft path)
    (%send-msg ft (cons 'resume (absolute-pathname path))))

  (define (fam-task-add-path ft path
                             &optional (proc #f) (events #f) (recursive #f))
    (let* ((path (absolute-pathname path))
           (recursive (and recursive
                           (not (is-file-path? path))
                           (or (not (number? recursive))
                               (and (> recursive 0) recursive))))
           (events (if (list? events) events 'all-fam-events))
           (fspec (make-fspec (and (procedure? proc) proc) events recursive)))
      (if (not (fam-task-thread ft))
          (hash-table-put! (fam-task-fspecs ft) path fspec)
          (%send-msg ft (list 'add path fspec)))))

  (define (fam-task-remove-path ft path)
    (%send-msg ft (cons 'remove (absolute-pathname path))))

)


;;; fam-task.ss ends here
