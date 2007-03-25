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
           fam-event-path
           fam-event-type)

  (require "fam.ss"
           "mz-fam.ss"
           (lib "list.ss"))

  (define-struct fam-task (thunk thread channel))

  (define fspec-path car)
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
               (tgt (fam-event-path event))
               (mp (fam-event-monitored-path event))
               (fs (assoc mp fspecs)))
          (when (and fs (or (eq? 'all-fam-events (fspec-evs fs))
                            (memq type (fspec-evs fs))))
            ((fspec-proc fs) tgt type)))
        (loop (cdr events)))))

  (define (%process-msg msg fc fspecs)
    (case (car msg)
      ((add)
       (fam-monitor-path (fspec-path (cdr msg)))
       (cons (cdr msg) fspecs))
      ((remove)
       (fam-cancel-path-monitoring (cdr msg))
       (remove (cdr msg) fspecs (lambda (it fs) (equal? it (fspec-path fs)))))
      ((suspend) (fam-suspend-path-monitoring (cdr msg)) fspecs)
      ((resume) (fam-resume-path-monitoring (cdr msg)) fspecs)
      (else fspecs)))

  (define fam-task-create
    (case-lambda
      ((fspecs) (fam-task-create fspecs 0.01))
      ((fspecs step)
       (for-each %check-fs fspecs)
       (let ((fspecs (map (lambda (fs)
                            (cons (path->string (path->complete-path (car fs)))
                                  (cdr fs)))
                          fspecs))
             (fc (or (make-fam) (make-mz-fam)))
             (ch (make-channel)))
         (let ((thunk (lambda ()
                        (for-each (lambda (p) (fam-monitor-path fc p))
                                  (map fspec-path fspecs))
                        (call/cc
                         (lambda (k)
                           (let loop ()
                             (sleep step)
                             (%process-events (fam-pending-events fc) fspecs)
                             (let loop ((msg (channel-try-get ch)))
                               (when msg
                                 (when (eq? (car msg) 'exit) (k 'exit))
                                 (set! fspecs (%process-msg msg fc fspecs))
                                 (loop (channel-try-get ch))))
                             (loop)))))))
           (make-fam-task thunk #f ch))))))

  (define (%fam-task-proc proc)
    (lambda (ft)
      (and (thread? (fam-task-thread ft))
           (proc (fam-task-thread ft)))))

  (define fam-task-running? (%fam-task-proc thread-running?))
  (define fam-task-dead? (%fam-task-proc thread-dead?))
  (define fam-task-suspend (%fam-task-proc thread-suspend))
  (define fam-task-resume (%fam-task-proc thread-resume))

  (define (fam-task-start ft)
    (and (not (fam-task-thread ft))
         (begin
           (set-fam-task-thread! ft (thread (fam-task-thunk ft)))
           (thread-running? (fam-task-thread ft)))))

  (define (fam-task-stop ft)
    (channel-put (fam-task-channel ft) '(exit)))

  (define (fam-task-join ft)
    (if (not (fam-task-running? ft))
        (call-in-nested-thread (fam-task-thunk ft))))

  (define (fam-task-suspend-monitoring ft path)
    (channel-put (fam-task-channel ft) (cons 'suspend path)))

  (define (fam-task-resume-monitoring ft path)
    (channel-put (fam-task-channel ft) (cons 'resume path)))

  (define (fam-task-add-path ft fspec)
    (%check-fs fspec)
    (channel-put (fam-task-channel ft) (cons 'add fspec)))

  (define (fam-task-remove-path ft path)
    (channel-put (fam-task-channel ft) (cons 'remove path)))

)


;;; fam-task.ss ends here
