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

(module fam-task mzscheme

  (provide fam-task-create
           fam-task-start
           fam-task-join
           fam-task-suspend
           fam-task-resume
           fam-task-stop
           fam-task-add-path
           fam-task-remove-path
           fam-task-suspend-monitoring
           fam-task-resume-monitoring)

  (require "fam.ss"
           (lib "list.ss"))

  (define-struct fam-task (thunk thread channel))

  (define fspec-path car)
  (define fspec-proc cadr)
  (define (fspec-xevs fs)
    (if (> (length fs) 2) (caddr fs) '()))

  (define (%check-fs fs)
    (when (< (length fs) 2) (error "Incomplet file-spec" fs))
    (when (not (string? (fspec-path fs)))
      (error "Pathname expected" (fspec-path fs)))
    (when (not (procedure? (fspec-proc fs)))
      (error "Procedure expected" (fspec-proc fs)))
    (when (not (list? (fspec-xevs fs)))
      (error "Event list expected" (fspec-xevs fs))))

  (define (%process-events events fspecs)
    (let loop ((events events))
      (when (not (null? events))
        (let ((fs (assoc (caar events) fspecs)))
          (when (and fs (not (memq (cdar events) (fspec-xevs fs))))
            ((fspec-proc fs) (fspec-path fs) (cdar events))))
        (loop (cdr events)))))

  (define (%process-msg msg fc fspecs)
    (case (car msg)
      ((exit) (exit))
      ((add)
       (fam-monitor-path (fspec-path (cdr msg)))
       (cons (cdr msg) fspecs))
      ((remove)
       (fam-cancel (cdr msg))
       (remove (cdr msg) fspecs (lambda (it fs) (equal? it (fspec-path fs)))))
      ((suspend) (fam-suspend (cdr msg)) fspecs)
      ((resume) (fam-resume (cdr msg)) fspecs)
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
             (fc (fam-open))
             (ch (make-channel)))
         (let ((thunk (lambda ()
                        (for-each (lambda (p) (fam-monitor-path fc p))
                                  (map fspec-path fspecs))
                        (let loop ()
                          (sleep step)
                          (%process-events (fam-pending-events fc)
                                           fspecs)
                          (let loop ((msg (channel-try-get ch)))
                            (when msg
                              (set! fspecs (%process-msg msg fc fspecs))
                              (loop (channel-try-get ch))))
                          (loop)))))
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
