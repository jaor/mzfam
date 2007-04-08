;; fam-mz.ss -- Pure-scheme fam substitution

;; Copyright (C) 2007 by Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sat Mar 24, 2007 21:10

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

;;; Code:

(module fam-mz "fam-base.ss"

  (provide make-mz-fam)

  (require "fam-utils.ss"
           "fam-base.ss"
           (lib "file.ss"))

  (defclass <mz-fam> () (files :initvalue '()) (events :initvalue '()) :auto #t)

  (defclass <monitored-file> ()
    path
    (mod-time :initvalue 0)
    (last-event :initvalue 'fam-event-new)
    (enabled? :initvalue #t)
    (events :initvalue '())
   :autoaccessors #t :autoinitargs #t)

  (defclass <monitored-folder> (<monitored-file>) (children :initvalue '())
   :autoaccessors #t :autoinitargs #t)

  (defclass <monitored-child> (<monitored-file>) :auto #t)

  (defmethod (%next-event (mc <monitored-child>))
    (let ((event (call-next-method)))
      (and event
           (not (eq? (fam-event-type event) 'fam-event-eol))
           event)))

  (defmethod (%next-event (mf <monitored-file>))
    (and (monitored-file-enabled? mf)
         (let* ((path (monitored-file-path mf))
                (fx? (or (file-exists? path) (directory-exists? path)))
                (mt (if fx? (file-or-directory-modify-seconds path) 0))
                (omt (monitored-file-mod-time mf))
                (lev (monitored-file-last-event mf))
                (nev (if (not fx?)
                         (case lev
                           ((fam-event-new fam-event-deleted) 'fam-event-null)
                           ((fam-event-null) (if (= 0 omt)
                                          'fam-event-null
                                          'fam-event-deleted))
                           (else 'fam-event-deleted))
                         (case lev
                           ((fam-event-new) 'fam-event-found)
                           ((fam-event-found) 'fam-event-eol)
                           ((fam-event-added) 'fam-event-created)
                           (else (cond ((= 0 omt) 'fam-event-created)
                                       ((= mt omt) 'fam-event-null)
                                       (else 'fam-event-modified)))))))
           (unless (eq? lev nev) (set-monitored-file-last-event! mf nev))
           (unless (= omt mt) (set-monitored-file-mod-time! mf mt))
           (and (not (eq? nev 'fam-event-null))
                (make <fam-event> :path path
                                  :type nev
                                  :monitored-path path
                                  :timestamp mt)))))

  (define (%path->mf pathname)
    (if (is-file-path? pathname)
        (make <monitored-file> :path pathname)
        (make <monitored-folder> :path pathname)))

  (defmethod (%pending-events (mf <monitored-file>))
    (let* ((events (let loop ((event (%next-event mf)) (events '()))
                     (if (not event)
                         (reverse events)
                         (loop (%next-event mf) (cons event events)))))
           (events (append (monitored-file-events mf) events))
           (enabled? (monitored-file-enabled? mf)))
      (set-monitored-file-events! mf (if enabled? '() events))
      (if enabled? events '())))

  (define (%refresh-children! mf &optional (init #f))
    (let* ((path (monitored-file-path mf))
           (children (monitored-folder-children mf))
           (paths (map monitored-file-path children)))
      (when (directory-exists? path)
        (parameterize ((current-directory path))
          (let ((nc (fold-files
                     (lambda (fn type acc)
                       (let* ((fn (path->string (path->complete-path fn path)))
                              (mt (file-or-directory-modify-seconds fn))
                              (acc (if (member fn paths)
                                       acc
                                       (cons (make <monitored-child>
                                              :path fn
                                              :mod-time mt
                                              :last-event (if init
                                                              'fam-event-new
                                                              'fam-event-added))
                                             acc))))
                         (if (eq? type 'dir) (values acc #f) acc)))
                     '())))
            (set-monitored-folder-children! mf (append nc children)))))))

  (defaftermethod (initialize (mf <monitored-folder>) initargs)
    (%refresh-children! mf #t))

  (defmethod (%pending-events (mf <monitored-folder>))
    (if (monitored-file-enabled? mf)
        (begin
          (%refresh-children! mf)
          (let ((nevents (mappend! %pending-events
                                   (monitored-folder-children mf)))
                (mfolder (monitored-file-path mf)))
            (for-each (lambda (event)
                        (slot-set! event 'monitored-path mfolder))
                      nevents)
            (if (eq? (monitored-file-last-event mf) 'fam-event-new)
                (let ((end (make <fam-event> :path mfolder
                                             :monitored-path mfolder
                                             :type 'fam-event-eol
                                             :timestamp (current-seconds))))
                  (set-monitored-file-last-event! mf 'fam-event-null)
                  (append nevents (list end)))
                nevents)))
        '()))

  (defmethod (%pending-events (fc <mz-fam>))
    (sort! (mappend! %pending-events (mz-fam-files fc))
           (lambda (e1 e2) (< (fam-event-timestamp e1)
                         (fam-event-timestamp e2)))))

  (defmethod (%find-path (mt <monitored-file>) (path <string>))
    (and (string=? (monitored-file-path mt) path)
         mt))

  (defmethod (%find-path (mt <monitored-folder>) (path <string>))
    (or (call-next-method)
        (some (lambda (ch) (%find-path ch path)) (monitored-folder-children mt))))

  (defmethod (%find-path (fc <mz-fam>) (path <string>))
    (some (lambda (mt) (%find-path mt path)) (mz-fam-files fc)))

  (defmethod (fam-release (fc <mz-fam>)) #t)

  (defmethod (fam-monitor-path (fc <mz-fam>) path)
    (set-mz-fam-files! fc (cons (%path->mf path) (mz-fam-files fc))))

  (defmethod (fam-monitored-paths (fc <mz-fam>))
    (map monitored-file-path (mz-fam-files fc)))

  (define (%enable-path fc path enable?)
    (let ((mt (%find-path fc path)))
      (and mt (begin (set-monitored-file-enabled?! mt enable?) #t))))

  (defmethod (fam-suspend-path-monitoring (fc <mz-fam>) path)
    (%enable-path fc path #f))

  (defmethod (fam-resume-path-monitoring (fc <mz-fam>) path)
    (%enable-path fc path #t))

  (defmethod (fam-cancel-path-monitoring (fc <mz-fam>) path)
    (set-mz-fam-files! fc (remove path
                                  (mz-fam-files fc)
                                  (lambda (mt)
                                    (string=? (monitored-file-path mt) path)))))

  (defmethod (fam-any-event? (fc <mz-fam>))
    (when (null? (mz-fam-events fc))
      (set-mz-fam-events! fc (%pending-events fc)))
    (not (null? (mz-fam-events fc))))

  (defmethod (fam-next-event (fc <mz-fam>) &optional (wait #f))
    (let ((event (and (fam-any-event? fc)
                      (car (mz-fam-events fc)))))
      (when event
        (set-mz-fam-events! fc (cdr (mz-fam-events fc))))
      (if (or event (not wait))
          event
          (begin
            (sleep 0.01)
            (fam-next-event fc wait)))))

  (defmethod (fam-pending-events (fc <mz-fam>))
    (let ((events (append (mz-fam-events fc) (%pending-events fc))))
      (set-mz-fam-events! fc '())
      events))

  (defaftermethod (initialize (fc <mz-fam>) initargs)
    (set-mz-fam-events! fc (%pending-events fc)))
)

;;; mz-fam.ss ends here
