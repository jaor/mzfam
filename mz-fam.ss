;; mz-fam.ss -- Pure-scheme fam substitution

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

(module mz-fam "fam-base.ss"

  (provide make-mz-fam)

  (require "fam-utils.ss"
           (lib "file.ss"))

  (defclass <mz-fam> () (files :initvalue '()) (events :initvalue '()) :auto #t)

  (defclass <monitored-file> ()
    path (mod-time :initvalue 0) (last-event :initvalue 'FAMNew)
   :autoaccessors #t :autoinitargs #t)

  (defclass <monitored-folder> (<monitored-file>) (children :initvalue '())
   :autoaccessors #t :autoinitargs #t)

  (defclass <mz-fam-event> (<fam-event>) time :auto #t)

  (defmethod (%next-event (mf <monitored-file>))
    (let* ((path (monitored-file-path mf))
           (fx? (file-exists? path))
           (mt (if fx? (file-or-directory-modify-seconds path) 0))
           (omt (monitored-file-mod-time mf))
           (lev (monitored-file-last-event mf))
           (nev (if (not fx?)
                    (case lev
                      ((FAMNew FAMDeleted) 'FAMNull)
                      ((FAMNull) (if (= 0 omt)
                                     'FAMNull
                                     (begin (set-monitored-file-mod-time! mf 0)
                                            'FAMDeleted)))
                      (else 'FAMDeleted))
                    (case lev
                      ((FAMNew) 'FAMExists)
                      ((FAMExists) 'FAMEndExist)
                      (else (cond ((= 0 omt) 'FAMCreated)
                                  ((= mt omt) 'FAMNull)
                                  (else 'FAMChanged)))))))
      (unless (eq? lev nev) (set-monitored-file-last-event! mf nev))
      (unless (= omt mt) (set-monitored-file-mod-time! mf mt))
      (and (not (eq? nev 'FAMNull))
           (make <mz-fam-event> :path path
                                :type nev
                                :monitored-path path
                                :time mt))))

  (define (%path->mf pathname)
    (if (is-file-path? pathname)
        (make <monitored-file> :path pathname)
        (make <monitored-folder> :path pathname)))

  (defmethod (%pending-events (mf <monitored-file>))
    (let loop ((event (%next-event mf)) (events '()))
      (if (not event) (reverse events) (loop (%next-event mf) (cons event events)))))

  (define (%refresh-children! mf)
    (let* ((path (monitored-file-path mf))
           (children (monitored-folder-children mf))
           (paths (map monitored-file-path children)))
      (when (directory-exists? path)
        (parameterize ((current-directory path))
          (let ((nc (fold-files
                     (lambda (fn type acc)
                       (let* ((fn (path->string (path->complete-path fn path)))
                              (acc (if (member fn paths)
                                       acc
                                       (cons (%path->mf fn) acc))))
                         (if (eq? type 'dir) (values acc #f) acc)))
                     '())))
            (set-monitored-folder-children! mf (append nc children)))))))

  (defmethod (%pending-events (mf <monitored-folder>))
    (let ((events (call-next-method)))
      (%refresh-children! mf)
      (sort! (append events (mappend! %pending-events (monitored-folder-children mf)))
             (lambda (e1 e2) (< (mz-fam-event-time e1) (mz-fam-event-time e2))))))

)



;;; mz-fam.ss ends here
