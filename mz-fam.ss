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

  (defclass <mz-fam> () files events :auto #t)

  (defclass <monitored-file> ()
    path mod-time (last-event :init-form 'FAMNew)
   :autoaccessors #t :autoinitargs #t)

  (defclass <monitored-folder> (<monitored-file>) children
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
                      ((FAMNew FAMNull FAMDeleted) 'FAMNull)
                      (else 'FAMDeleted))
                    (case lev
                      ((FAMNew) 'FAMExists)
                      ((FAMExists) 'FAMEndExist)
                      ((FAMNull) (if (= 0 omt) 'FAMCreated 'FAMNull))
                      (else (if (= mt omt) 'FAMNull 'FAMChanged))))))
      (unless (eq? lev nev) (set-monitored-file-last-event! mf nev))
      (unless (= omt mt) (set-monitored-file-mod-time! mf mt))
      (and (not (eq? nev 'FAMNull))
           (make-mz-fam-event :path path :type nev :time mt))))
)



;;; mz-fam.ss ends here
