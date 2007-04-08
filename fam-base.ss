;; fam-base.ss -- FAM generics and basic event objects

;; Copyright (C) 2007 by Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sat Mar 24, 2007 23:41

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

(module fam-base (lib "swindle.ss" "swindle")

  (provide (all-from (lib "swindle.ss" "swindle")))

  (provide fam-monitor-path
           fam-monitored-paths
           fam-suspend-path-monitoring
           fam-resume-path-monitoring
           fam-cancel-path-monitoring
           fam-any-event?
           fam-next-event
           fam-pending-events
           fam-release

           fam-event-type->string
           fam-event-path
           fam-event-monitored-path
           fam-event-type
           fam-event-timestamp
           <fam-event>)

  (defgeneric (fam-release fc))
  (defgeneric (fam-monitor-path fc path))
  (defgeneric (fam-monitored-paths fc))
  (defgeneric (fam-suspend-path-monitoring fc path))
  (defgeneric (fam-resume-path-monitoring fc path))
  (defgeneric (fam-cancel-path-monitoring fc path))
  (defgeneric (fam-any-event? fc))
  (defgeneric (fam-next-event fc &optional wait))
  (defgeneric (fam-pending-events fc))

  (defclass <fam-event> () path type monitored-path timestamp :auto #t)
  (defmethod (print-object (fev <fam-event>) esc? port)
    (print-object-with-slots fev esc? port))

  (define (fam-event-type->string type)
    (define descs '((fam-event-created . "Created")
                    (fam-event-found . "Found")
                    (fam-event-eol . "End of found files list")
                    (fam-event-moved . "Moved")
                    (fam-event-modified . "Modified")
                    (fam-event-deleted . "Deleted")
                    (fam-event-exec-start . "Execution started")
                    (fam-event-exec-stop . "Execution stopped")
                    (fam-event-acknowledge . "Acknowledge")))
    (cond ((assoc type descs) => cdr)
          (else (format "Unknown type (~A)" type))))
)


;;; fam-base.ss ends here
