;; fam-utils.ss -- Misc utilities

;; Copyright (C) 2007 by Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Tue Mar 27, 2007 23:15

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

(module fam-utils mzscheme
  (provide is-file-path?
           last-modification-time
           absolute-pathname)

  (require (lib "file.ss"))

  (define (is-file-path? pathname)
    (let* ((is-file? (file-exists? pathname))
           (is-dir? (and (not is-file?) (directory-exists? pathname)))
           (is-file? (if (or is-file? is-dir?) is-file? (path-only pathname))))
      is-file?))

  (define (last-modification-time pathname)
    (if (or (file-exists? pathname)
            (directory-exists? pathname))
        (file-or-directory-modify-seconds pathname)
        (current-seconds)))

  (define (absolute-pathname pathname)
    (path->string (path->complete-path pathname)))

)

;;; fam-utils.ss ends here
