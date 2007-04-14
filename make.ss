#!/bin/sh
#|
exec mzscheme -r "$0" "$@"
|#

(require (lib "make.ss" "make")
         (lib "tex2page.ss" "tex2page")
         (lib "file.ss")
         (lib "process.ss")
         (lib "util.ss" "planet"))

(define major-version 1)
(define minor-version 2)

(define doc-dir "doc/")
(define html-dir "html/")
(define main-html (build-path html-dir "mzfam.html"))

(define doc-files
  (cons "doc/mzfam.png"
        (map (lambda (f) (string-append doc-dir f ".tex"))
             '("mzfam" "intro" "fam-task" "fam-task-ref" "fam"))))

(define (dd dir)
  (if (directory-exists? dir) (delete-directory/files dir)))

(define make-docs
  (lambda ()
    (dd html-dir)
    (parameterize ((current-directory "doc"))
      (tex2page "mzfam")
      (tex2page "mzfam")
      (copy-file "mzfam.png" "../html/mzfam.png"))))

(define scm-files
  (map (lambda (f) (string-append f ".ss"))
       '("fam" "fam-mz" "file-utils" "fam-base" "fam-task" "info")))

(define dist-dir "dist/")
(define make-dist
  (lambda ()
    (dd dist-dir)
    (make-directory dist-dir)
    (for-each (lambda (f) (copy-file f (build-path dist-dir f)))
              (cons "license.txt" scm-files))
    (copy-directory/files html-dir
                          (build-path dist-dir html-dir))
    (copy-directory/files "examples"
                          (build-path dist-dir "examples"))))

(define planet-file "mzfam.plt")
(define make-planet
  (lambda ()
    (make-planet-archive (path->complete-path dist-dir)
                         (path->complete-path planet-file))))

(define clean
  (lambda ()
    (dd dist-dir)
    (dd html-dir)
    (if (file-exists? planet-file) (delete-file planet-file))))

(define (assoc-plt)
  (add-hard-link "jao" planet-file major-version minor-version
                 (current-directory)))

(define (unassoc-plt)
  (remove-hard-link "jao" planet-file major-version minor-version))

(define (install)
  (system (format "planet --file ~A jao ~A ~A"
                  planet-file major-version minor-version)))

(define (uninstall)
  (remove-pkg "jao" planet-file major-version minor-version))

(make/proc `((,main-html ,doc-files ,make-docs)
             ("doc" (,main-html))
             ("dist" ("doc" ,@scm-files) ,make-dist)
             (,planet-file ("dist") ,make-planet)
             ("planet" (,planet-file))
             ("clean" () ,clean)
             ("assoc" () ,assoc-plt)
             ("unassoc" () ,unassoc-plt)
             ("install" ("planet") ,install)
             ("uninstall" () ,uninstall))
           (current-command-line-arguments))
