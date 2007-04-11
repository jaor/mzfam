#!/bin/sh
#|
exec mzscheme -r "$0" "$@"
|#

(require (lib "make.ss" "make")
         (lib "tex2page.ss" "tex2page")
         (lib "file.ss")
         (lib "util.ss" "planet"))

(define doc-dir "doc/")

(define doc-files 
  (map (lambda (f) (string-append doc-dir f ".tex")) 
       '("mzfam" "intro" "fam-task" "fam-task-ref" "fam")))

(define make-docs
  (lambda () 
    (parameterize ((current-directory "doc")) 
      (tex2page "mzfam")
      (copy-file "mzfam.png" "doc/mzfam.png"))))

(define html-dir (build-path doc-dir "html/"))
(define main-html (build-path html-dir "mzfam.html"))

(define scm-files 
  (map (lambda (f) (string-append f ".ss"))
       '("fam" "fam-mz" "fam-utils" "fam-base" "fam-task" "info")))

(define dist-dir "dist/")
(define make-dist
  (lambda () 
    (if (directory-exists? dist-dir) (delete-directory/files dist-dir))
    (make-directory dist-dir)
    (for-each (lambda (f) (copy-file f (build-path dist-dir f)))
              scm-files)
    (copy-directory/files (build-path "doc" "html") 
                          (build-path dist-dir "html"))))

(define planet-file "mzfam.plt")
(define make-planet
  (lambda ()
    (make-planet-archive (path->complete-path dist-dir) 
                         (path->complete-path planet-file))))

(define clean
  (lambda ()
    (delete-directory/files dist-dir)
    (delete-directory/files html-dir)))

(make/proc `((,main-html ,doc-files ,make-docs)
             ("doc" (,main-html))
             ("dist" ("doc" ,@scm-files) ,make-dist)
             (,planet-file ("dist") ,make-planet)
             ("planet" (,planet-file))
             ("clean" () ,clean))
           (current-command-line-arguments))
