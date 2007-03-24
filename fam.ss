(module fam mzscheme
  (provide fam-open
           fam-close
           fam-suspend
           fam-resume
           fam-cancel
           fam-monitor-path
           fam-any-event?
           fam-next-event
           fam-pending-events
           fam-event-monitored-path
           fam-event-target-path
           fam-event-type)

  (require (lib "etc.ss"))
  (require (lib "foreign.ss")) (unsafe!)

  (define (%try-ffi proc . args)
    (with-handlers ((exn:fail? (lambda (x) #f)))
      (apply proc args)))

  (define libfam (or (%try-ffi ffi-lib "libfam")
                     (%try-ffi ffi-lib "libgamin")))

  (when (not libfam) (error "Neither libfam nor libgamin is available"))

  (define _FAMCodes
    (_enum '(FAMNull = 0
             FAMChanged = 1
             FAMDeleted = 2
             FAMStartExecuting = 3
             FAMStopExecuting = 4
             FAMCreated = 5
             FAMMoved = 6
             FAMAcknowledge = 7
             FAMExists = 8
             FAMEndExist = 9)))

  (define *max-path-len* 4096)
  (define _Buffer (make-cstruct-type (build-list *max-path-len* (lambda (i) _byte))))

  (define-cstruct _FAMConnection ((fd _int)))
  (define-cstruct _FAMRequest ((reqnum _int)))
  (define-cstruct _FAMEvent ((fc _FAMConnection-pointer)
                             (fr _FAMRequest)
                             (hostname _string)
                             (filename _Buffer)
                             (userData _string) ;; trick
                             (code _FAMCodes)))

  (define-struct fam-connection (conn files event))

  (define (fam-open)
    (define %open-fam
      (get-ffi-obj "FAMOpen" libfam
                   (_fun (conn : (_ptr o _FAMConnection)) ->  (d : _int)
                         -> (values (= 0 d) conn))))
    (let-values (((result conn) (%open-fam)))
      (and result (make-fam-connection (ptr-ref conn _FAMConnection 0)
                                       '()
                                       (malloc (ctype-sizeof _FAMEvent))))))

  (define (fam-close fc)
    (define %close-fam
      (get-ffi-obj "FAMClose" libfam (_fun _FAMConnection-pointer -> _int)))
    (= 0 (%close-fam (fam-connection-conn fc))))

  (define %monitor-directory
    (get-ffi-obj "FAMMonitorDirectory" libfam
                 (_fun _FAMConnection-pointer
                       _file
                       _FAMRequest-pointer
                       _string -> _int)))

  (define %monitor-file
    (get-ffi-obj "FAMMonitorFile" libfam
                 (_fun _FAMConnection-pointer
                       _file
                       _FAMRequest-pointer
                       _string -> _int)))

  (define (fam-monitor-path fc pathname)
    (let* ((pathname (path->string (path->complete-path pathname)))
           (is-file? (file-exists? pathname))
           (is-dir? (and (not is-file?) (directory-exists? pathname))))
      (and (or is-file? is-dir?)
           (let ((conn (fam-connection-conn fc))
                 (req (make-FAMRequest 0))
                 (ffun (if is-dir? %monitor-directory %monitor-file)))
             (and (= 0 (ffun conn pathname req pathname))
                  (begin
                    (set-fam-connection-files! fc
                                              (cons (cons pathname req)
                                                    (fam-connection-files fc)))
                    #t))))))

  (define (%path->req fc path)
    (cond ((assoc path (fam-connection-files fc)) => cdr)
          (else #f)))

  (define (%reqnum->%path fc reqnum)
    (let loop ((paths (fam-connection-files fc)))
      (cond ((null? paths) "")
            ((= (FAMRequest-reqnum (cdar paths)) reqnum) (caar paths))
            (else (loop (cdr paths))))))

  (define-syntax %c+r-ffun
    (syntax-rules ()
      ((%a2fun ffi-name exp-name)
       (define (exp-name fc file)
         (define ffun
           (%try-ffi
            get-ffi-obj ffi-name libfam
            (_fun _FAMConnection-pointer _FAMRequest-pointer -> _int)))
         (let ((conn (fam-connection-conn fc))
               (req (%path->req fc file)))
           (and ffun req (= 0 (ffun conn req))))))))


  (%c+r-ffun "FAMSuspendMonitor" fam-suspend)
  (%c+r-ffun "FAMResumeMonitor" fam-resume)
  (%c+r-ffun "FAMCancelMonitor" fam-cancel)

  (define (fam-any-event? fc)
    (define %pending
      (get-ffi-obj "FAMPending" libfam
                   (_fun _FAMConnection-pointer -> _int)))
    (> (%pending (fam-connection-conn fc)) 0))

  (define %next-event
    (get-ffi-obj "FAMNextEvent" libfam
                 (_fun _FAMConnection-pointer (ev : _pointer)
                       -> (d : _int) -> (values (= d 1)
                                                (ptr-ref ev _FAMEvent 0)))))

  (define (%bs->path bs)
    (let ((match (regexp-match #rx#"(?>([^\0]+)\0)" bs)))
      (if match (path->string (bytes->path (cadr match))) "")))

  (define fam-next-event
    (case-lambda
      ((fc) (fam-next-event fc #f))
      ((fc wait)
       (and (or wait (fam-any-event? fc))
            (let-values (((result event) (%next-event (fam-connection-conn fc)
                                                      (fam-connection-event fc))))
              (and result
                   (list (%reqnum->%path fc (FAMRequest-reqnum (FAMEvent-fr event)))
                         (%bs->path
                          (make-sized-byte-string (FAMEvent-filename event)
                                                  *max-path-len*))
                         (FAMEvent-code event))))))))

  (define (fam-pending-events fc)
    (let loop ((next (fam-next-event fc)) (events '()))
      (if (not next)
          (reverse events)
          (loop (fam-next-event fc) (cons next events)))))

  (define fam-event-monitored-path car)
  (define fam-event-target-path cadr)
  (define fam-event-type caddr)

)
