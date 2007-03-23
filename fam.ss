(module fam mzscheme
  (provide fam-open
           fam-close
           fam-suspend
           fam-resume
           fam-cancel
           fam-monitor-pathname
           fam-any-event?
           fam-next-event)

  (require (lib "foreign.ss")) (unsafe!)

  (define libfam (ffi-lib "libfam"))

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

  (define-cstruct _FAMConnection ((fd _int)))
  (define-cstruct _FAMRequest ((reqnum _int)))
  (define-cstruct _FAMEvent ((fc _FAMConnection-pointer)
                             (fr _FAMRequest)
                             (hostname _cvector)
                             (filename _cvector)
                             (userData _string)
                             (code _FAMCodes)))

  (define-struct fam-connection (conn files))

  (define (fam-open)
    (define %open-fam
      (get-ffi-obj "FAMOpen" libfam
                   (_fun (conn : (_ptr o _FAMConnection)) ->  (d : _int)
                         -> (values (= 0 d) conn))))
    (let-values (((result conn) (%open-fam)))
      (and result (make-fam-connection (ptr-ref conn _FAMConnection 0) '()))))

  (define (fam-close fc)
    (define %close-fam
      (get-ffi-obj "FAMClose" libfam (_fun _FAMConnection-pointer -> _int)))
    (= 0 (%close-fam (fam-connection-conn fc))))

  (define %monitor-directory
    (get-ffi-obj "FAMMonitorDirectory" libfam
                 (_fun _FAMConnection-pointer
                       _string
                       _FAMRequest-pointer
                       _string -> _int)))

  (define %monitor-file
    (get-ffi-obj "FAMMonitorFile" libfam
                 (_fun _FAMConnection-pointer
                       _string
                       _FAMRequest-pointer
                       _string -> _int)))

  (define (fam-monitor-pathname fc pathname)
    (let* ((pathname (path->string (path->complete-path pathname)))
           (is-file? (file-exists? pathname))
           (is-dir? (and (not is-file?) (directory-exists? pathname))))
      (when (or is-file? is-dir?)
        (let ((conn (fam-connection-conn fc))
              (req (make-FAMRequest 0))
              (ffun (if is-dir? %monitor-directory %monitor-file)))
          (when (= 0 (ffun conn pathname req pathname))
            (set-fam-connection-files! fc
                                       (cons (cons pathname req)
                                             (fam-connection-files fc)))
            req)))))

  (define (%->req obj fc)
    (cond ((FAMRequest? obj) obj)
          ((assoc obj (fam-connection-files fc)) => cdr)
          (else #f)))

  (define-syntax %c+r-ffun
    (syntax-rules ()
      ((%a2fun ffi-name exp-name)
       (define (exp-name fc obj)
         (define ffun
           (get-ffi-obj ffi-name libfam
                        (_fun _FAMConnection-pointer _FAMRequest-pointer -> _int)))
         (let ((conn (fam-connection-conn fc))
               (req (%->req obj fc)))
           (when req (ffun conn req)))))))


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
                 (_fun _FAMConnection-pointer (ev : (_ptr o _FAMEvent))
                       -> (d : _int) -> (values (= d 1) ev))))

  (define fam-next-event
    (case-lambda
      ((fc) (fam-next-event fc #f))
      ((fc wait)
       (when (or wait (fam-any-event? fc))
         (let-values (((result eptr) (%next-event (fam-connection-conn fc))))
           (and result
                (let ((event (ptr-ref eptr _FAMEvent 0)))
                  (cons (FAMEvent-userData event) (FAMEvent-code event)))))))))

)
