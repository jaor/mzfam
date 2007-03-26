(module fam "fam-base.ss"

  (provide fam-available?
           make-fam)

  (require (lib "etc.ss")
           (lib "file.ss"))
  (require (lib "foreign.ss")) (unsafe!)

  (define (%try proc . args)
    (with-handlers ((exn:fail? (lambda (x) #f)))
      (apply proc args)))

  (define %get-ffi-obj
    (lambda args
      (or (apply %try get-ffi-obj args)
          (lambda x -1))))

  (define libfam (or (%try ffi-lib "libfam")
                     (%try ffi-lib "libgamin")))

  (define (fam-available?) (if libfam #t #f))

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
  (define _Buffer
    (make-cstruct-type (build-list *max-path-len* (lambda (i) _byte))))

  (define-cstruct _FAMConnection ((fd _int) (extra _pointer)))
  (define-cstruct _FAMRequest ((reqnum _int)))
  (define-cstruct _FAMEvent ((fc _FAMConnection-pointer)
                             (fr _FAMRequest)
                             (hostname _string)
                             (filename _Buffer)
                             (userData _string)
                             (code _FAMCodes)))

  (defclass <fam-connection> () conn files event
    :autoaccessors #t :autoinitargs #t)

  (define (fam-open)
    (define %open-fam
      (%get-ffi-obj "FAMOpen" libfam
                    (_fun (conn : (_ptr o _FAMConnection)) ->  (d : _int)
                          -> (values (= 0 d) conn))))
    (let-values (((result conn) (%open-fam)))
      (and result (ptr-ref conn _FAMConnection 0))))

  (define (make-fam)
    (let ((conn (and (fam-available?) (fam-open))))
      (and conn
           (make <fam-connection> :conn conn
                                  :files '()
                                  :event (malloc (ctype-sizeof _FAMEvent))))))

  (defmethod (fam-release (fc <fam-connection>))
    (define %close-fam
      (%get-ffi-obj "FAMClose" libfam (_fun _FAMConnection-pointer -> _int)))
    (= 0 (%close-fam (fam-connection-conn fc))))

  (define %monitor-directory
    (%get-ffi-obj "FAMMonitorDirectory" libfam
                  (_fun _FAMConnection-pointer
                        _file
                        _FAMRequest-pointer
                        _string -> _int)))

  (define %monitor-file
    (%get-ffi-obj "FAMMonitorFile" libfam
                  (_fun _FAMConnection-pointer
                        _file
                        _FAMRequest-pointer
                        _string -> _int)))

  (defmethod (fam-monitor-path (fc <fam-connection>) pathname)
    (let ((pathname (path->string (path->complete-path pathname))))
      (if (assoc pathname (fam-connection-files fc))
          #t
          (let* ((is-file? (file-exists? pathname))
                 (is-dir? (and (not is-file?) (directory-exists? pathname)))
                 (is-file? (if (or is-file? is-dir?) is-file? (path-only pathname)))
                 (is-dir? (not is-file?)))
            (and (or is-file? is-dir?)
                 (let ((conn (fam-connection-conn fc))
                       (req (make-FAMRequest 0))
                       (ffun (if is-file? %monitor-file %monitor-directory)))
                   (and (= 0 (ffun conn pathname req pathname))
                        (begin
                          (set-fam-connection-files! fc
                                                     (cons (cons pathname req)
                                                           (fam-connection-files fc)))
                          #t))))))))

  (defmethod (fam-monitored-paths (fc <fam-connection>))
    (map car (fam-connection-files fc)))

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
       (defmethod (exp-name (fc <fam-connection>) file)
         (define ffun
           (%get-ffi-obj ffi-name libfam
                         (_fun _FAMConnection-pointer _FAMRequest-pointer -> _int)))
         (let ((conn (fam-connection-conn fc))
               (req (%path->req fc file)))
           (and ffun req (= 0 (ffun conn req))))))))


  (%c+r-ffun "FAMSuspendMonitor" fam-suspend-path-monitoring)
  (%c+r-ffun "FAMResumeMonitor" fam-resume-path-monitoring)
  (%c+r-ffun "FAMCancelMonitor" fam-cancel-path-monitoring)

  (define %pending
    (%get-ffi-obj "FAMPending" libfam (_fun _FAMConnection-pointer -> _int)))

  (defmethod (fam-any-event? (fc <fam-connection>))
    (let ((cn (fam-connection-conn fc)))
      (> (%pending cn) 0)))

  (define %next-event
    (%get-ffi-obj "FAMNextEvent" libfam
                  (_fun _FAMConnection-pointer (ev : _pointer)
                        -> (d : _int) -> (values (= d 1)
                                                 (ptr-ref ev _FAMEvent 0)))))

  (define (%bs->path bs)
    (let ((match (regexp-match #rx#"(?>([^\0]+)\0)" bs)))
      (if match (path->string (bytes->path (cadr match))) "")))

  (defmethod (fam-next-event (fc <fam-connection>) &optional (wait #f))
    (and (or wait (fam-any-event? fc))
         (let-values (((result event) (%next-event (fam-connection-conn fc)
                                                   (fam-connection-event fc))))
           (and result
                (make <fam-event>
                 :monitored-path (FAMEvent-userData event)
                 :path (%bs->path (make-sized-byte-string (FAMEvent-filename event)
                                                          *max-path-len*))
                 :type (FAMEvent-code event))))))

  (defmethod (fam-pending-events (fc <fam-connection>))
    (let loop ((next (fam-next-event fc)) (events '()))
      (if (not next)
          (reverse events)
          (loop (fam-next-event fc) (cons next events)))))
)
