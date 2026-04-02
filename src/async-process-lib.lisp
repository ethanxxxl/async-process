(in-package async-process)
(defpackage async-process/libc)

(cffi:define-foreign-library libc (:default "libc"))
(cffi:use-foreign-library libc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEMP, for debugging.  This will be handled by ASDF at load time.
(load (cffi-grovel:process-grovel-file 
       "/home/ethan/Documents/async-process/src/libc-symbols-grovel.lisp"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cffi:defcfun ("setsid" %setsid) :void)
(cffi:defcfun ("execvp" %execvp) :void)
(cffi:defcfun ("dup2" %dup2) :void)
(cffi:defcfun ("fork" %fork) :void)

(cffi:defcfun ("open" %open) :int
  (pathname :string)
  (flags :int)
  &rest)

(cffi:defcfun ("close" %close) :int
  (fd :int))

(cffi:defcfun ("posix_openpt" %posix_openpt) :int
  (flags :int))

(cffi:defcfun ("grantpt" %grantpt) :int
  (master_fd :int))

(cffi:defcfun ("unlockpt" %unlockpt) :int
  (master_fd :int))

(cffi:defcfun ("ptsname" %ptsname) :string
  "The C function returns a char *, which must be copied.  Here,
CFFI automaticaly does this conversion/copy for us."
  (master_fd :int))

(cffi:defcfun ("fcntl" %fcntl) :int
  (filedes :int)
  (cmd :int)
  &rest)

;; termios struct retrieved by groveler
(cffi:defcfun ("tcgetattr" %tcgetattr) :int
  (fd :int)
  (termios-ptr (:pointer (:struct termios))))

;; termios struct retreived by groveler
(cffi:defcfun ("tcsetattr" %tcsetattr) :int
  (fd :int)
  (optional_actions :int)
  (termios-ptr (:pointer (:struct termios))))

;; termios struct retreived by groveler
(cffi:defcfun ("cfmakeraw" %cfmakeraw) :void
  (termios-ptr (:pointer (:struct termios))))

(defstruct posix-pty
  (fdm)
  (fds)
  (name))

(defun init-pty (fdm fds &optional nonblock)
  "helper function to set file attributes/settings on master/slave file 
descriptors after they are opened."
  ;; ensure both slave and master close after program finishes
  (%fcntl fdm +f-setfd+ :int +fd-cloexec+)
  (%fcntl fds +f-setfd+ :int +fd-cloexec+)

  ;; set master as non-blocking
  (when nonblock
    (%fcntl fdm +f-setfl+ :int +o_nonblock+))

  ;; set raw mode
  (cffi:with-foreign-object (tty '(:struct termios))
    (%tcgetattr fds tty)
    (%cfmakeraw tty)
    (%tcsetattr fds +tcsanow+ tty))
  
  ;; always return nil
  nil)

(defun close-pty (pty)
  (declare (type posix-pty pty))
  (let ((fdm (posix-pty-fdm pty))
        (fds (posix-pty-fds pty)))
    (when (/= -1 fdm) (%close fdm))
    (setf (posix-pty-fdm pty) -1)

    (when (/= -1 fds) (%close fds))
    (setf (posix-pty-fds pty) -1)
    
    (setf (posix-pty-name pty) ""))
  nil)


(defun open-pty (&optional nonblock)
  "opens a PTY and returns a `POSIX-PTY` struct."
  (declare (optimize (debug 3)))

  ;; get the master FD through `posix_openpt`.
  (let* ((open-flags (logior +o-rdwr+ +o-noctty+))
         (fdm (%posix_openpt open-flags))
         (fds -1)
         (name nil)
         (pty nil))

    (when
        (prog1
            (cond
              ;; Initialized the attached slave PTS.  grantpt and unlockpt
              ;; are required before opening the slave device.
              ((or (eq -1 fdm)
                   (eq -1 (%grantpt fdm))
                   (eq -1 (%unlockpt fdm))))
              
              ;; get pathname of pty we just opened.
              ((eq nil (setf name (%ptsname fdm)))
               (equal name nil))

              ;; use pathname to open slave file descriptor
              ((eq -1 (setf fds (%open name open-flags))))
          
              ;; both fds and fdm are opened, finish configuration
              ((init-pty fdm fds nonblock)))
          (setf pty (make-posix-pty :fdm fdm :fds fds :name name)))
      
      ;; if any of the conditions were true, then there was an error somewhere.
      (format t "WARNING: couldn't open PTY")
      (close-pty pty))
    
    pty))

(defun pty-test (test-str)
  "should print "
  (let ((test (open-pty t)))
    (cffi:with-foreign-string ((buf len) test-str)
      (when (= -1 (cffi:foreign-funcall "write"
                                        :int (posix-pty-fdm test)
                                        :pointer buf
                                        :size (1- len)
                                        :ssize))
        (cffi:foreign-funcall "perror" :string "write error" :void)))

    (cffi:with-foreign-object (buf :uint8 1024)
      (let ((n (cffi:foreign-funcall "read" :int (posix-pty-fds test) :pointer buf :size 1024 :ssize)))
        (when (> n 0)
          (format t "got ~A: ~A" n (cffi:foreign-string-to-lisp buf :count n)))))
    
    (close-pty test)))

(pty-test "hello world")
