(in-package async-process)

(cffi:define-foreign-library libc (:default "libc"))
(cffi:use-foreign-library libc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TEMP, for debugging.  This will be handled by ASDF at load time.
(load (cffi-grovel:process-grovel-file 
       "/home/ethan/Documents/async-process/src/libc-symbols-grovel.lisp"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Libc Function Definitions
;;;
;;; these are the libc functions that are used in this program to 
;;; open/manage the PTY's for each process.  The CFFI Groveler is also used
;;; to pull in constants/struct definitions used as parameters.

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PTY specific functions
;;;
;;; The following functions are used to open/close a pty.  The posix-pty
;;; struct is used to hold and keep track of the state of the PTY's

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
  "This must be called on the PTY if the program doesn't terminate
TODO: figure out the exact semantics of when this needs to be called."
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

    (if (cond
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
          ((and (init-pty fdm fds nonblock)
                nil)) ; the and ensures that nil is returned
          ((and (setf pty (make-posix-pty :fdm fdm :fds fds :name name))
                nil))) ; the and ensures that nil is returned
        (progn
          ;; if any of the conditions were true, then there was an error somewhere.
          ;; cleanup and return NIL
          (format t "WARNING: couldn't open PTY")
          (close-pty pty)
          nil)
        
        ;; no errors occured, return the PTY
        pty)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Process specific functions
;;;
;;; Unix PTYs are bi-diretional communication streams.  Typically, a
;;; terminal will combine STDOUT/STDERR and display them in the same output.
;;; We want to keep the outputs separate at this level so that the process
;;; can send output on separate STDOUT and STDERR streams.
;;;
;;; Note that this introduces a potential bug: if the process tries to set
;;; terminal attributes (like with stty), these updates won't be propogated
;;; across both STDOUT and STDERR terminals.

(defstruct (posix-process (:constructor nil))
  (stdio-pty)
  (stder-pty)
  (command)
  (path)
  (nonblockp))

(defun make-posix-process (command path nonblockp)
  "creates a posix-process struct.  Doesn't open any PTYs or start any
programs.  This is purely to initialize the `posix-process` structure."
  (make-instance 'posix-process
                 :stdio-pty nil
                 :stder-pty nil
                 :command command
                 :path path
                 :nonblockp nonblockp))

(defun start-process (proc)
  (let (()))
  (cond
    ((not (setf (posix-process-stdio-pty proc) (open-pty ))))))

(defun close-process (posix-process))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testing Functions
;;;
;;; used to validate functionality and correctness of this library.

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
