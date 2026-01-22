(defpackage :async-process
  (:use :cl)
  (:export
   :delete-process
   :process-send-input
   :process-receive-output
   :process-alive-p
   :create-process
   :cffi-test))
(in-package :async-process)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun system (cmd)
    (ignore-errors (string-right-trim '(#\Newline) (uiop:run-program cmd :output :string))))
  (defun muslp ()
    (ignore-errors
      (not (zerop (length (uiop:run-program
                           "ldd /bin/ls |grep musl"
                           :ignore-error-status t
                           :output :string)))))))

(pushnew (asdf:system-relative-pathname
          :async-process
          (format nil "../static/~A/"
                  (cond
                    ;; Windows
                    ((uiop/os:featurep '(:and :windows :x86-64))
                     "x86_64/windows")
                    ((uiop/os:featurep :windows)
                     "x86/windows")
                    ;; macOS (Darwin)
                    ((uiop/os:featurep :os-macosx)
                     (format nil "~A/darwin"
                             (uiop:run-program '("uname" "-m") :output '(:string :stripped t))))
                    ;; Linux / Generic Unix
                    ((uiop/os:featurep :unix)
                     (format nil "~A/~A"
                             (uiop:run-program '("uname" "-m") :output '(:string :stripped t))
                             (let ((os (uiop:run-program '("uname") :output '(:string :stripped t))))
                               (cond ((and (equal os "Linux")
                                           (ignore-errors (funcall (read-from-string "muslp"))))
                                      "Linux-musl")
                                     (t os))))))))
         cffi:*foreign-library-directories*
         :test #'uiop:pathname-equal)

(cffi:define-foreign-library async-process
  (:darwin "libasyncprocess.dylib")
  (:unix "libasyncprocess.so")
  (:windows "libasyncprocess.dll"))

(cffi:use-foreign-library #P"/home/ethan/Documents/async-process/.libs/libasyncprocess.so")

(defclass process ()
  ((process :reader process-process :initarg :process)
   (encode :accessor process-encode :initarg :encode)))

(cffi:defcfun ("create_process" %create-process) :pointer
  (command :pointer)
  (path :string)
  (noblock :bool))

(cffi:defcfun ("delete_process" %delete-process) :void
  (process :pointer))

(cffi:defcfun ("process_pid" %process-pid) :int
  (process :pointer))

(cffi:defcfun ("process_write" %process-write) :ssize
  (process :pointer)
  (string :string)
  (n :size))

(cffi:defcfun ("process_write_string" %process-write-string) :ssize
  (process :pointer)
  (string :string))

(cffi:defcfun ("process_receive_stdout" %process-receive-stdout) :string
  (process :pointer)
  (bytes :pointer))

(cffi:defcfun ("process_receive_stderr" %process-receive-stderr) :string
  (process :pointer)
  (bytes :pointer))

(cffi:defcfun ("process_receive_output" %process-receive-output) :pointer
  (process :pointer)
  (bytes :pointer))

(cffi:defcfun ("process_alive_p" %process-alive-p) :boolean
  (process :pointer))

(cffi:defcfun "cffi_test" :string)

(defun create-process (command &key nonblock (encode cffi:*default-foreign-encoding*) directory)
  (when (and directory (not (uiop:directory-exists-p directory)))
    (error "Directory ~S does not exist" directory))
  (let* ((command (uiop:ensure-list command))
         (length (length command)))
    (cffi:with-foreign-object (argv :string (1+ length))
      (loop :for i :from 0
            :for c :in command
            :do (setf (cffi:mem-aref argv :string i) c))
      (setf (cffi:mem-aref argv :string length) (cffi:null-pointer))
      (let ((p (%create-process argv 
                                (if directory
                                    (namestring directory)
                                    (cffi:null-pointer))
                                nonblock)))
        (if (cffi:null-pointer-p p)
            (error "create-process failed: ~S" command)
            (make-instance 'process :process p :encode encode))))))

(defun delete-process (process)
  (%delete-process (process-process process)))

(defun process-pid (process)
  (%process-pid (process-process process)))

(defun process-send-input (process string)
  (let ((cffi:*default-foreign-encoding* (process-encode process)))
    (%process-write-string (process-process process) string)))

(defun process-receive-output (process &optional (source :both))
  "`source` can be either `:stdout`, `:stderr`, or `:both`.  It specifies the stream
to read from."
  (declare (optimize (debug 3)))
  (flet ((call-cfun (read-func)
           "helper function to call one of the three cffi functions for receiving output."
           (cffi:with-foreign-pointer (bytes 8)
             (let ((cffi:*default-foreign-encoding* (process-encode process))
                   (output (funcall read-func 
                                    (process-process process)
                                    bytes)))
               (cffi:foreign-string-to-lisp 
                output
                :count (cffi:mem-ref bytes :size))))))
    
    (case source
      (:stdout (call-cfun '%process-receive-stdout))
      (:stderr (call-cfun '%process-receive-stderr))
      (:both (call-cfun '%process-receive-output)))))

(defun process-alive-p (process)
  (%process-alive-p (process-process process)))
