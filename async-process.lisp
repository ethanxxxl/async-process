(defpackage :async-process
  (:use :cl)
  (:export
   :delete-process
   :process-send-input
   :process-receive-output
   :process-alive-p
   :create-process
   :process-input-stream
   :process-output-stream
   :with-process))

(in-package async-process)

(defvar *active-processes* nil
  "list of processes started by async-process.  If a process won't exit and needs
killed, it can be found in this list.")

(defclass process ()  
  ((info :type uiop:process-info :initarg :info)
   (nonblockp :type t :initform t :initarg :nonblockp)
   (command :type string :initarg :command))
  (:documentation "Represents an asynchronous process.  `async-process` used to
implement bespoke logic for starting processes.  Now, this functionality is
implemented using `uiop:launch-program` which returns a `process-info` class.

IMPORTANT: You must call `DELETE-PROCESS` on this process before it
goes out of scope.  Otherwise, system resources will not be properly free'd."))

(defun create-process (command &rest keys &key nonblock
                                    (encode cffi:*default-foreign-encoding*) 
                                    &allow-other-keys)
  "calls creates a process that runs in the background.  `DELETE-PROCESS` must
be called when process is completed.  Passes arguments to uiop:launch-program.
`NONBLOCK` will affect behavior of reading output.  Encode is not used."
  (declare (ignore encode))

  (let ((proc (make-instance 'process
                             :command command
                             :nonblockp nonblock
                             :info (apply 'uiop:launch-program
                                          command
                                          :input :stream
                                          :output :stream
                                          :error-output :stream
                                          keys))))
    
    (push proc *active-processes*)
    proc))

(defun delete-process (proc)
  "terminate `PROCESS` and remove it from *active-processes*"
  (declare (type process proc))

  (with-slots (info) proc
    (when (uiop:process-alive-p info)
      (uiop:terminate-process info)
      (uiop:wait-process info)))
    
  (setf *active-processes* (delete proc *active-processes*))
  t)

(defun process-pid (proc)
  "Returns the process ID for the specified process."
  (declare (type process proc))

  (uiop:process-info-pid (slot-value proc 'info)))

(defun process-send-input (proc input)
  "send the string `INPUT` to the process"
  (declare (type process proc)
           (type string input))

  (let ((s (uiop:process-info-input (slot-value proc 'info))))
    (write-string input s)
    (finish-output s)))

(defun process-receive-output (proc &key (errorp nil))
  "returns a string containing the output of `PROC`.  This defaults to STDOUT.
if you need the output of STDERR, then set `:ERRORP` to true."
  (declare (type process proc))
  
  (with-slots (info nonblockp) proc
    (let ((s (if errorp
                 (uiop:process-info-error-output info)
                 (uiop:process-info-output info)))
          
          (blockp (not nonblockp))
          (str-out (make-array 20 
                               :element-type 'character
                               :adjustable t
                               :fill-pointer 0)))
      (when (or blockp (listen s))
        ;; attempt reading output whenever there is data, or we can block
        
        (loop :for c = (read-char-no-hang s nil nil)
              :while (or (and blockp (= 0 (length str-out)))
                         c)
              :do (when c (vector-push-extend c str-out))))
      str-out)))
   

(defun process-alive-p (proc)
  "Returns T if `PROC` is still running."
  (declare (type process proc))
 
  (uiop:process-alive-p (slot-value proc 'info)))


(defun process-input-stream (proc)
  (declare (type process proc))
  (uiop:process-info-input (slot-value proc 'info)))

(defun process-output-stream (proc)
  (declare (type process proc))
  (uiop:process-info-output (slot-value proc 'info)))

(defmacro with-process ((&key name input output)
                        (&rest create-process-args)
                        &body body)
  "Creates a process and runs it in the background while executing `BODY`.  The 
process object and input/output streams will be bound to `NAME`, `INPUT`, and 
`OUTPUT`, respectively if specified.  bear in mind that the input and output
streams are the process input/output streams.  The `CREATE-PROCESS-ARGS` are
passed to the function create-process to create the new process."
  (let* ((proc-name (or name (gensym)))
         (bindings (append
                    `((,proc-name (funcall 'create-process ,@create-process-args)))
                    (and input `((,input (process-input-stream ,proc-name))))
                    (and output `((,output (process-output-stream ,proc-name))))))
         (stream-cleanup (append (and input `((close ,input)))
                                 (and output `((close ,output))))))
    `(let* ,bindings
       (unwind-protect (progn ,@body)
         ,@stream-cleanup
         (delete-process ,proc-name)))))
