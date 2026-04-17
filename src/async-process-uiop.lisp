(defpackage :async-process
  (:use :cl)
  (:export
   :delete-process
   :process-send-input
   :process-receive-output
   :process-alive-p
   :create-process))

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
implemented using `uiop:launch-program` which returns a `process-info` class"))

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
  (declare (type process proc))

  (uiop:process-info-pid (slot-value proc 'info)))

(defun process-send-input (proc input)
  (declare (type process proc)
           (type string input))

  (let ((s (uiop:process-info-input (slot-value proc 'info))))
    (write-string input s)
    (finish-output s)))

(defun process-receive-output (proc)
  (declare (type process proc)
           (optimize (debug 3)))

  (with-slots (info nonblockp) proc
    (let ((s (uiop:process-info-output info)))
      (unless (and nonblockp (not (listen s)))
        ;; read output, unless we are nonblocking and there is no data available

        (loop :with v = (make-array 20 
                                    :element-type 'character
                                    :adjustable t
                                    :fill-pointer 0)
              :for c = (read-char-no-hang s nil nil)
              :while c
              :do (vector-push-extend c v)
              :finally (return v))))))
   

(defun process-alive-p (proc)
  (declare (type process proc))
 
  (uiop:process-alive-p (slot-value proc 'info)))

(defun test-process-output ()
  (let ((proc (create-process '("echo" "hello" "world") :nonblock nil)))
    (sleep 0.5)
    (format t "~&ouptut: ~S" (process-receive-output proc))
    (delete-process proc)))

(defun test-process-input ()
  (let ((proc (create-process '("tee") :nonblock nil)))
    (sleep 0.5)
    (process-send-input proc "hello world
")
    (format t "~&output: ~S" (process-receive-output proc))
    (delete-process proc)))

;(test-process-input)

(defvar *test-proc* nil)

(defun test1 ()
  (setf *test-proc* (create-process '("tee") :nonblock t)))

(defun test2 ()
  (process-send-input *test-proc* "hello world
"))
(defun test-process-input ()
  (let ((proc (create-process '("tee") :nonblock nil)))
    (sleep 0.5)
    (process-send-input proc "hello world
")
    (format t "~&output: ~S" (process-receive-output proc))
    (delete-process proc)))
(defun test3 ()
  (format t "~&~A" (process-receive-output *test-proc*)))

(defun test1-2-cleanup ()
  (delete-process *test-proc*)
  (setf *test-proc* nil))

;(test1)
;(test2)
;(test3)
;(test1-2-cleanup)