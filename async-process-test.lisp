(ql:quickload 'rove)
(defpackage async-process/test
  (:use :cl
        :rove
        :async-process))

(in-package async-process/test)

(deftest all-tests
  (testing "Process setup/cleanup"
    (ok (let ((procs (copy-list async-process::*active-processes*))
              (p))
          (setf p (create-process '("true") :nonblock nil))              
          (delete-process p)
          (equal procs async-process::*active-processes*)))
    
    (ok (let ((procs (copy-list async-process::*active-processes*)))
          (with-process () ('("true")))
          (equal procs async-process::*active-processes*))))
                                                       
  (testing "Process input/output"
    (ok (outputs (let ((proc (create-process '("echo" "hello" "world") :nonblock nil)))
                   (princ (process-receive-output proc))
                   (delete-process proc))
                 (format nil "hello world~%")))
    
    (ok (outputs (with-process (:name p) ('("echo" "hello" "world"))
                    (format t "~A" (process-receive-output p)))
                 (format nil "hello world~%")))
    
    (ok (outputs (with-process (:output s) ('("echo" "hello" "world") :nonblock nil)
                   (princ (read-line s)))
                 (format nil "hello world")))

    (ok (outputs (with-process (:output s) ('("echo" "hello" "world"))
                   (format t "~A" (read-line s)))
                 (format nil "hello world")))

    (ok (outputs (with-process (:name p) ('("echo" "hello" "world") :nonblock t)
                   (format t "~A" (process-receive-output p)))
                 (format nil "")))

    (ok (outputs (with-process (:name p) ('("tee") :nonblock nil)
                   (process-send-input p "hello world")
                   (format t "~A" (process-receive-output p)))
                 
                 (format nil "hello world")))

    (ok (outputs (with-process (:input in :output out) ('("tee") :nonblock nil)
                   (format in "hello world~%")
                   (finish-output in)
                   (format t "~A" (read-line out)))
                 
                 (format nil "hello world")))))
