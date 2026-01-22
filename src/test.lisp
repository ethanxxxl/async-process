asdf:*central-registry*
(ql:quickload "alexandria")
(ql:quickload "babel")

(setf asdf:*central-registry* (list #P"/home/ethan/Documents/async-process/src/"))
(asdf:load-asd #P"/home/ethan/Documents/async-process/src/async-process.asd")

(asdf:load-system "async-process")

(defvar *proc* nil)
(setf *proc* (async-process:create-process '("tee" "/home/ethan/test.log")
                                           :nonblock t))

(format t "~&~a"
        (with-output-to-string (s)
          (async-process:process-send-input *proc* (format nil "ima bot~%"))
          (sleep 0.1)
          (format s "~A" (async-process:process-receive-output *proc*))))

(async-process:process-send-input *proc* (format nil "bop~%"))
(format t (async-process:process-receive-output *proc* :both))

(format t "~&~S" (async-process:process-receive-output *proc* :both))

(defun cffi-null-string-test ()
  (format t "~&~S"
          (cffi:with-pointer-to-vector-data 
              (p (make-array 10
                             :element-type '(unsigned-byte 8)
                             :initial-element 0))
            (cffi:foreign-string-to-lisp p))))