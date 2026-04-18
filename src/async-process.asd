(defsystem "async-process"
  :description "asynchronous process execution for common lisp"
  :author "cxxxr <g23tlm@gmail.com>"
  :version "0.0.1"
  :license "MIT"
  :serial t
  :components ((:file "async-process_windows"
                :if-feature (:or :win32 :windows))
               (:file "async-process-uiop"
                :if-feature (:not (:or :win32 :windows)))))
