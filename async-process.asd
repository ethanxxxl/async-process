(defsystem "async-process"
  :description "asynchronous process execution for common lisp"
  :author "cxxxr <g23tlm@gmail.com>, Ethan Smith <ethansmith.dev@gmail.com"
  :version "0.0.2"
  :license "MIT"
  :depends-on ("cffi" "rove")
  :serial t
  :components ((:file "async-process_windows"
                :if-feature (:or :win32 :windows))
               (:file "async-process"
                :if-feature (:not (:or :win32 :windows)))))
