# async-process

A Common Lisp library for creating and managing asynchronous processes

## Platform Support

- **Linux**: Full support via UIOP
- **BSD**: Full support via UIOP
- **macOS**: Full support via UIOP
- **Windows**: Full support via CFFI implementation

The ASDF system will automatically load the Windows-specific implementation (`src/async-process_windows.lisp`) when on Windows platforms.

The the UIOP implementation may also work on windows.  Once it is tested, it may be
switched over.

## Usage

```lisp
(ql:quickload "async-process")
(in-package async-process) ; => #<PACKAGE "ASYNC-PROCESS>
(defparameter p (create-process '("tee"))) ; => P
(process-send-input p "hello world") ; => NIL
(process-receive-output p) ; => "hello world"
(delete-process p) ; => T
```

## LICENSE
MIT
