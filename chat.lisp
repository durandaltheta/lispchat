(load "~/quicklisp/setup.lisp")
(ql:quickload "usocket")

(load "defaults.lisp")

;; declare our chat parameters with default values
;; we'll resolve hostnames later so address can be ip4 or a hostname
(defparameter *server-address* "192.168.1.0") 
(defparameter *port* 22)
(defparameter *username* "u0")

;; switches default to false
(defparameter *server* nil)

(defparameter *params* '(*server-address*
                          *port*
                          *username*
                          *server*))

;; evaluate command line arguments (argv)
(defun eval-arg (cur-arg prev-arg test test2 internal-param)
  (when (or (string= prev-arg test) (string= prev-arg test2))
    (setf (symbol-value internal-param) cur-arg)))

;; evaluate command line switches (argv)
(defun eval-switch (cur-arg test test2 internal-param)
  (when (or (string= cur-arg test) (string= cur-arg test2)) 
    (setf (symbol-value internal-param) t)))

;; iterate through a list of symbols printing both the name and value
(defun print-params (params)
  (loop
    for i in params do 
    (format t "~a: ~a~%" i (symbol-value i))))

;; send data to the server, automatically handle string conversion to a buffer
(defun send-data (socket input)
  (let ((buffer (make-array (list-length input) 
                            :element-type '(unsigned-byte 8)
                            :initial-element 0)) 
        (input-length (list-length input)))
    (loop 
      for i upto input-length collect i do
      (setf (nth i buffer) (parse-integer (nth i input))))
    (usocket::socket-send socket buffer input-length)))

;; loop to gather user input to send
(defun input-data (socket username)
  (loop 
    (let ((input "") (prepend (concatenate username ": ")) (input-length 0))
      (setf input (read))
      (if (string= input "(exit-chat)")
        (return))
      (setf input (concatenate prepend input (list #\newline))) ; attach username to message
      (send-data socket input))))

;; handle received buffer and print to 
(defun handle-received-data (buffer)
  (let ((output ""))
    (loop 
      for x in buffer do
      (concatenate 'string output (write-to-string x)))
    (print output)))

;; thread function to handle all incoming data
(defun receive-thread (socket)
  (block receiver-nested-loop
         (loop 
           (let ((socket-list '()))
             (setf socket-list (usocket::wait-for-input socket))
             (loop
               for s in socket-list do
               (multiple-value-bind 
                 (return-buffer return-length remote-host remote-port) 
                 (usocket::socket-receive socket nil nil)
                 (if (not (eq return-buffer :eof))
                   (handle-received-data return-buffer)
                   (return-from receiver-nested-loop))))))))

;; create a server and set it to listen
(defun create-server (host port username)
  (let ((socket (usocket::socket-listen host port :element-type '(unsigned-byte 8))) 
        (input ""))
    (unwind-protect
      (block run-server-block
             (sb-thread:make-thread (lambda () (receive-thread socket)))
             (input-data socket username))
      (usocket::socket-close socket))))

;; create a client and attempt to connect to a server
(defun create-client (host port username)
  (let ((socket (usocket::socket-connect host port :element-type '(unsigned-byte 8)))
        (input ""))
    (unwind-protect 
      (block run-client-block
             (sb-thread:make-thread (lambda () (receive-thread socket)))
             (input-data socket username))
      (socket-close socket))))

;; our main function
(defun main () 
  (handler-case 
    (progn
      ;; print out the passed argv arguments for sanity
      ;(format t "~s~%" *posix-argv*)

      ;; set chat parameters based on command line input
      (let ((prev-arg ""))
        (loop 
          for cur-arg in *posix-argv* 
          do (progn
               ;(format t "cur-arg: ~s~%" cur-arg)
               (eval-arg cur-arg prev-arg "--address" "-a" '*address*)
               (eval-arg cur-arg prev-arg "--port" "-p" '*port*)
               (eval-arg cur-arg prev-arg "--local-server-ip" "-l" '*local-server-ip*)
               (eval-arg cur-arg prev-arg "--username" "-u" '*username*)
               (eval-switch cur-arg "--server" "-s" '*server*)
               (setf prev-arg cur-arg))))

      (print-params *params*)
      (format t "~d~%" (parse-integer *port*))

      (if *server* 
        (create-server *server-address* (parse-integer *port*) *username*)
        (create-client *server-address* (parse-integer *port*) *username*)))
    (sb-sys:interactive-interrupt (e)
                                    (format t "Exiting chat~%")
                                    (sb-ext:quit))))

(main)
