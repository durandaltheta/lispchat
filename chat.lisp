(ql:quickload "usocket")

;; declare our chat parameters with default values
;; we'll resolve hostnames later so address can be ip4 or a hostname
(defparameter *server-address* "192.168.1.0") 
(defparameter *port* "22")
(defparameter *username* "u0")

;; switches default to false
(defparameter *server* nil)

(defparameter *params* '(*server-address*
                          *port*
                          *username*
                          *server*))

(defun debug-print (func str)
  (format t "~s:~s~%" func str)
  (finish-output))

;; evaluate command line arguments (argv)
(defun eval-arg (cur-arg prev-arg test test2 internal-param)
  (when (or (string= prev-arg test) (string= prev-arg test2))
    (setf (symbol-value internal-param) cur-arg)))

;; evaluate command line switches (argv)
(defun eval-switch (cur-arg test test2 internal-param)
  (when (or (string= cur-arg test) (string= cur-arg test2)) 
    (setf (symbol-value internal-param) t)))

;; set chat parameters based on command line input
(defun eval-args (args)
  (let ((prev-arg ""))
    (loop 
      for cur-arg in args
      do (progn
           ;(format t "cur-arg: ~s~%" cur-arg)
           (eval-arg cur-arg prev-arg "--server-address" "-a" '*server-address*)
           (eval-arg cur-arg prev-arg "--port" "-p" '*port*)
           (eval-arg cur-arg prev-arg "--username" "-u" '*username*)
           (eval-switch cur-arg "--server" "-s" '*server*)
           (format t "prev-arg: ~a, cur-arg: ~a~%" prev-arg cur-arg)
           (setf prev-arg cur-arg)))))

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
    (setf buffer (map '(vector (unsigned-byte 8)) #'char-code input))
    (usocket:socket-send socket buffer input-length)))

;; loop to gather user input to send
(defun input-data (socket username)
  (loop 
    (let ((input (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
          (prepend (concatenate 'string username ": ")))
      (setf input (read))
      (debug-print "input-data user input:" input)
      (if (string= input "(exit-chat)")
          (return))
      ;; attach username to message 
      ;(format t "~s~s~%" prepend input)
      (setf input (concatenate 'string prepend input "~%")) 
      (send-data socket input))))

;; handle received buffer and print to 
(defun handle-received-data (buffer)
  (let ((output ""))
    (setf output (map 'string #'code-char buffer))
    (print output)))

;; thread function to handle all incoming data
(defun receive-thread (socket)
  (debug-print "receive-thread" "1")
  (block receiver-nested-loop
         (debug-print "receive-thread" "2")
         (loop 
           (let ((ready-socket (usocket:wait-for-input socket)))
             (debug-print "receive-thread" "3")
             (let ((return-buffer (usocket:socket-receive ready-socket nil nil))) 
               (debug-print "receive-thread" "4")
               (if (not (eq return-buffer :eof))
                   (handle-received-data return-buffer)
                   (return-from receiver-nested-loop)))))))

;; handle the chat communication
(defun handle-connection (socket username)
  (unwind-protect
    (block run-server-block
           (debug-print "handle-connection" "1")
           (sb-thread:make-thread (lambda () 
                                    (progn
                                      (receive-thread socket))))
           (input-data socket username))
    (usocket:socket-close socket)))

;; create a server and set it to listen
(defun create-server (host port username)
  (debug-print "create-server" "1")
  (let ((socket (usocket:socket-listen host port :element-type '(unsigned-byte 8))))
    (loop 
      (handle-connection (usocket:socket-accept socket) username))))

;; create a client and attempt to connect to a server
(defun create-client (host port username)
  (debug-print "create-client" "1")
  (let ((socket (usocket:socket-connect host port :element-type '(unsigned-byte 8))))
    (handle-connection socket username)))

;; our main function
(defun main () 
  (handler-case 
    (progn
      ;; print out the passed argv arguments for sanity
      ;(format t "~s~%" *posix-argv*)

      (eval-args *posix-argv*)

      (print-params *params*)
      (format t "~d~%" (parse-integer *port*))

      (if *server* 
          (progn
            (debug-print "main" "entering create-server")
            (create-server *server-address* (parse-integer *port*) *username*)) 
          (progn
            (debug-print "main" "entering create-client")
            (create-client *server-address* (parse-integer *port*) *username*))) 
      (format t "past main functions")
      (sb-ext:exit))
    (sb-sys:interactive-interrupt (e)
                                  (format t "Exiting chat: ~a~%" e)
                                  (sb-ext:exit))))

