#!/usr/bin/sbcl --script

;; print out the passed argv arguments for sanity
(format t "~s~%" *posix-argv*)

;; declare our chat parameters with default values
;; we'll resolve hostnames later so address can be ip4 or a hostname
(defparameter *inp-address* "192.168.1.0") 
(defparameter *inp-port* "22")

;; switches default to false
(defparameter *server* nil)

;; evaluate command line arguments (argv)
(defun eval-arg (cur-arg prev-arg test test2 internal-param)
  (if (or (string= prev-arg test) (string= prev-arg test2))
    ((setf (symbol-value internal-param) cur-arg))))

(defun eval-switch (cur-arg test test2 internal-param)
  (when (or (string= prev-arg test) (string= prev-arg test2)) 
    (setf (symbol-value internal-param) t))
  )

;; set chat parameters based on command line input
(let ((prev-arg ""))
  (loop for cur-arg in *posix-argv* 
        do ((eval-arg cur-arg prev-arg "--address" "-a" '*address*)
            (eval-arg cur-arg prev-arg "--ip" "-i" '*address*)
            (eval-arg cur-arg prev-arg "--port" "-p" '*port*)
            (eval-switch cur-arg "--server" "-s" '*server*)
            (setf prev-arg cur-arg))))

;; declare our internal connection variables
(defparameter *address* 19216810)
(defparameter *port* 22)

;; The following line:
;; 1) resolves the host name (turns a "www.example.com" into an ip4 address)
;; 2) removes the leading variable so we have a valid list of keywords
;; 3) extract just the value of the ip address we want
;; 4) translate the dotted ip value to a raw integer 
;; 5) stores said ip address in the *address* parameter
(setf *address* (dotted-to-ipaddr (getf (cdr (resolve-host-ipaddr *inp-address*)) :addr-list)))

;; simply translate our port from a string to an int
(setf *port* (parse-integer *inp-port*))

;; start connecting
(if *server* 
  (let ((server (open-socket-server *port*))) ; if true
    (loop 

      ;; listen for incoming connections
      (let ((socket (socket-accept-server)))
        
        ;; spawn a process to handle the connection 
        (make-process "Connection handler"
                      #'handle-connection
                      socket))
  ()))) ; else
