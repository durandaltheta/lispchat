#!/usr/bin/sbcl --script

(load quicklisp.lisp)
(ql:quickload "usocket")

;; print out the passed argv arguments for sanity
(format t "~s~%" *posix-argv*)

;; declare our chat parameters with default values
;; we'll resolve hostnames later so address can be ip4 or a hostname
(defparameter *address* "192.168.1.0") 
(defparameter *port* 22)

;; switches default to false
(defparameter *server* nil)

;; evaluate command line arguments (argv)
(defun eval-arg (cur-arg prev-arg test test2 internal-param)
  (when (or (string= prev-arg test) (string= prev-arg test2))
    (setf (symbol-value internal-param) cur-arg)))

(defun eval-switch (cur-arg test test2 internal-param)
  (when (or (string= prev-arg test) (string= prev-arg test2)) 
    (setf (symbol-value internal-param) t)))

;; set chat parameters based on command line input
(let ((prev-arg ""))
  (loop for cur-arg in *posix-argv* 
        do ((eval-arg cur-arg prev-arg "--address" "-a" '*address*)
            (eval-arg cur-arg prev-arg "--ip" "-i" '*address*)
            (eval-arg cur-arg prev-arg "--port" "-p" '*port*)
            (eval-switch cur-arg "--server" "-s" '*server*)
            (setf prev-arg cur-arg))))

(defparameter *buffer-length*)

(defun send-data (socket input)
  (let ((buffer (simple-array (unsigned-byte 8) (list-length input))) (input-length (list-length input)) )
    (loop 
      for i upto input-length collect i
      (setf (nth i buffer) (parse-integer (nth i input))))
    (socket-send socket buffer input-length)))

(defun create-server (host port)
  (let ((socket (socket-listen host port :element-type '(unsigned-byte 8))) 
        (buffer (simple-array (unsigned-byte 8) *buffer-length*)))
    (unwind-protect
      (loop 
        (let ((socket-list '()))
          (setf socket-list (wait-for-input socket))
          (loop
            for s in socket-list do
            (multiple-value-bind (return-buffer return-length remote-host remote-port) (socket-receive socket buffer nil)
              (print return-buffer)))))
      (socket-close socket))))

(defun create-client (host port)
  (let ((socket (socket-connect host port :element-type '(unsigned-byte 8)))
        (input ""))
    (unwind-protect 
      (block nested-loop
             (loop 
               (let ((input "") (input-length 0))
                 (setf input (read))
                 (if (string= input "(exit-chat)")
                   (return-from nested-loop))
                 (send-data socket input))))
      (socket-close socket))))

(if *server* 
  (create-server *address* *port*)
  (create-client *address* *port*))

