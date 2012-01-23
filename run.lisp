;; ===============================================
;; LISP->CL-MONOME ===============================
;; ===============================================
(require :sb-bsd-sockets)
(require :osc)

(use-package :osc)
(use-package :sb-bsd-sockets)

;; -----------------------------------------------
;; GLOBALS ---------------------------------------
;; -----------------------------------------------
(defvar *monome* nil)

(format t "connecting to ~a~%" *connect-port*)

;; -----------------------------------------------
;; UTILITY->OSC ----------------------------------
;; -----------------------------------------------
(defun make-osc-socket()
  (let ((socket (make-instance 'inet-socket :type :datagram :protocol :udp)))
    (setf (sockopt-reuse-address socket) t)
    socket))

(defmacro osc-write-to-stream (stream &body args)
  `(progn (write-sequence (osc:encode-message ,@args) ,stream)
          (finish-output ,stream)))

(defun make-osc-tree ()
  (make-hash-table :test 'equalp))

(defun dp-register (tree address function)
  "registers a function to respond to incoming osc message. since
   only one function should be associated with an address, any
   previous registration will be overwritten"
  (setf (gethash address tree)
        function))

(defun dp-remove (tree address)
  "removes the function associated with the given address.."
  (remhash address tree))

(defun dp-match (tree pattern)
"returns a list of functions which are registered for
 dispatch for a given address pattern.."
  (list (gethash pattern tree)))

(defun dispatch (tree osc-message)
  "calls the function(s) matching the address(pattern) in the osc 
   message with the data contained in the message"
  (let ((pattern (car osc-message)))
    (dolist (x (dp-match tree pattern))
      (unless (eq x NIL)
        (apply #'x (cdr osc-message))))))

;; -----------------------------------------------
;; CLASS->MONOME ---------------------------------
;; -----------------------------------------------
(defclass monome ()
  ((host
     :accessor monome-host
     :initarg :host)
   (host-name
     :accessor monome-host-name
     :initarg :host-name)
   (host-address
     :accessor monome-host-address
     :initarg :host-address)
   (host-port
     :accessor monome-host-port
     :initarg :host-port)
   (socket-client
     :accessor monome-socket-client
     :initform (make-osc-socket))
   (socket-server
     :accessor monome-socket-server
     :initform (make-osc-socket))
   (stream-client
     :accessor monome-stream-client)
   (stream-server
     :accessor monome-stream-server)))

;; Methods - - - - - - - - - - - - - - - - - - - -
(defmethod initialize-instance :after ((monome monome) &key)
  (setf (monome-host monome)
        (get-host-by-name (monome-host-name monome)))
  (setf (monome-host-address monome)
        (host-ent-address (monome-host monome))))

(defmethod monome-connect ((monome monome))
  (socket-connect (monome-socket-client monome)
                  (monome-host-address  monome)
                  (monome-host-port     monome))
  (setf (monome-stream-client monome)
        (socket-make-stream (monome-socket-client monome)
          :input t
          :output t
          :element-type '(unsigned-byte 8) :buffering :full))
  monome)

(defmacro monome-write-to-stream (monome function-path &rest function-arguments)
 `(osc-write-to-stream (monome-stream-client ,monome)
                       ,(concatenate 'string "/python" function-path) ;; FIX: This shouldn't be /python
                       ,@function-arguments))

(defmethod monome-led-set ((monome monome) x y state)
  (monome-write-to-stream monome "/grid/led/set" x y state))

(defmethod monome-led-set-all ((monome monome) state)
  (monome-write-to-stream monome "/grid/led/all" state))

(defmethod monome-listen ((monome monome) port)
  (let ((buffer (make-sequence '(vector (unsigned-byte 8)) 1024)))
    (socket-bind (monome-socket-server monome) (monome-host-address monome) port)
    (format t "listening on localhost port ~A~%" port)
    (unwind-protect 
        (loop do
              (socket-receive (monome-socket-server monome) buffer nil)
              (format t "received => ~S~%" (osc:decode-bundle buffer)))
      (when (monome-socket-server monome) (socket-close (monome-socket-server monome))))))

;; Functions - - - - - - - - - - - - - - - - - - -
(defun make-monome (&rest make-instance-options)
  (apply #'make-instance (append (list 'monome)
                                 make-instance-options)))

;; -----------------------------------------------
;; TEMPORARY -------------------------------------
;; -----------------------------------------------
(defun random-test ()
  (loop :while t :do
    (progn
      (let ((random-x (map-into (make-list 15) (lambda ()
                                                (random 8))))
            (random-y (map-into (make-list 15) (lambda ()
                                                (random 8)))))
        (monome-led-set-all *monome* 0)
        (dotimes (index 15)
          (monome-led-set *monome* (elt random-x index) (elt random-y index) 1))
        (sleep 0.5)))))

;; -----------------------------------------------
;; MAIN ------------------------------------------
;; -----------------------------------------------
(setf *monome* (make-monome :host-name "Eriks-MacBook-Air.local."
                            :host-port 15932))

;;(monome-connect *monome*)

(monome-listen *monome* *connect-port*)

;;(dp-register (make-osc-tree) #(0 0 0 0) (lambda () (format t "got something~%")))

