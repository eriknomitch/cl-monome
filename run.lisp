;; ===============================================
;; LISP->CL-MONOME ===============================
;; ===============================================
(require :sb-bsd-sockets)
(require :osc)

(use-package :osc)
(use-package :sb-bsd-sockets)
(use-package :sb-thread)

;; -----------------------------------------------
;; GLOBALS ---------------------------------------
;; -----------------------------------------------
(defvar *monome* nil)

;; -----------------------------------------------
;; UTILITY ---------------------------------------
;; -----------------------------------------------
(defun prompt-read (prompt)
  (format *query-io* "~a" prompt)
  (force-output *query-io*)
  (read-line *query-io*))

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

;; -----------------------------------------------
;; CLASS->CELL -----------------------------------
;; -----------------------------------------------
(defclass cell ()
  ((index
     :accessor cell-index
     :initarg :index)
   (state
     :accessor cell-state
     :initarg :state
     :initform nil)))

;; -----------------------------------------------
;; CLASS->ROW ------------------------------------
;; -----------------------------------------------
(defclass row ()
  ((index
     :accessor row-index
     :initarg :index
     :initform nil)
   (cells
     :accessor row-cells
     :initarg :cells
     :initform '())))

;; -----------------------------------------------
;; CLASS->MONOME ---------------------------------
;; -----------------------------------------------
;; The CLIENT connects to the Monome unit and sends commands (LED on/off, etc.)
;; the SERVER connects to the port we get from python and listens for keypresses.
(defclass monome ()
  ((cells
     :accessor monome-cells
     :initarg :cells
     :initform '())
   (host
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

(defun sweep ()
  (monome-led-set-all *monome* 1)
  (sleep 0.5)
  (monome-led-set-all *monome* 0))

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
        (sleep 1)))))

;; -----------------------------------------------
;; PROMPT ----------------------------------------
;; -----------------------------------------------
(defun execute-command (command)
  (if (string= command "x")
    nil
    t))

(defun prompt ()
  (loop :while (execute-command (prompt-read "> ")))
  0)

;; -----------------------------------------------
;; MAIN ------------------------------------------
;; -----------------------------------------------
(defun main ()
  (setf *monome* (make-monome :host-name "Eriks-MacBook-Air.local."
                              :host-port 15932))
  (monome-connect *monome*)
  (make-thread
    (lambda ()
      (monome-listen *monome* *connect-port*)))
  (make-thread
    (lambda ()
      (sweep)))
  (prompt)
  (sb-ext:quit))

;; -----------------------------------------------
;; -----------------------------------------------
;; -----------------------------------------------
(main)

;; (handler-case (main)
;;   (sb-sys:interactive-interrupt ()
;;     (format t "I'm your friendly interrupt handler.~%")
;;     (sb-ext:quit)))

;; (handler-case (loop)
;;   (sb-sys:interactive-interrupt ()
;;     (format t "friendly~%")
;;     (sb-ext:quit)))

