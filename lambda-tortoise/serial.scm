;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2015
;;      "Mu Lei" known as "NalaGinrut" <mulei@gnu.org>
;;  This file is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License published
;;  by the Free Software Foundation, either version 3 of the License,
;;  or (at your option) any later version.

;;  This file is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program. If not, see <http://www.gnu.org/licenses/>.

(define-module (lambda-tortoise serial)
  #:use-module (system foreign)
  #:use-module ((rnrs) #:select (bytevector-s32-native-ref)) 
  #:export (init-tortoise *tortoise*))

(define *tortoise* #f)
(define-public B1152000 0010011)
(define-public B38400 0000017)
(define VTIME 5)
(define VMIN 6)
(define CS8 60)
(define CREAD 200)
(define CLOCAL 4000)
(define TCSANOW 0)

(define %libc-errno-pointer
  ;; Glibc's 'errno' pointer.
  (let ((errno-loc (dynamic-func "__errno_location" (dynamic-link))))
    (and errno-loc
         (let ((proc (pointer->procedure '* errno-loc '())))
           (proc)))))

(define errno
  (if %libc-errno-pointer
      (let ((bv (pointer->bytevector %libc-errno-pointer (sizeof int))))
        (lambda ()
          "Return the current errno."
          ;; XXX: We assume that nothing changes 'errno' while we're doing all this.
          ;; In particular, that means that no async must be running here.

          ;; Use one of the fixed-size native-ref procedures because they are
          ;; optimized down to a single VM instruction, which reduces the risk
          ;; that we fiddle with 'errno' (needed on Guile 2.0.5, libc 2.11.)
          (let-syntax ((ref (lambda (s)
                              (syntax-case s ()
                                ((_ bv)
                                 (case (sizeof int)
                                   ((4)
                                    #'(bytevector-s32-native-ref bv 0))
                                   ((8)
                                    #'(bytevector-s64-native-ref bv 0))
                                   (else
                                    (error "unsupported 'int' size"
                                           (sizeof int)))))))))
            (ref bv))))
      (lambda () 0)))

(define termios-struct
  (list
   unsigned-int   ; c_iflag,  input mode flags
   unsigned-int   ; c_oflag,  output mode flags
   unsigned-int   ; c_cflag,  control mode flags
   unsigned-int   ; c_lflag,  local mode flags
   uint8          ; c_line,   line discipline
   (make-list 32 uint8) ; c_cc, control characters
   unsigned-int   ; c_ispeed, input speed
   unsigned-int)) ; c_ospeed, output speed

(define-syntax-rule (ffi-define name ret args)
  (define-public name
    (pointer->procedure
     ret (dynamic-func (symbol->string 'name) (dynamic-link)) args)))

(ffi-define tcgetattr int (list int '*))
(ffi-define tcsetattr int (list int int '*))
(ffi-define cfsetspeed int (list '* unsigned-int))
(ffi-define cfsetispeed int (list '* unsigned-int))
(ffi-define cfsetospeed int (list '* unsigned-int))
(ffi-define tcflush int (list int int))

(define-syntax-rule (try what)
  (when (< what 0) (error "ERROR: " (strerror (errno)))))

(define (init-attr)
  (let ((cc (make-list 32 0)))
    (list-set! cc VMIN 1)  ; read doesn't block
    (list-set! cc VTIME 5) ; 0.5 seconds read timeout
    (make-c-struct
     termios-struct
     (list 0 0
           ;; 8-bit chars, enable reading, ignore modem controls
           (logior CS8 CREAD CLOCAL)
           0 0 cc 0 0))))

(define* (init-tortoise #:key (dev "/dev/rfcomm0") (speed 38400))
  (when (not (file-exists? dev)) (error "Device doesn't exist!" dev))
  (let ((sp (open dev (logior O_RDWR O_NOCTTY #;O_NONBLOCK)))
        (attr (init-attr)))
    (when (not (isatty? sp)) (error "Invalid TTY device!" dev))
    (try (cfsetspeed attr speed))
    (try (tcsetattr (port->fdes sp) TCSANOW attr))
    ;;(fcntl sp F_SETFL O_NONBLOCK)
    (set! *tortoise* sp)
    (format sp "init~%~!")
    "ok"))
