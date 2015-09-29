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

(define-module (lambda-tortoise instructions)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match))

(define-syntax-rule (define-instruction name doc handle ...)
  (define-public name
    (lambda args
      doc
      (match args
        handle ...
        (else (error (format #f "Invalid instruction usage: ~a~%" doc)))))))

(define-syntax-rule (->instr name args ...)
  (format #f "~{~a~^ ~} ~a" (list args ...) 'name))

(define-instruction penup "(penup)" (() (->instr penup)))
(define-instruction pendown "(pendown)" (() (->instr pendown)))

;; equilateral triangle
(define-instruction e-tri "(e-tri n)" ((n) (->instr e-tri n)))

(define-instruction hexagon "(hexagon n)" ((n) (->instr hexagon n)))

(define-instruction point "(point x y)"
  (((? integer? x) (? integer? y)) (->instr point x y)))

(define-instruction forward "(forward nSteps)"
  ((? integer? n) (->instr forward n)))

(define-instruction back "(back nSteps)"
  ((? integer? n) (->instr back n)))

(define-instruction left "(left nSteps)" (() (->instr left)))
(define-instruction right "(right nSteps)" (() (->instr right)))

;; Bresenham Circle Drawing Algorithm:
;; http://www.ecse.rpi.edu/~wrf/Research/Short_Notes/bresenham.html
(define (draw-circle r)
  (let ((x 0) (y r) (d (- r)) (end (/ r (sqrt 2))))
    (point 0 r)
    (let lp((x 1) (y y) (d (+ d (* 2 x) -1)))
      (cond
       ((>= x end) (format #t "Draw circle end!~%"))
       ((>= d 0) (lp x (1- y) (- d (* 2 (1- y)))))
       (else
        (point x y)
        (lp (1+ x) y (+ d (* 2 x) -1)))))))
(define-instruction circle "(circle Rmm)" ((? integer? r) (draw-circle r)))

;; Bresenham Line Drawing Algorithm:
;; http://www.ecse.rpi.edu/~wrf/Research/Short_Notes/bresenham.html
(define (draw-line x0 y0 x1 y1)
  (let ((m (* 2 y1)) (d (- x1)))
    (point 0 0)
    (let lp((x 1) (y 0) (d (+ d m)))
      (cond
       ((> x x1) (format #t "Draw line end!~%"))
       ((>= d 0) (lp x (1+ y) (- d (* 2 x1))))
       (else
        (point x y)
        (lp (1+ x) y (+ d m)))))))
(define-instruction line "(line x0 y0 x1 y1)"
  (((? integer? x0) (? integer? y0) (? integer? x1) (? integer? y1))
   (draw-line x0 y0 x1 y1)))

(define-instruction ellipse "(ellipse a b)"
  (((? integer? a) (? integer? b)) (->instr ellipse a b)))

(define (draw-square n)
  (forward n) (right)
  (forward n) (right)
  (forward n) (right)
  (forward n))
(define-instruction square "(square nMM)" ((? integer? n) (draw-square n)))

(define (draw-rect len width)
  (forward len) (right)
  (forward width) (right)
  (forward len) (right)
  (forward width))
(define-instruction rect "(rect lenMM widMM)"
  (((? integer? len) (? integer? width))
   (draw-rect len width)))
