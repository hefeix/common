;;; Random useful utilities.
;;; Copyright (c) 1993 by Olin Shivers.

(require 'srfi-1)

(define (mapv f v)
  (let* ((len (vector-length v))
         (ans (make-vector len)))
    (do ((i 0 (+ i 1)))
        ((= i len) ans)
      (vector-set! ans i (f (vector-ref v i))))))

(define (mapv! f v)
  (let ((len (vector-length v)))
    (do ((i 0 (+ i 1)))
        ((= i len) v)
      (vector-set! v i (f (vector-ref v i))))))

(define (vector-every? pred v)
  (let lp ((i (- (vector-length v) 1)))
    (or (< i 0)
        (and (pred (vector-ref v i))
             (lp (- i 1))))))

(define (copy-vector v)
  (let* ((len (vector-length v))
         (ans (make-vector len)))
    (do ((i (- len 1) (- i 1)))
        ((< i 0) ans)
      (vector-set! ans i (vector-ref v i)))))

(define (initialize-vector len init)
  (let ((v (make-vector len)))
    (do ((i (- len 1) (- i 1)))
        ((< i 0) v)
      (vector-set! v i (init i)))))

(define (vector-append . vecs)
  (let* ((vlen (fold (lambda (v len) (+ (vector-length v) len)) 0 vecs))
         (ans (make-vector vlen)))
    (let lp1 ((vecs vecs) (to 0))
      (if (pair? vecs)
          (let* ((vec (car vecs))
                 (len (vector-length vec)))
            (let lp2 ((from 0) (to to))
              (cond ((< from len)
                     (vector-set! ans to (vector-ref vec from))
                     (lp2 (+ from 1) (+ to 1)))
                    (else (lp1 (cdr vecs) to)))))))
    ans))


(define (vfold kons knil v)
  (let ((len (vector-length v)))
    (do ((i 0 (+ i 1))
         (ans knil (kons (vector-ref v i) ans)))
        ((>= i len) ans))))

(define (vfold-right kons knil v)
  (do ((i (- (vector-length v) 1) (- i 1))
       (ans knil (kons (vector-ref v i) ans)))
      ((< i 0) ans)))

(define (deprecated-proc proc name . maybe-preferred-msg)
  (let ((warned? #f))
    (lambda args
      (cond ((not warned?)
             (set! warned? #t)
             (apply error
                    "Deprecated procedure (may not be supported in a future release)"
                    name
                    maybe-preferred-msg)))
      (apply proc args))))


(define (real->exact-integer x)
  (let ((f (round x)))
    (if (inexact? f) (inexact->exact f) f)))

#|

;--------------
; Run thunk1 until thunk2 escapes
; This is *extremly* low level
; Don't use unless you know what you are doing

(define (run-as-long-as thunk1 thunk2 spawn-thread . name)
  (let ((thread (make-placeholder)))
    (apply spawn-thread
           (lambda ()
             (placeholder-set! thread (current-thread))
             (thunk1))
           name)
    (dynamic-wind
     (lambda () #t)
     thunk2
     (lambda ()
       (terminate-thread! (placeholder-value thread))))))

(define (obtain-all-or-none . locks)
  (let lp ((obtained '()) (needed locks))
    (if (not (null? needed))
        (let ((next (car needed)))
          (if (maybe-obtain-lock next)
              (lp (cons next obtained)
                  (cdr needed))
              (begin
                (for-each release-lock obtained)
                (obtain-lock next)
                (lp (list next) (delete next locks eq?))))))))

|#

;; loop while expression false
(define-syntax until
  (syntax-rules ()
    ((until test body ...)
     (let loop ()
       (unless test
         body ...
         (loop))))))

(define-syntax repeat
  (syntax-rules ()
    ((repeat n body ...)
     (let loop ((i n))
       (when (< 0 i)
         body ...
         (loop (sub1 i)))))))

(define-syntax while
  (syntax-rules ()
    ((while test body ...)
     (let loop ()
       (if test
           (begin
             body ...
             (loop)))))))

;; repeat body n times, w/ countup n bound to v
(define-syntax dotimes
  (syntax-rules ()
    ((dotimes (v n) body ...)
     (dotimes (v n (begin)) body ...))
    ((dotimes (v n f) body ...)
     (let loop ((v 0) (nv n))
       (if (< v nv)
           (begin
             body ...
             (loop (add1 v) nv))
           f)))))

;;----------------------------------------------------------------------
;; list utilities

;; Returns the original list starting at element n.
(define (skip+ ls n)
  (if (or (null? ls) (<= n 0))
      (values ls n)
      (skip+ (cdr ls) (sub1 n))))

;; Returns new list with all elements [0 n-1] and original list from n.
;; The new list is padded upto n elements from pads, when supplied.
;; Returns partial split when fewer than n elements are available,
;; either from the primary or pad list, or no split when pads is #f. Default is
;; no padding & paritial section.
(define (split-at+ ls n #!optional (pads '()))
  (if (null? ls) (values '() '())
      (let loop ((ls ls) (n n) (part '()))
        (cond
          ((<= n 0)
           (values (reverse! part) ls))
          ((null? ls)
           (cond
             ((not pads)   (values '() '()))
             ((null? pads) (values (reverse! part) '()))
             (else         (values (append-reverse! part (split-at+ pads n))
                                   '()))))
          (else
           (loop (cdr ls) (sub1 n) (cons (car ls) part)))))))

;; Returns sublists of length n from the list, the last sublist padded, if
;; necessary and possible, from pads. The sublists are constructed starting
;; at every step element.
;; ls - list
;; n - elements per section
;; step - elements between section
;; pads - remainder fill
(define (section ls n #!optional (step n) (pads '()))
  (check-positive-fixnum 'section n 'size)
  (check-positive-fixnum 'section step 'step)
  (cond
    ((null? ls) '())
    (else
     (let ((inc (- step n)))
       (let loop ((ls ls) (parts '()))
         (let-values (((part nls) (split-at+ ls n pads)))
           (cond
             ((null? nls)
              (if (null? part) (reverse! parts)
                  (reverse! (cons part parts))))
             (else
              (let ((ls (cond
                          ((zero? inc)      nls)
                          ((negative? inc)  (skip+ ls (+ n inc)))
                          (else             (skip+ nls inc)))))
                (loop ls (cons part parts)))))))))))

;; shift! with a variable
(define-syntax shift!/set
  (syntax-rules ()
    ((_ ?var)
     (shift!/set ?var #f))
    ((_ ?var ?empval)
     (if (not (pair? ?var)) ?empval
         (let ((_tmp (car ?var)))
           (set! ?var (cdr ?var))
           _tmp)))))

;; List of length = 0?
(define-syntax length=0?
  (syntax-rules ()
    ((_ ?obj)
     (null? ?obj))))

;; List of length = 1?
(define-syntax length=1?
  (syntax-rules ()
    ((_ ?obj)
     (let ((_obj ?obj))
       (and (pair? _obj) (null? (cdr _obj)))))))

;; List of length > 1?
(define-syntax length>1?
  (syntax-rules ()
    ((_ ?obj)
     (let ((_obj ?obj))
       (and (pair? _obj) (pair? (cdr _obj)))))))

;; List of length = 2?
(define-syntax length=2?
  (syntax-rules ()
    ((_ ?obj)
     (let ((_obj ?obj))
       (and (length>1? _obj) (null? (cddr _obj)))))))

;; Returns a list
(define-syntax ensure-list
  (syntax-rules ()
    ((_ ?obj)
     (let ((_obj ?obj))
       (or (and (list? _obj) _obj)
           (list _obj))))))

;; Returns #f if given list is empty and the list itself otherwise
;; It is intended for emulation of MIT-style empty list treatment
;; (not-null? <list>) may be considered as a counterpart to MIT-style <list>
(define-syntax not-null?
  (syntax-rules ()
    ((_ ?obj)
     (let ((_obj ?obj))
       (and (not (null? _obj))
            _obj)))))

;; Remove 1st matching elements from the alist (functional)
(define-syntax alist-delete-first
  (syntax-rules ()
    ((_ ?key ?als)
     (alist-delete-first ?key ?als eqv?))
    ((_ ?key ?als ?=)
     (alist-delete/count ?key ?als ?= 1))))

;; Remove 1st matching elements from the alist (destructive)
(define-syntax alist-delete-first!
  (syntax-rules ()
    ((_ ?key ?als)
     (alist-delete-first ?key ?als eqv?))
    ((_ ?key ?als ?=)
     (alist-delete!/count ?key ?als ?= 1))))

;; Some alist search macros.
;; Supplied default maybe a thunk or other.
;; The default is an error.
(define-syntax assoc-def
  (syntax-rules ()
    ((_ ?key ?als)
     (assoc-def ?key ?als equal?))
    ((_ ?key ?als ?=)
     (or (assoc ?key ?als ?=)
         (error 'assoc-def "key not found" ?key)))
    ((_ ?key ?als ?= ?def)
     (or (assoc ?key ?als ?=)
         (if (procedure? ?def) (?def) ?def)))))

(define-syntax assq-def
  (syntax-rules ()
    ((_ ?key ?als)
     (or (assq ?key ?als)
         (error 'assq-def "key not found" ?key)))
    ((_ ?key ?als ?def)
     (or (assq ?key ?als)
         (if (procedure? ?def) (?def) ?def)))))

(define-syntax assv-def
  (syntax-rules ()
    ((_ ?key ?als)
     (or (assv ?key ?als)
         (error 'assv-def "key not found" ?key)))
    ((_ ?key ?als ?def)
     (or (assv ?key ?als)
         (if (procedure? ?def) (?def) ?def)))))

;;
;; Note - the order is preserved!
;; (<key>1 <val>1 ... <key>n <val>n) ->
;; ((<key>1 . <val>1) ... (<key>n . <val>n))
(define (plist->alist pls)
  (let loop ((pls pls) (als '()))
    (if (null? pls) (reverse! als)
        (let ((hd (car pls))
              (tl (cdr pls)))
          (if (null? tl) (error-plist 'plist->alist pls)
              (loop (cdr tl) (cons (cons hd (car tl)) als)))))))

(define (alist->plist als)
  (let loop ((als als) (pls '()))
    (if (null? als) (reverse! pls)
        (let ((elt (car als)))
          (if (not (pair? elt)) (error-alist 'alist->plist als)
              (loop (cdr als) (cons* (cdr elt) (car elt) pls)))))))

;; Search the alist from back to front.
(define (alist-inverse-ref val alist #!optional (cmp eqv?) default)
  (let ((elt (rassoc val alist cmp)))
    (if elt (car elt)
        default )))

;; Remove 1st N matching elements from the alist (functional)
(define (*alist-delete/count loc key al cmp cnt)
  (check-procedure loc cmp)
  (let loop ((cal al) (cnt (check-fixnum loc cnt)) (oal '()))
    (cond
      ((null? cal)
       (reverse! oal))
      ((pair? cal)
       (let ((elt (car cal))
             (nxt (cdr cal)))
         (if (not (pair? elt)) (error-alist loc  al)
             (if (positive? cnt)
                 (if (cmp key (car elt))
                     (loop nxt (sub1 cnt) oal)
                     (loop nxt cnt (cons elt oal)))
                 (loop nxt 0 (cons elt oal))))))
      (else
       (error-alist loc al)))))

(define (alist-delete/count key al #!optional (cmp eqv?) (cnt 1073741823))
  (*alist-delete/count 'alist-delete/count key al cmp cnt))

;; Remove 1st N matching elements from the alist (destructive)
(define (*alist-delete!/count loc key al cmp cnt)
  (check-procedure loc cmp)
  (let ((ral al))
    (let loop ((cal al) (pal #f) (cnt (check-fixnum loc cnt)))
      (cond
        ((or (null? cal) (not (positive? cnt)))
         ral )
        ((pair? cal)
         (let ((elt (car cal))
               (nxt (cdr cal)))
           (if (not (pair? elt)) (error-alist loc al)
               (cond
                 ((cmp key (car elt))
                  (if pal (set-cdr! pal nxt)
                      (set! ral nxt))
                  (loop nxt pal (sub1 cnt)))
                 (else
                  (loop nxt cal cnt))))))
        (else
         (error-alist loc al))))))

(define (alist-delete!/count key al #!optional (cmp eqv?) (cnt 1073741823))
  (*alist-delete!/count 'alist-delete!/count key al cmp cnt))

;; Returns alist of improper lists
;; The keys & vals lists must be of the same length!
;; This works with any proper list, not just an alist.
(define (zip-alist keys vals)
  (unless (= (length (check-list 'zip-alist keys))
             (length (check-list 'zip-alist vals)))
    (error 'zip-alist "lists are not of same length" keys vals))
  (map cons keys vals))

;; Split alist into (values keys vals)
(define (unzip-alist al)
  (let loop ((al (check-list 'unzip-alist al)) (keys '()) (vals '()))
    (if (null? al) (values (reverse! keys) (reverse! vals))
        (let ((elt (car al)))
          (if (not (pair? elt)) (error-alist 'unzip-alist al)
              (loop (cdr al) (cons (car elt) keys) (cons (cdr elt) vals)))))))

;;; Handy little things:
(define (shift! ls #!optional default)
  (if (null? ls) default
      (begin
        (check-pair 'shift! ls)
        (let ((x (car ls))
              (d (cdr ls)))
          (check-pair 'shift! d)
          (set-car! ls (car d))
          (set-cdr! ls (cdr d))
          x ))))

(define (unshift! x ls)
  (check-pair 'unshift! ls)
  (set-car! ls x)
  (set-cdr! ls (cons (car ls) (cdr ls)))
  ls )

;;
(define (andmap func ls0 . rest)
  (cond
    ((null? rest)
     (let mapf ((ls ls0))
       (or (null? ls)
           (and (func (car ls))
                (mapf (cdr ls))))))
    ((null? (cdr rest))
     (let mapf ((ls1 ls0) (ls2 (car rest)))
       (or (null? ls1)
           (and (func (car ls1) (car ls2))
                (mapf (cdr ls1) (cdr ls2))))))
    (else
     (let mapf ((ls0 ls0) (rest rest))
       (or (null? ls0)
           (and (apply func (car ls0) (map car rest))
                (mapf (cdr ls0) (map cdr rest))))))))

(define (ormap func ls0 . rest)
  (and (pair? ls0)
       (let ((rest (cons ls0 rest)))
         (or (apply func (map car rest))
             (apply ormap func (map cdr rest))))))

;;----------------------------------------------------------------------
;; list-of.scm - by Phil Bewig

(define-syntax fold-of
  (syntax-rules (range in is)
    ((_ "aux" op acc expr) (set! acc (op acc expr)))
    ((_ "aux" op acc expr (var range first past step) clause ...)
     (let* ((f first) (p past) (s step) (le? (if (positive? s) <= >=)))
       (do ((var f (+ var s))) ((le? p var) acc)
         (fold-of "aux" op acc expr clause ...))))
    ((_ "aux" op acc expr (var range first past) clause ...)
     (let* ((f first) (p past) (s (if (< f p) 1 -1)))
       (fold-of "aux" op acc expr (var range f p s) clause ...)))
    ((_ "aux" op acc expr (v in vs) clause ...)
     (do ((lst vs (cdr lst))) ((null? lst) acc)
       (let ((v (car lst))) (fold-of "aux" op acc expr clause ...))))
    ((_ "aux" op acc expr (x is y) clause ...)
     (let ((x y)) (fold-of "aux" op acc expr clause ...)))
    ((_ "aux" op acc expr pred? clause ...)
     (if pred? (fold-of "aux" op acc expr clause ...)))
    ((_ op base expr clause ...)
     (let ((acc base)) (fold-of "aux" op acc expr clause ...)))))

(define-syntax list-of
  (syntax-rules ()
    ((_ arg ...)
     (reverse (fold-of (lambda (d a) (cons a d)) '() arg ...)))))

;;----------------------------------------------------------------------
;; clojure syntax sugar
;; (doto as-> -> ->* ->> ->>* if-let if-let*)

;; originally contributed by Martin DeMello
;; rewritten in terms of syntax-rules by Moritz Heidkamp
(define-syntax doto
  (syntax-rules ()
    ((_ x) x)
    ((_ x (fn args ...) ...)
     (let ((val x))
       (fn val args ...)
       ...
       val))))

(define-syntax as->
  (syntax-rules ()
    ((_ state name) state)
    ((_ state name expr)
     (let ((name state))
       expr))
    ((_ state name expr rest ...)
     (as-> (let ((name state))
             expr) name rest ...))))

(define-syntax ->
  (syntax-rules ()
    ((_ x) x)
    ((_ x (y z ...) rest ...)
     (-> (y x z ...) rest ...))
    ((_ x y rest ...)
     (-> x (y) rest ...))))

(define-syntax ->>
  (syntax-rules ()
    ((_ x) x)
    ((_ x (y ...) rest ...)
     (->> (y ... x) rest ...))
    ((_ x y rest ...)
     (->> x (y) rest ...))))

(define-syntax ->*
  (syntax-rules ()
    ((_ x) x)
    ((_ x (y z ...) rest ...)
     (->* (receive args x
            (apply y (append args (list z ...))))
          rest ...))
    ((_ x y rest ...)
     (->* x (y) rest ...))))

(define-syntax ->>*
  (syntax-rules ()
    ((_ x) x)
    ((_ x (y z ...) rest ...)
     (->>* (receive args x
             (apply y (append (list z ...) args)))
           rest ...))
    ((_ x y rest ...)
     (->>* x (y) rest ...))))

(define-syntax if-let
  (syntax-rules ()
    ((_ (x y) then else)
     (let ((x y))
       (if x then else)))))

(define-syntax if-let*
  (syntax-rules ()
    ((_ ((x y) more ...) then else)
     (car (or (and-let* ((x y) more ...)
                (list then))
              (list else))))))

;;----------------------------------------------------------------------
