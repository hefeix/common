(define-syntax benchmark-measure
  (syntax-rules ()
    ((_  ?code)
     (let ((start  (%gettime/microsecs))
           (result ?code)
           (stop   (%gettime/microsecs)))
       (if (or (< start 0.0) (< stop 0.0))
           (error "Could not retrieve time reliably"))
       (- stop start)))))

(define (%gettime/microsecs)
  (long (java.lang.System:currentTimeMillis)))

(define (current-benchmark-iterations) 100)

;; run the given procedure n times and return statistics about the runtime
;; returns an alist with statistics
(define-syntax benchmark-run
  (syntax-rules ()
    ((_ ?code)
     (benchmark-run (current-benchmark-iterations) ?code))
    ((_ ?iterations ?code)
     (let ((runtimes (list-tabulate ?iterations (lambda _ (benchmark-measure ?code)))))
       (generate-statistics runtimes)))))

;; should we also add percentiles to give a pointer on what to improve?
;; like the 95%?
(define (generate-statistics runtimes)
  (let ((m (mean runtimes)))
    `((max  . ,(apply max runtimes))
      (min  . ,(apply min runtimes))
      (mean . ,m)
      (standard-deviation . ,(sample-standard-deviation runtimes m)))))

(define (mean values)
  (/ (apply + values) (length values)))

(define (sample-variance values #!optional (m (mean values)))
  (let ((amount (length values)))
    (/ (fold (lambda (elt acc)
               (+ acc (expt (- elt m) 2)))
             0
             values)
       (- amount 1))))

(define (sample-standard-deviation values #!optional (m (mean values)))
  (let ((sample-size (length values)))
    (if (<= 0 sample-size 1)
        0.0
        (sqrt (sample-variance values m)))))

;; benchmarking that tries to find out how many times your code can run in a given
;; amount of time. Idea taken from: https://github.com/evanphx/benchmark-ips
;; the idea is to find out how many iterations we can do in a certain (small) amount
;; of time and then tell how much iterations we can make for a fixed timeframe.
;; This is for cases where you don't want to guess the amount of iterations
;; that are needed to produce values that you can work with
(define-syntax benchmark-ips
  (syntax-rules ()
    ((_ ?code)
     (benchmark-ips 5 ?code))
    ((_ ?seconds ?code)
     (benchmark-ips* (lambda () ?code) ?seconds))))

(define-syntax dotimes
  (syntax-rules ()
    ((_ ?n ?code ...)
     (let loop ((n ::int ?n))
       (when (positive? n)
         ?code ...
         (loop (- n 1)))))))

(define (iterations-per-100ms thunk limit)
  (let ((before (current-milliseconds))
        (threshold (+ limit (current-milliseconds))))
    (let loop ((iterations ::int 0))
      (cond
        ((< (current-milliseconds) threshold)
         (thunk)
         (loop (+ 1 iterations)))
        (else
         (let* ((after (current-milliseconds))
                (total-time (- after before))
                (per-100ms  (inexact->exact (round (/ (* iterations 100) total-time)))))
           (cons (max 1 per-100ms) total-time)))))))

(define (benchmark-ips* thunk #!optional (time 5) (warmup 2))
  (let* ((per-100ms (car (iterations-per-100ms thunk (* 1000 warmup))))
         (threshold  (+ (* time 1000) (current-milliseconds))))
    (let loop ((iterations 0) (timings (list)))
      (cond
        ((< (current-milliseconds) threshold)
         (let ((before (current-milliseconds))
               (_ (dotimes per-100ms (thunk)))
               (after  (current-milliseconds)))
           (loop (+ iterations per-100ms) (cons (- after before) timings))))
        (else
         (let ((all-ips (map (lambda (i) (/ per-100ms (/ i 1000.0))) timings)))
           `((mean . ,(mean all-ips))
             (standard-deviation . ,(sample-standard-deviation all-ips)))))))))
