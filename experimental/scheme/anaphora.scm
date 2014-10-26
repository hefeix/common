; Author: Juergen Lorenz
; ju (at) jugilo (dot) de
;
; Copyright (c) 2011-2013, Juergen Lorenz
; All rights reserved.
; 
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are
; met:
; 
; Redistributions of source code must retain the above copyright
; notice, this list of conditions and the following disclaimer.
; 
; Redistributions in binary form must reproduce the above copyright
; notice, this list of conditions and the following disclaimer in the
; documentation and/or other materials provided with the distribution.
; 
; Neither the name of the author nor the names of its contributors may be
; used to endorse or promote products derived from this software without
; specific prior written permission. 
; 
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
; HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
; 
; Last update: Dec 11, 2013
;
#|[
;Inspired by Paul Graham's classic "On Lisp" this module introduces
;anaphoric macros, which are unhygienic by design. Hence they can not be
;implemented with syntax-rules! In fact, they introduce new identifiers
;behind the scene, mostly named it, which can be referenced in the body
;without being declared. Please note, that this identifier is not
;renamed! 
]|#

(module anaphora

(export anaphora
        aif nif
        alambda nlambda
        awhen nwhen
        acond ncond
        awhile nwhile
        aand nand
        define-anaphor
        define-properties alist-recurser atree-recurser
        tree-recurser list-recurser)
(import scheme (only chicken case-lambda gensym print))

;;; (aif test consequent [alternative])
;;; -----------------------------------
;;; anaphoric if, where consequent and alternative can refer to result
;;; of test named it
(define-syntax aif
  (ir-macro-transformer
    (lambda (form inject compare?)
      (let ((it (inject 'it)))
        (let (
          (test (cadr form))
          (consequent (caddr form))
          (alternative (cdddr form))
          )
          (if (null? alternative)
            `(let ((,it ,test))
               (if ,it ,consequent))
            `(let ((,it ,test))
               (if ,it ,consequent ,(car alternative)))))))))

;;; (nif name test consequent [alternative])
;;; ----------------------------------------
;;; named if, where consequent and alternative can refer to result
;;; of test named name
(define-syntax nif
  (syntax-rules ()
    ((_ name test consequent)
     (let ((name test))
       (if name consequent)))
    ((_ name test consequent alternative)
     (let ((name test))
       (if name consequent alternative)))))

;;; (awhen test xpr . xprs)
;;; -----------------------
;;; anaphoric when, where xpr ... can refer to result of test
;;; named it
(define-syntax awhen
  (ir-macro-transformer
    (lambda (form inject compare?)
      (let ((it (inject 'it)))
        (let (
          (test (cadr form))
          (xpr (caddr form))
          (xprs (cdddr form))
          )
          `(let ((,it ,test))
             (if ,it (begin ,xpr ,@xprs))))))))

;;; (nwhen name test xpr . xprs)
;;; ----------------------------
;;; named when, where xpr ... can refer to result of test
;;; named name
(define-syntax nwhen
  (syntax-rules ()
    ((_ name test xpr . xprs)
     (let ((name test))
       (if name (begin xpr . xprs))))))

;;; (acond (test xpr ...) ... [(else ypr ...)])
;;; -------------------------------------------
;;; anaphoric cond, where each test is bound to it and else to #t.
(define-syntax acond
  (ir-macro-transformer
    (lambda (form inject compare?)
      (let ((it (inject 'it)))
        (let ((clauses (cdr form)))
          (let loop ((clauses clauses))
            (if (null? clauses)
              #f
              (let* (
                (clause (car clauses))
                (cnd (car clause))
                )
                `(let ((sym ,(if (compare? cnd 'else) #t cnd)))
                   (if sym
                     (let ((,it sym))
                       ,@(cdr clause))
                     ,(loop (cdr clauses))))))))))))

;;; (ncond name (test xpr ...) ... [(else ypr ...)])
;;; ------------------------------------------------
;;; anaphoric cond, where each test is bound to name and else to #t.
(define-syntax ncond
  (syntax-rules (else)
    ((_ name) #f)
    ((_ name (else xpr . xprs) . clauses)
     (let ((sym #t))
       (if sym
         (let ((name sym)) xpr . xprs)
         #f)))
    ((_ name (test xpr . xprs) . clauses)
     (let ((sym test))
       (if sym
         (let ((name sym)) xpr . xprs)
         (ncond name . clauses))))))

;;; (awhile test xpr . xprs)
;;; ------------------------
;;; anaphoric while, where each xpr ... can refer to the result of
;;; the successive test, named it
(define-syntax awhile
  (ir-macro-transformer
    (lambda (form inject compare?)
      (let ((it (inject 'it)))
        (let (
          (test (cadr form))
          (xpr (caddr form))
          (xprs (cdddr form))
          )
          `(let loop ((,it ,test))
             (when ,it
               ,xpr ,@xprs
               (loop ,test))))))))

;;; (nwhile name test xpr . xprs)
;;; -----------------------------
;;; named while, where each xpr ... can refer to the result of
;;; the successive test, named name
(define-syntax nwhile
  (syntax-rules ()
    ((_ name test xpr . xprs)
     (let loop ((name test))
       (when name
         (begin xpr . xprs)
         (loop test))))))

;;; (aand . args)
;;; -------------
;;; anaphoric and, where each successive argument can refer to the
;;; result of the previous argument, named it.
(define-syntax aand
  (ir-macro-transformer
    (lambda (form inject compare?)
      (let ((it (inject 'it)))
        (let ((args (cdr form)))
          (let loop ((args args))
            (cond 
              ((null? args) #t)
              ((null? (cdr args)) (car args))
              (else 
                `(let ((,it ,(car args)))
                   (if ,it
                     ,(loop (cdr args))
                     #f))))))))))

;;; (nand name . args)
;;; ------------------
;;; named and, where each successive argument can refer to the
;;; result of the previous argument, named name.
(define-syntax nand
  (syntax-rules ()
    ((_ name) #t)
    ((_ name arg) arg)
    ((_ name arg0 arg1 ...)
     (let ((name arg0))
       (if name (nand name arg1 ...))))))

;;; (alambda args xpr . xprs)
;;; -------------------------
;;; anaphoric lambda, where the body xpr ... can refer to self, so that
;;; recursion is possible
(define-syntax alambda
  (ir-macro-transformer
    (lambda (form inject compare?)
      (let ((self (inject 'self)))
        (let ((args (cadr form)) (body (cddr form)))
          `(letrec ((,self (lambda ,args ,@body)))
             ,self))))))

;;; (nlambda name args xpr . xprs)
;;; ------------------------------
;;; named lambda, where the body xpr ... can refer to name, so that
;;; recursion is possible
(define-syntax nlambda
  (syntax-rules ()
    ((_ name args xpr . xprs)
     (letrec ((name (lambda args xpr . xprs)))
       name))))

#|[
Most of  the anaphoric macros above could be generated automatically by
means of the following macro, define-anaphor, which generates another
macro defining it. It accepts three arguments, the name of the new
macro to be defined, the name of the procedure or macro on which the
anaphoric macro is patterned and a rule transforming the latter into the
former, presently one of the procedures cascade-it and first-it.
cascade-it produces a cascade of variables named it, storing the
values of the previous arguments as in aand above, where first-it stores
only the first argument as variable it to be used in any of the
following arguments as in awhen above.  So we could have defined them as

  (define-anaphor aand and cascade-it)
  (define-anaphor awhen when first-it)

and used as follows

  (aand '(1 2 3) (cdr it) (cdr it)) ; -> '(3)
  (awhen (! 5) it (* 2 it)) ; -> 240

where ! is the factorial.
But note, that define-anaphor could be used for any function as well,
for example

  (define-anaphor a* * cascade-it)
  (a* 10 (* 2 it) (+ 5 it)) ; -> 35
]|#

;;; (define-anaphor name from rule)
;;; -------------------------------
;;; defines an anaphoric macro, name, patterned after the fuction or
;;; macro from and transformed according to rule, one of the symbols
;;; cascade or first. 
;;; Note, that this macro is hygienic, but it creates an anaphoric one.
(define-syntax define-anaphor
  (syntax-rules ()
    ((_ name from rule)
     (define-syntax name
       (er-macro-transformer
         (lambda (form rename compare?)
           (let ((%let (rename 'let)) (%let* (rename 'let*)))
             (letrec (
               (cascade-it
                 (lambda (op args)
                   (let loop ((args args) (xpr `(,op)))
                     (if (null? args)
                       xpr
                       (let ((sym (gensym)))
                         `(,%let* ((,sym ,(car args)) (it ,sym))
                                  ,(loop (cdr args)
                                         (append xpr (list sym)))))))))
               (first-it
                 (lambda (op args)
                   `(,%let ((it ,(car args)))
                           (,op it ,@(cdr args)))))
               )
               (case rule
                 ((#:cascade)
                  (cascade-it 'from (cdr form)))
                 ((#:first)
                  (first-it 'from (cdr form)))
                 (else
                   (error 'define-anaphor
                       "rule must be one of #:cascade or #:first")))))))))))
;(define-syntax define-anaphor
;  (syntax-rules ()
;    ((_ name from rule)
;     (define-syntax name
;       (er-macro-transformer
;         (lambda (form rename compare?)
;           (rule 'from (cdr form) rename)))))))
;
;(define (first-it op args rename)
;  (let ((%let (rename 'let)))
;    `(,%let ((it ,(car args)))
;            (,op it ,@(cdr args)))))
;
;(define (cascade-it op args  rename)
;  (let ((%let* (rename 'let*)))
;    (let loop ((args args) (xpr `(,op)))
;      (if (null? args)
;        xpr
;        (let ((sym (gensym)))
;          `(,%let* ((,sym ,(car args)) (it ,sym))
;                   ,(loop (cdr args) (append xpr (list sym)))))))))

#|[
The following macro defines new macros masking property-accessors and
-mutators get and put!  For each supplied identifier, prop, another
identifier, prop!, is constructed behind the scene. The former will be
the accessor, the latter the mutator. So
  (prop sym)
is expands into
  (get sym 'prop)
and
  (prop! sym val)
into
  (put! sym 'prop val)
Note how the new names with the ! suffix are generated at compile time,
i.e. within an unquote. Note also the use of the injection argument, i, for
the property-name, prop, and the suffixed name, prop!, within that unquote.
]|#

;;; (define-properties . names)
;;; ---------------------------
;;; defines, for each name, property-accessors and -mutators
;;; name and name!
(define-syntax define-properties
  (ir-macro-transformer
    (lambda (f i c?)
      `(begin
         ,@(map (lambda (prop)
                  `(begin
                     (define-syntax ,prop
                       (ir-macro-transformer
                         (lambda (form inject compare?)
                           `(get ,(cadr form) ',',prop))))
                     (define-syntax ,(i (string->symbol
                                          (string-append
                                            (symbol->string (i prop))
                                            "!")))
                       (ir-macro-transformer
                         (lambda (form inject compare?)
                           `(put! ,(cadr form)
                                  ',',prop
                                  ,(caddr form)))))))
                (cdr f))))))

#|[
The following two macros and two procedures represent recursion an lists
and trees respectively. They are, again, inspired by Graham. The
procedures are defined with alambda, the anaphoric version of lambda
with injected symbol self.  These procedures, list-recurser and
tree-recurser,  accept a recurser and a base as arguments, the recurser
being itself procedures accepting the actual list or tree as argument,
as well as one or two thunks representing recursion along the cdr or the
car and the cdr respectively.
The macros, alist-recurser and atree-recurser, are anaphoric versions of
the procedures list-recurser and tree-recurser. They both inject the
symbol it behind the scene, representing the actual list or tree
respectively, as well as symbols go-on or go-left and go-right
respectively representing the recurser arguments of the functions.

The relations between the procedures and the anaphoric macros are shown
in the following exaples:
  (define lcopy
    (list-recurser (lambda (lst th) (cons (car lst) (th))) '()))
  (define alcopy
    (alist-recurser (cons (car it) (go-on)) '()))
  (define tcopy
    (tree-recurser (lambda (tree left right)
                     (cons (left) (or (right) '())))
                   identity))
  (define atcopy
    (atree-recurser (cons (go-left) (or (go-right) '())) it))
]|#

;;; (alist-recurser recurser base)
;;; ------------------------------
;;; wrapping list-recurser into an anaphoric macro with injected symbols it and go-on
;;; where it is the list itself and go-on the recurser-thunk
(define-syntax alist-recurser
  (ir-macro-transformer
    (lambda (form inject compare?)
      (let ((it (inject 'it))
            (go-on (inject 'go-on)))
        `(list-recurser (lambda (,it thunk)
                          (letrec ((,go-on thunk))
                            ,(cadr form)))
                        ,@(cddr form))))))

;;; (atree-recurser recurser base)
;;; ------------------------------
;;; wrapping tree-recurser into an anaphoric macro with injected symbols
;;; it, go-left and go-right representing the actual tree and recursers
;;; along the car and the cdr respectively.
(define-syntax atree-recurser
  (ir-macro-transformer
    (lambda (form inject compare?)
      (let ((recurser (cadr form))
            (base (caddr form))
            (it (inject 'it))
            (go-left (inject 'go-left))
            (go-right (inject 'go-right)))
        `(tree-recurser
           (lambda (,it left right)
             (letrec ((,go-left left)
                      (,go-right right))
               ,recurser))
           (lambda (,it) ,base))))))

;;; (list-recurser recurser base)
;;; -----------------------------
;;; recurser is a procedure of a list and a thunk processing the cdr
(define (list-recurser recurser base)
  (alambda (lst)
    (if (null? lst)
      (if (procedure? base)
        (base)
        base)
      (recurser lst
                (lambda ()
                  (self (cdr lst)))))))

;;; (tree-recurser recurser base)
;;; -----------------------------
;;; recurser is a procedure of a tree and two thunks processing the car
;;; and the cdr
(define (tree-recurser recurser base)
  (alambda (tree)
    (cond
      ((pair? tree)
       (recurser tree
                 (lambda ()
                   (self (car tree)))
                 (lambda ()
                   (if (null? (cdr tree))
                     #f
                     (self (cdr tree))))))
      (else ; atom
        (if (procedure? base)
          (base tree)
          base)))))

;;; documentation dispatcher
(define anaphora
  (let (
    (alist '(
      (aif
        (macro
        (aif test consequent alternative ..)
"anaphoric if where result of test is named it"))
      (nif
        (macro
        (nif name test consequent alternative ..)
"named if where result of test is named name"))
      (awhen
        (macro
         (awhen test xpr . xprs)
"anaphoric when where result of test is named it"))
      (nwhen
        (macro
         (nwhen name test xpr . xprs)
"named when where result of test is named name"))
      (acond
        (macro
        (acond (test xpr . xprs) ... (else xpr . xprs) ..)
"anaphoric cond, where each test except else is named it"))
      (ncond
        (macro
        (ncond name (test xpr . xprs) ... (else xpr . xprs) ..)
"named cond, where each test except else is named name"))
      (awhile
        (macro
        (awhile test xpr . xprs)
"anaphoric while, where each successive test is named it"))
      (nwhile
        (macro
        (nwhile name test xpr . xprs)
"named while, where each successive test is named name"))
      (aand
        (macro
        (aand . args)
"anaporic and, where each arg can refer to the previous arg named it"))
      (nand
        (macro
        (nand name . args)
"named and, where each arg can refer to the previous arg named name"))
      (alambda
        (macro
        (alambda args . body)
"anaphoric lambda, where body can refer to self"))
      (nlambda
        (macro
        (nlambda name args . body)
"named lambda, where body can refer to name"))
      (define-anaphor
        (macro
        (define-anaphor name from rule)
"define an anaphoric macro from a routine with implicit it and rule cascade: or first:"))
      (define-properties
        (macro
        (define-properties name ...)
"abstracting away get and put! Defines properties name and name! ..."))
      (alist-recurser
        (macro
        (alist-recurser recur-xpr base-xpr)
"creates unary procedure from macro-arguments with implicit it and go-on thunk"))
      (atree-recurser
        (macro
        (alist-recurser recur-xpr base-xpr)
"creates unary procedure from macro-arguments with implicit it, go-left and go-right thunks"))
      (list-recurser
        (procedure
        (list-recurser recurser base)
"creates procedure which traverses on cdrs of its only argument"))
      (tree-recurser
        (procedure
        (tree-recurser recurser base)
"creates procedure which traverses on cars and cdrs of its only argument"))
      )))
    (case-lambda
      (() (map car alist))
      ((sym)
       (let ((pair (assq sym alist)))
         (if pair
           (for-each print (cadr pair))
           (print "Choose one of " (map car alist))))))))

) ; module anaphora

