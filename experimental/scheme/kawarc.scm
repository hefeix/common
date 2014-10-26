(define (simple-prompter port)
  (let ((state (input-port-read-state port)))
    (cond ((char=? state #\Newline) "")
          ((char=? state #\Space) "> ")
          ((char=? state #\() "")
          (else ""))))

(set-input-port-prompter! (current-input-port) simple-prompter)
