(require srfi/9)

; basic units
(define (inverter input output)
  (define (invert-input)
    (let [(new-value (if (= (get-signal input) 0) 1 0))]
     (after-delay inverter-delay
                  (lambda ()
                    (set-signal! output new-value)))))
  (add-action! input invert-input))

(define (and-gate a1 a2 output)
  (define (logical-and a1 a2)
    (if (and (= 1 (get-signal a1)) (= 1 (get-signal a2))) 1 0))
  (define (and-inputs)
    (let [(new-value (logical-and a1 a2))]
     (after-delay and-gate-delay
                  (lambda ()
                    (set-signal! output new-value)))))
  (add-action! a1 and-inputs)
  (add-action! a2 and-inputs))

(define (or-gate a1 a2 output)
  (define (logical-or a1 a2)
    (if (or (= 1 (get-signal a1)) (= 1 (get-signal a2))) 1 0))
  (define (or-inputs)
    (let [(new-value (logical-or a1 a2))]
     (after-delay or-gate-delay
                  (lambda ()
                    (set-signal! output new-value)))))
  (add-action! a1 or-inputs)
  (add-action! a2 or-inputs))


(define (half-adder a b s c)
  (define d (make-wire))
  (define e (make-wire))
  (or-gate a b d)
  (and-gate a b c)
  (inverter c e)
  (and-gate d e s))

(define (full-adder a b c-in sum c-out)
  (define s (make-wire))
  (define c1 (make-wire))
  (define c2 (make-wire))
  (half-adder b c-in s c1)
  (half-adder a s sum c2)
  (or-gate c2 c1 c-out))

(define (ripple-carry-adder a-list b-list s-list carry)
  (let loop [(a a-list)
             (b b-list)
             (s s-list)
             (last-carry carry)]
    (cond [(and (null? a) (null? b) (null? s))
           (set-signal! last-carry 0)]
          [else (let [(new-carry (make-wire))]
                  (full-adder (car a) (car b) new-carry (car s) last-carry)
                  (loop (cdr a) (cdr b) (cdr s) new-carry))])))

; wire implementation
(define (make-wire)
  (let [(signal-value 0) (action-procedures '())]
   (define (set-my-value v)
     (if (not (= v signal-value v))
       (begin (set! signal-value v)
              (call-each action-procedures))
       'done))

   (define (accept-action-procedure! proc) 
     (set! action-procedures 
       (cons proc action-procedures))
     (proc))

   (lambda (m)
     (cond [(eq? m 'get-signal) signal-value]
           [(eq? m 'set-signal!) set-my-value]
           [(eq? m 'add-action!) accept-action-procedure!]
           [(eq? m 'get-actions) action-procedures]
           [else (error "unknown message for wire")]))))

(define (set-signal! wire v)
  ((wire 'set-signal!) v))

(define (get-signal wire)
  (wire 'get-signal))

(define (add-action! wire act)
  ((wire 'add-action!) act))

(define (call-each procedures)
  (if (null? procedures)
    'done
    (begin ((car procedures))
           (call-each (cdr procedures)))))

; agenda implementation with skewed heap (faster and more elegant)

; srfi-9 feature
(define-record-type 
  :entry 
  (make-agenda-entry key proc left right)
  is-entry?
  (key entry-key set-entry-key!)
  (proc entry-proc set-entry-proc!)
  (left entry-left set-entry-left!)
  (right entry-right set-entry-right!))

(define (less key1 key2)
  (cond [(or (null? key2) (null? key1)) #f]
        [(< (car key1) (car key2)) #t]
        [(> (car key1) (car key2)) #f]
        [else (less (cdr key1) (cdr key2))]))

(define (merge-entries! e1 e2)
  (cond [(null? e1) e2]
        [(null? e2) e1 ]
        [(less (entry-key e2) (entry-key e1))
         (merge-entries! e2 e1)]
        [else (set-entry-left! e1 (merge-entries! (entry-left e1) e2))
         (let [(tmp (entry-left e1))]
           (set-entry-left! e1 (entry-right e1))
           (set-entry-right! e1 tmp)
           e1)]))

; message-passing style agenda
(define (make-agenda)
  (let [(current-time 0)
        (order 0)
        (entries '())]

    (define (access-order)
      (set! order (+ order 1)) 
      order)

    (define (add-proc! time action)
      (define new-entry (make-agenda-entry (list time (access-order)) action '() '()))
      (set! entries (merge-entries! new-entry entries)))

    (define (delete-first!)
      (set! entries (merge-entries! (entry-left entries) (entry-right entries))))

    (lambda (m) 
      (cond [(eq? m 'time) current-time]
            [(eq? m 'entries) 
             (if (not (null? entries))
               (set! current-time (car (entry-key entries)))
               'done)
             entries]
            [(eq? m 'add-proc!) add-proc!]
            [(eq? m 'delete-first!) delete-first!]
            [else (error "unknown message for agenda" m)]))))

; interfaces for compatibility
(define (empty-agenda? agenda)
  (null? (agenda 'entries)))

(define (current-time agenda)
  (agenda 'time))

(define (add-to-agenda! time action agenda)
  ((agenda 'add-proc!) time action))

(define (first-agenda-item agenda)
  (entry-proc (agenda 'entries)))

(define (remove-first-agenda-item! agenda)
  ((agenda 'delete-first!)))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
    'done
    (let [(first-item (first-agenda-item the-agenda))]
     (first-item)
     (remove-first-agenda-item! the-agenda)
     (propagate))))

(define (probe name wire)
  (add-action! wire 
               (lambda () 
                 (printf "~A ~A New-value = ~A~N"
                         name (current-time the-agenda) (get-signal wire)))))


; test part

;half adder
(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))
(define carry-in (make-wire))

(probe 'sum sum)
(probe 'carray carry)

(full-adder input-1 input-2 carry-in sum carry)

(set-signal! input-1 1)
(set-signal! input-2 1)
(set-signal! carry-in 1)
(propagate)


; ripple adder

; make a n-bits integer
(define (make-n-wires n)
  (if (= n 0) 
    '() 
    (cons (make-wire) (make-n-wires (- n 1)))))

(define (set-nth-wire! wires n val)
  (if (= n 1)
    (set-signal! (car wires) val)
    (set-nth-wire! (cdr wires) (- n 1) val)))

(define (set-wires! wires vals)
  (map (lambda (w v) (set-signal! w v)) wires vals))

(define (map-with-index op l)
  (let loop [(ind 1)
             (rest l)]
    (if (null? rest)
      '()
      (cons (op (car rest) ind)
            (loop (+ ind 1) (cdr rest))))))


(define int1 (make-n-wires 8))
(define int2 (make-n-wires 8))
(define int3 (make-n-wires 8))
(define carry (make-wire))

(ripple-carry-adder int1 int2 int3 carry)

(set-wires! int1 '(1 0 0 1 1 0 1 0))
(set-wires! int2 '(0 0 1 1 0 0 1 0))

; probe for all wires in output
(map-with-index (lambda (w ind) 
                  (probe (string-append "sum" (number->string ind)) w))
                int3)

(probe "carry" carry)
(propagate)

(printf "add result: ~A~N" (map (lambda (w) (get-signal w)) int3))


; more test: ripple-substractor
(define (ripple-inverter input-wires output-wires)
  (map (lambda (i o) (inverter i o)) input-wires output-wires))

(define (ripple-subtractor input1 input2 output carray)
  (define n (length input1))
  (define tmp (make-n-wires n))
  (define neg2 (make-n-wires n))
  (define one (make-n-wires n))
  (define not-used (make-wire))
  (set-nth-wire! one n 1)

  (ripple-inverter input2 tmp)
  (ripple-carry-adder tmp one neg2 not-used)
  (ripple-carry-adder neg2 input1 output carry))

(define int4 (make-n-wires 8))
(ripple-subtractor int1 int2 int4 carry)

(map-with-index (lambda (w ind) 
                  (probe (string-append "sub" (number->string ind)) w))
                int3)
(propagate)

(printf "sub result: ~A~N" (map (lambda (w) (get-signal w)) int4))
