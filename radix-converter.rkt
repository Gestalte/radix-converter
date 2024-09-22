#lang racket

(require racket/gui)

; UI

(define frame (new frame%
                   [label "Radix Converter"]
                   [width 300]
                   [height 50]
                   [stretchable-width #t]	 
                   [stretchable-height #f]))

(define text-field-decimal (new text-field%
                                (label "Decimal")
                                (parent frame)
                                (style (list 'single 'vertical-label))
                                (callback (lambda (t e) (decimal-changed t e)))))

(define text-field-octal (new text-field%
                              (label "Octal")
                              (parent frame)
                              (style (list 'single 'vertical-label))
                              (callback (lambda (t e) (octal-changed t e)))))

(define text-field-hex (new text-field%
                            (label "Hex")
                            (parent frame)
                            (style (list 'single 'vertical-label))
                            (callback (lambda (t e) (hex-changed t e)))))

(define text-field-binary (new text-field%
                               (label "Binary")
                               (parent frame)
                               (style (list 'single 'vertical-label))
                               (callback (lambda (t e) (binary-changed t e)))))

(send frame show #t)

; Non-UI

; lst should be the input number as a list in reverse.
(define convert-radix-number-to-decimal (lambda (lst counter radix)
                                          (define calc (lambda (item count)
                                                         (cond
                                                           [(eq? item #\A) (* 10 (expt radix count))]
                                                           [(eq? item #\B) (* 11 (expt radix count))]
                                                           [(eq? item #\C) (* 12 (expt radix count))]
                                                           [(eq? item #\D) (* 13 (expt radix count))]
                                                           [(eq? item #\E) (* 14 (expt radix count))]
                                                           [(eq? item #\F) (* 15 (expt radix count))]
                                                           [else (* (- (char->integer item) 48) (expt radix count))])))
                                                       
                                          (if (= (length lst) 1)
                                              (calc (car lst) counter)
                                              (+ (calc (car lst) counter) (convert-radix-number-to-decimal (cdr lst) (+ counter 1) radix)))))

(define decimal-changed (lambda (text-field event)
                          (define input (string->number (send text-field get-value)))
                          (define octal (~r input #:base 8))
                          (define hex (~r input #:base '(up 16)))
                          (define binary (~r input #:base 2))
                          (send text-field-octal set-value octal)
                          (send text-field-hex set-value hex)
                          (send text-field-binary set-value binary)
                          (print input)))

(define octal-changed (lambda (text-field event)
                        (define input (send text-field get-value))
                        (define input-list (reverse (string->list input)))
                        (define input-as-decimal (convert-radix-number-to-decimal input-list 0 8))

                        (define hex (~r input-as-decimal #:base '(up 16)))
                        (define binary (~r input-as-decimal #:base 2))

                        (send text-field-decimal set-value (number->string input-as-decimal))
                        (send text-field-binary set-value binary)                      
                        (send text-field-hex set-value hex)
                         
                        (print input)))

(define hex-changed (lambda (text-field event)
                      (define input (send text-field get-value))
                      (define input-list (reverse (string->list input)))
                      (define input-as-decimal (convert-radix-number-to-decimal input-list 0 16))
                      
                      (define octal (~r input-as-decimal #:base 8))
                      (define binary (~r input-as-decimal #:base 2))

                      (send text-field-decimal set-value (number->string input-as-decimal))
                      (send text-field-octal set-value octal)                      
                      (send text-field-binary set-value binary)
                      
                      (print input)))

(define binary-changed (lambda (text-field event)
                         (define input (send text-field get-value))
                         (define input-list (reverse (string->list input)))
                         (define input-as-decimal (convert-radix-number-to-decimal input-list 0 2))

                         (define octal (~r input-as-decimal #:base 8))
                         (define hex (~r input-as-decimal #:base '(up 16)))

                         (send text-field-decimal set-value (number->string input-as-decimal))
                         (send text-field-octal set-value octal)                      
                         (send text-field-hex set-value hex)
                         
                         (print input)))

