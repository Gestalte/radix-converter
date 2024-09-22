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
                          (define show-error (lambda ()
                                               (send text-field-octal set-value "Error")
                                               (send text-field-hex set-value "Error")
                                               (send text-field-binary set-value "Error")))

                          (define set-values (lambda (num)
                                               (define decimal-input (string->number num))
                                               (define octal (~r decimal-input #:base 8))
                                               (define hex (~r decimal-input #:base '(up 16)))
                                               (define binary (~r decimal-input #:base 2))
                                               (send text-field-octal set-value octal)
                                               (send text-field-hex set-value hex)
                                               (send text-field-binary set-value binary)))
                          
                          (define input (send text-field get-value))
                          
                          (if (regexp-match "[^0-9]" input)
                              (show-error)
                              (when (> (string-length input) 0)
                                (set-values input)))))
                              
(define octal-changed (lambda (text-field event)
                        (define show-error (lambda ()
                                             (send text-field-decimal set-value "Error")
                                             (send text-field-hex set-value "Error")
                                             (send text-field-binary set-value "Error")))

                        (define set-values (lambda (num)
                                             (define input-list (reverse (string->list num)))
                                             (define input-as-decimal (convert-radix-number-to-decimal input-list 0 8))
                                             (send text-field-decimal set-value (number->string input-as-decimal))
                                             (define hex (~r input-as-decimal #:base '(up 16)))
                                             (define binary (~r input-as-decimal #:base 2))
                                             (send text-field-binary set-value binary)                      
                                             (send text-field-hex set-value hex)))
                        
                        (define input (send text-field get-value))
                          
                        (if (regexp-match "[^0-8]" input)
                            (show-error)
                            (when (> (string-length input) 0)
                              (set-values input)))))

(define hex-changed (lambda (text-field event)
                      (define show-error (lambda ()
                                           (send text-field-decimal set-value "Error")
                                           (send text-field-octal set-value "Error")
                                           (send text-field-binary set-value "Error")))

                      (define set-values (lambda (num)
                                           (define input-list (reverse (string->list num)))
                                           (define input-as-decimal (convert-radix-number-to-decimal input-list 0 16))
                                           (define octal (~r input-as-decimal #:base 8))
                                           (define binary (~r input-as-decimal #:base 2))
                                           (send text-field-decimal set-value (number->string input-as-decimal))
                                           (send text-field-octal set-value octal)
                                           (send text-field-hex set-value (string-upcase num))
                                           (send text-field-binary set-value binary)))

                      (define input (send text-field get-value))
                      
                      (if (regexp-match "[^0-9A-F]" (string-upcase input))
                          (show-error)
                          (when (> (string-length input) 0)
                            (set-values input)))))

(define binary-changed (lambda (text-field event)
                         ; TODO: If input contains [^0-1] show error
                         (define input (send text-field get-value))
                         (define input-list (reverse (string->list input)))
                         (define input-as-decimal (convert-radix-number-to-decimal input-list 0 2))

                         (define octal (~r input-as-decimal #:base 8))
                         (define hex (~r input-as-decimal #:base '(up 16)))

                         (send text-field-decimal set-value (number->string input-as-decimal))
                         (send text-field-octal set-value octal)                      
                         (send text-field-hex set-value hex)
                         
                         (print input)))

