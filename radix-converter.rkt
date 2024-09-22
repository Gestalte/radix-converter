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
                        (print  (send text-field get-value))))

(define hex-changed (lambda (text-field event)
                      (print  (send text-field get-value))))

(define binary-changed (lambda (text-field event)
                         (print  (send text-field get-value))))