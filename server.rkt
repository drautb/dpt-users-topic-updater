#lang racket

(require "spin/main.rkt"
         web-server/servlet
         json)

(define PORT
  (let ([port (environment-variables-ref (current-environment-variables) #"PORT")])
    (if port
        (string->number (bytes->string/utf-8 port))
        5000)))

(define (json-response-maker status headers body)
  (response (if (eq? body #f) 404 status)
            (status->message status)
            (current-seconds)
            #"application/json; charset=utf-8"
            headers
            (lambda (op)
              (when body
                (write-json (force body) op)))))

(define (json-get path handler)
  (define-handler "GET" path handler json-response-maker))

(define (status) (λ ()
                   (log-info "Status requested.")
                   (make-hash (list (cons 'status "healthy")))))

;; Healthcheck
(json-get "/" (status))
(json-get "/status" (status))

(log-info "Starting server on port ~a" PORT)
(run #:port PORT
     #:listen-ip #f)
