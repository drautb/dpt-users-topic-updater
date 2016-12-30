#lang racket

(require "spin/main.rkt"
         web-server/servlet
         net/http-client
         net/uri-codec
         json)

(define (load-env-var name)
  (bytes->string/utf-8 (environment-variables-ref (current-environment-variables) name)))

;; CONSTANTS
(define PORT
  (let ([port (environment-variables-ref (current-environment-variables) #"PORT")])
    (if port
        (string->number (bytes->string/utf-8 port))
        5000)))

(define XMATTERS_HOST "familysearch.hosted.xmatters.com")
(define XMATTERS_TOKEN (load-env-var #"XMATTERS_TOKEN"))

(define TOPIC-TEMPLATE
  (string-append "@xMatters: ~a | ~a | ~a~n"
                 "Task/Bug? http://almtools.ldschurch.org/fhjira/projects/DPT~n"
                 "Get help while helping others? http://almtools.ldschurch.org/fhconfluence/questions"))

(define PROVISIONING_GROUP "Platform Provisioning")
(define VPC_GROUP "Platform VPC")
(define CDSVCS_GROUP "Continuous Delivery Services")

;; G07KHQH1Q = #drautb-test
;; C07KDNCN7 = #dpt-users
(define SLACK_CHANNEL_ID (load-env-var #"SLACK_CHANNEL_ID"))
(define SLACK_TOKEN (load-env-var #"SLACK_TOKEN"))
(define SLACK_HOST "slack.com")

(log-all-levels (current-logger))

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

(define (json-post path handler)
  (define-handler "POST" path handler json-response-maker))

(define (status) (λ ()
                   (log-info "Status requested.")
                   (make-hash (list (cons 'status "healthy")))))


(define (update-topic) (λ ()
                         (log-info "Updating topic.")
                         (define new-topic (generate-new-topic))
                         (set-channel-topic new-topic)
                         (make-hash (list (cons 'newTopic new-topic)))))

(define (get-group-calendar group-name)
  (define (generate-uri group-name)
    (format "/api/xm/1/groups/~a/calendar" (uri-encode group-name)))
  (define (generate-headers)
    (list (format "Authorization: Basic ~a" XMATTERS_TOKEN)))
  (let-values ([(status-code header in-port)
                (http-sendrecv XMATTERS_HOST
                               (generate-uri group-name)
                               #:ssl? #t
                               #:headers (generate-headers))])
    (log-info "group-name=~a status-code=~a" group-name status-code)
    (read-json in-port)))

(define (extract-primary-from-calendar calendar)
  ; (log-info (format "Calendar: ~a" (jsexpr->string calendar)))
  (define primary
    (hash-ref (first (hash-ref (hash-ref (first (hash-ref calendar
                                                          'data))
                                         'members)
                               'data))
              'member))
  (format "~a ~a"
          (hash-ref primary 'firstName)
          (hash-ref primary 'lastName)))

(define (primary-on-call group-name)
  (extract-primary-from-calendar (get-group-calendar group-name)))

(define (generate-new-topic)
  (format TOPIC-TEMPLATE
          (primary-on-call PROVISIONING_GROUP)
          (primary-on-call VPC_GROUP)
          (primary-on-call CDSVCS_GROUP)))

(define (set-channel-topic topic-str)
  (define (generate-uri)
    (define type (substring SLACK_CHANNEL_ID 0 1))
    (if (eq? type "C")
        "/api/channels.setTopic"
        "/api/groups.setTopic"))
  (define (generate-headers)
    (list (format "Authorization: Basic ~a" XMATTERS_TOKEN)))
  (let-values ([(status-code header in-port)
                (http-sendrecv SLACK_HOST
                               (generate-uri)
                               #:ssl? #t
                               #:method "POST"
                               #:headers (list "Content-Type: application/x-www-form-urlencoded")
                               #:data (alist->form-urlencoded
                                        (list (cons 'token SLACK_TOKEN)
                                              (cons 'channel SLACK_CHANNEL_ID)
                                              (cons 'topic topic-str))))])
    (log-info "response from slack api call. status-code=~a response-body=~a"
              status-code (port->string in-port))))


;; Healthcheck
(json-get "/" (status))
(json-get "/status" (status))

;; Update Topic
(json-post "/update-topic" (update-topic))

(log-info "Starting server on port ~a" PORT)
(run #:port PORT
     #:listen-ip #f)
