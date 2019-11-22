(define-module (nongnu services kernel-modules)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (nongnu packages linux)
  #:export (nvidia-insmod-service-type))


(define (nvidia-insmod-shepherd-service config)
  (list (shepherd-service
         (provision '(nvidia-insmod))
         (requirement '())
         ;; run the nvidia-insmod script
         (start #~(lambda _
                    (and
                     (zero? (system* (string-append #$nvidia-driver "/bin/nvidia-insmod"))))))
         (one-shot? #t)
         (auto-start? #t)
         (respawn? #f))))

(define nvidia-insmod-service-type
  (service-type
   (name 'nvidia-insmod-name)
   (extensions
    (list (service-extension shepherd-root-service-type nvidia-insmod-shepherd-service)))
   (default-value '())))


;; testing shepherd service
(define (hello-shepherd-service config)
  (list (shepherd-service
         (provision '(hello))
         (requirement '())
         (start #~(lambda _
                    (and
                     (zero? (system* "touch" "/tmp/hello"))
                     (zero? (system* "touch" "/home/hebi/tmp/hello"))
                     (zero? (system* "touch" "/var/hello")))))
         (respawn? #f))))

(define hello-service-type
  (service-type
   (name 'hello)
   (extensions
    (list (service-extension shepherd-root-service-type hello-shepherd-service)))
   (default-value '())))
