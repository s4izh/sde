#!/usr/bin/env guile
!#
;; Shared logging helper for SDE scripts.
;;
;;   (load (string-append (getenv "HOME") "/.local/scripts/lib/log.scm"))
;;   (define log (make-logger "autostart"))
;;   (log "started swaync")
;;
;; Writes timestamped lines to <dir>/<name>.log. If <dir> is omitted, uses
;; $SDE_LOG_DIR or $XDG_RUNTIME_DIR/sde (fallback: /tmp/sde-<uid>).
;; Set SDE_LOG_STDERR=1 to also write to stderr.

(use-modules (srfi srfi-19))

(define (log-directory)
  (or (getenv "SDE_LOG_DIR")
      (let ((runtime (getenv "XDG_RUNTIME_DIR")))
        (if runtime
            (string-append runtime "/sde")
            (string-append "/tmp/sde-" (number->string (getuid)))))))

(define (timestamp)
  (date->string (current-date) "~Y-~m-~d ~H:~M:~S"))

(define (make-logger name . dirs)
  (let* ((dir (if (null? dirs) (log-directory) (car dirs)))
         (file (string-append dir "/" name ".log")))
    (system (string-append "mkdir -p '" dir "'"))
    (lambda (msg)
      (let ((line (string-append (timestamp) " " msg)))
        (when (getenv "SDE_LOG_STDERR")
          (display line (current-error-port))
          (newline (current-error-port)))
        ;; Never let logging break the caller.
        (catch #t
          (lambda ()
            (let ((port (open-file file "a")))
              (display line port)
              (newline port)
              (close-port port)))
          (lambda args #f))))))
