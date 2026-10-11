#!/usr/bin/env guile
!#
(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 match))

;; Configuration: Map codes to Icons/Names
(define (map-layout code)
  (match code
    ("S"  "󰕰 Scroller")
    ("T"  "󰕰 Tile")
    ("G"  "󰕰 Grid")
    ("M"  "󰕰 Monocle")
    ("K"  "󰕰 Deck")
    ("VS" "󰕰 V-Scroller")
    ("VT" "󰕰 V-Tile")
    ("CT" "󰕰 Center Tile")
    (_    (string-append "󰕰 " code))))

(define (get-current-layout monitor)
  (let* ((port (open-input-pipe (string-append "mmsg -g -l -o " monitor)))
         (output (read-line port)))
    (close-pipe port)
    (if (eof-object? output) "Unknown" output)))

(define (main args)
  (let ((monitor (if (> (length args) 1) (list-ref args 1) "eDP-1")))
    ;; 1. Print initial state
    (display (map-layout (get-current-layout monitor)))
    (newline)
    (force-output)

    ;; 2. Watch for changes
    (let ((port (open-input-pipe "mmsg -w -l")))
      (let loop ((line (read-line port)))
        (unless (eof-object? line)
          ;; MangoWC -w -l output format: "MONITOR CODE" (e.g., "eDP-1 S")
          (match (string-tokenize line)
            ((m code)
             (when (string=? m monitor)
               (display (map-layout code))
               (newline)
               (force-output))))
          (loop (read-line port)))))))

(main (command-line))
