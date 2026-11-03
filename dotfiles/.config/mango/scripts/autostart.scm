#!/usr/bin/env guile
!#
(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (srfi srfi-1))

(load (string-append (getenv "HOME") "/.local/scripts/lib/log.scm"))
(define log (make-logger "autostart"))

(define apps
  '("kanshi"
    ; "waybar"
    ; "swayidle -w timeout 300 'swaylock -f' before-sleep 'swaylock -f'"
    "swayosd-server"
    "swaync"
    ; "swaybg -i ~/git/wallpapers/wallhaven-q6p667.jpg"
    ; "swaybg -o DP-3 -i ~/pix/fotos/sara_vertical.jpg -m fill"
    ; "swaybg -o HDM1-A-1 ~/pix/fotos/sara_horizontal.jpg -m fill"
    ; "swaybg -o DP-1 ~/pix/fotos/familia_coche.jpg -m fill"
    "wpaperd"
    "discord"))
    ; "whatsapp-electron"))
    ; "kitty --first-terminal"
    ; "swww img ~/git/wallpapers/viktor.jpg"
    ; "nm-applet --indicator"))

(define (config-file name)
  (string-append (or (getenv "XDG_CONFIG_HOME")
                     (string-append (getenv "HOME") "/.config"))
                 "/sde/" name))

(define (read-app-list file)
  (if (file-exists? file)
      (call-with-input-file file
        (lambda (port)
          (let loop ((apps '()))
            (let ((line (read-line port)))
              (cond
                ((eof-object? line) (reverse apps))
                ((or (string-null? line)
                     (eqv? (string-ref line 0) #\#)) (loop apps))
                (else (loop (cons line apps))))))))
      '()))

(define (apps-to-run)
  (let ((remove (read-app-list (config-file "autostart.remove"))))
    (filter (lambda (app) (not (member app remove)))
            (append apps (read-app-list (config-file "autostart.add"))))))

(define (run-bg cmd)
  (let* ((pname (car (string-split cmd #\space))))
    (system (string-append "pkill " pname " > /dev/null 2>&1"))
    (log (string-append "start " cmd))
    (system (string-append cmd " &"))
    (display (string-append "Started: " pname "\n"))))

(define (run-ipc cmd)
    (system (string-append cmd)))

(log "=== autostart begin ===")

(for-each run-bg (apps-to-run))

(run-ipc "theme-mode restore")

(sleep 1)

(run-bg "waybar")
(run-bg "nm-applet --indicator")
(run-bg "blueman-applet")

(log "=== autostart end ===")

(define (spawn-and-move app-command target-tag target-monitor)
  (system (string-append app-command " &"))
  (sleep 1)
  (display "hello")
  (let ((tag-cmd (string-append "mmsg -d tag,1,0" (number->string target-tag))))
    (system tag-cmd)))

;;; (spawn-and-move "kitty" 3 "DP-3")
