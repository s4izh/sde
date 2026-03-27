#!/usr/bin/env guile
!#
(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (srfi srfi-1))

(define apps
  '("kanshi"
    "waybar"
    "swayosd-server"
    "swaync"
    ; "swaybg -i ~/git/wallpapers/wallhaven-q6p667.jpg"
    ; "swaybg -o DP-3 -i ~/pix/fotos/sara_vertical.jpg -m fill"
    ; "swaybg -o HDM1-A-1 ~/pix/fotos/sara_horizontal.jpg -m fill"
    ; "swaybg -o DP-1 ~/pix/fotos/familia_coche.jpg -m fill"
    "wpaperd"
    "discord"
    "whatsapp-electron"
    ; "kitty --first-terminal"
    ; "swww img ~/git/wallpapers/viktor.jpg"
    "nm-applet --indicator"))

(define (run-bg cmd)
  (let* ((pname (car (string-split cmd #\space))))
    (system (string-append "pkill " pname " > /dev/null 2>&1"))
    (system (string-append cmd " &"))
    (display (string-append "Started: " pname "\n"))))

(define (run-ipc cmd)
    (system (string-append cmd)))

(for-each run-bg apps)

(define (spawn-and-move app-command target-tag target-monitor)
  (system (string-append app-command " &"))
  (sleep 1)
  (display "hello")
  (let ((tag-cmd (string-append "mmsg -d tag,1,0" (number->string target-tag))))
    (system tag-cmd)))

;;; (spawn-and-move "kitty" 3 "DP-3")
