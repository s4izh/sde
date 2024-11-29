;; This is an operating system configuration generated
;; by the graphical installer.
;;
;; Once installation is complete, you can learn and modify
;; this file to tweak the system configuration, and pass it
;; to the 'guix system reconfigure' command to effect your
;; changes.

;; Indicate which modules to import to access the variables
;; used in this configuration.

(use-modules (gnu)
             (gnu home)
             (gnu services)
             (gnu services guix)
             (gnu packages package-management)
             ;; (gnu home services)
             (gnu packages terminals)
             (gnu packages fonts)
             (guix channels)
             (nongnu packages linux)
             (nongnu system linux-initrd)
             (srfi srfi-1) ;remove
             (sergio home config))

(use-service-modules cups desktop networking ssh xorg)

(use-package-modules wm)

(define %channels
  (cons* (channel
           (name 'nonguix)
           (url "https://gitlab.com/nonguix/nonguix")
           ;; Enable signature verification:
           (introduction
            (make-channel-introduction
             "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
             (openpgp-fingerprint
              "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
         %default-channels))

(define home
  home-config)

(define system
  (operating-system
    (kernel linux)
    (initrd microcode-initrd)
    (firmware (list linux-firmware))
    (locale "en_US.utf8")
    (timezone "Europe/Madrid")
    (keyboard-layout (keyboard-layout "us" "altgr-intl"))

    ;; (keyboard-layout (keyboard-layout "es" "caps:escape"))
    (host-name "rx")

    ;; The list of user accounts ('root' is implicit).
    (users (cons* (user-account
                    (name "sergio")
                    ;; (comment "Sergio")
                    (group "users")
                    (home-directory "/home/sergio")
                    (supplementary-groups '("wheel" "netdev" "audio" "video")))
                  %base-user-accounts))

    (packages (append (specifications->packages (list "sway"
                                                      "river"
                                                      "xdg-desktop-portal-wlr"
                                                      "swaybg"
                                                      "swayidle"
                                                      "swaylock"
                                                      "bemenu"
                                                      "vim"
                                                      "nss-certs"
                                                      "git"
                                                      "foot"
                                                      "alacritty"
                                                      "firefox"
                                                      "pavucontrol"
                                                      "make"
                                                      "vim"
                                                      "tmux"
                                                      "git"
                                                      "neovim"
                                                      "gcc-toolchain"
                                                      "setxkbmap"
                                                      "font-iosevka"
                                                      "fzf")) %base-packages))

    ;; Allow resolution of '.local' host names with mDNS
    (name-service-switch %mdns-host-lookup-nss)

    ;; Below is the list of system services.  To search for available
    ;; services, run 'guix system search KEYWORD' in a terminal.
    (services
     (append (list
              ;; To configure OpenSSH, pass an 'openssh-configuration'
              ;; record as a second argument to 'service' below.
              (service openssh-service-type)
              (service guix-home-service-type
                       `(("sergio" ,home-config)))

              (service screen-locker-service-type
                       (screen-locker-configuration (name "swaylock")
                                                    (program (file-append
                                                              swaylock
                                                              "/bin/swaylock"))
                                                    (using-pam? #t)
                                                    (using-setuid? #f)))

              (set-xorg-configuration
               (xorg-configuration (keyboard-layout keyboard-layout))))

             (modify-services (remove (lambda (service)
                                        (eq? (service-kind service)
                                             gdm-service-type))
                                      %desktop-services)
               
               (guix-service-type config =>
                                  (guix-configuration
                                   ;; makes all so slow
                                   ;; (inherit config)
                                   ;; (guix (guix-for-channels %channels))
                                   ;; (channels %channels)
                                   (substitute-urls (append (list
                                                             "https://substitutes.nonguix.org")
                                                     %default-substitute-urls))
                                   (authorized-keys (append (list (local-file
                                                                   "../../substitutes/nonguix.pub"))
                                                     %default-authorized-guix-keys)))))))

    (bootloader (bootloader-configuration
                  (bootloader grub-efi-bootloader)
                  (targets (list "/boot/efi"))
                  (keyboard-layout keyboard-layout)
                  (menu-entries (list (menu-entry (label "Windows 10")
                                                  (chain-loader
                                                   "/EFI/Microsoft/Boot/bootmgfw.efi"))))))

    (swap-devices (list (swap-space
                          (target (uuid "f36e9e6b-6758-4a2e-a879-b79ca1dd40f7")))))

    ;; The list of file systems that get "mounted".  The unique
    ;; file system identifiers there ("UUIDs") can be obtained
    ;; by running 'blkid' in a terminal.
    (file-systems (cons* (file-system
                           (mount-point "/boot/efi")
                           (device (uuid "32DA-A2C7"
                                         'fat32))
                           (type "vfat"))
                         (file-system
                           (mount-point "/")
                           (device (uuid
                                    "91a63a18-7a2b-4868-8d45-3a745e54a00c"
                                    'ext4))
                           (type "ext4")) %base-file-systems))))

;; Return home or system config based on environment variable
(if (getenv "RUNNING_GUIX_HOME") home system)
