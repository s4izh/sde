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
             (gnu packages terminals)
             (gnu packages fonts)
             ; (home sergio)
             (nongnu packages linux)
             (nongnu system linux-initrd))

(use-service-modules cups desktop networking ssh xorg)

; (define home-config (load "../../home/config.scm"))

(operating-system
  (kernel linux)
  (initrd microcode-initrd)
  (firmware (list linux-firmware))
  (locale "en_US.utf8")
  (timezone "Europe/Madrid")
  (keyboard-layout (keyboard-layout "us" "altgr-intl"))
  ; (keyboard-layout (keyboard-layout "es" "caps:escape"))
  (host-name "rx")

  ;; The list of user accounts ('root' is implicit).
  (users (cons* (user-account
                  (name "sergio")
                  (comment "Sergio")
                  (group "users")
                  (home-directory "/home/sergio")
                  (supplementary-groups '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))

   ; (fontconfig
   ;   (config
   ;     (list
   ;       (make-fontconfig-alias
   ;         'monospace "Iosevka"))))

  ;; Packages installed system-wide.  Users can also install packages
  ;; under their own account: use 'guix search KEYWORD' to search
  ;; for packages and 'guix install PACKAGE' to install a package.
  (packages (append (specifications->packages (list "i3-wm"
                                                    "i3status"
                                                    "dmenu"
                                                    "st"
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
                                                    "fzf"))
                    %base-packages))

  ;; Below is the list of system services.  To search for available
  ;; services, run 'guix system search KEYWORD' in a terminal.
  (services
    (append
      (list
        ;; To configure OpenSSH, pass an 'openssh-configuration'
        ;; record as a second argument to 'service' below.
        (service openssh-service-type)

        ; (service fontconfig-service-type
        ;      (fontconfig-configuration
        ;       (font-aliases
        ;        (list
        ;         ;; Alias for monospace to prefer Iosevka
        ;         (alias
        ;          (family "monospace")
        ;          (prefer (family "Iosevka")))))))

        ; (service home-service-type home-config)

        (set-xorg-configuration
          (xorg-configuration (keyboard-layout keyboard-layout))))

   (modify-services %desktop-services
    (guix-service-type config =>
     (guix-configuration
      (substitute-urls
       (append (list "https://substitutes.nonguix.org")
        %default-substitute-urls))
      (authorized-keys
       (append (list (local-file "../../substitutes/nonguix.pub"))
        %default-authorized-guix-keys)))))))

  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout keyboard-layout)))
  (swap-devices (list (swap-space
                        (target (uuid
                                 "f36e9e6b-6758-4a2e-a879-b79ca1dd40f7")))))

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
                         (type "ext4")) %base-file-systems)))
