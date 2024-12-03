(define-module (sergio packages dwm)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages suckless))

(define-public dwm-custom
  (package
   (inherit dwm)
   (name "dwm-custom")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/s4izh/dwm")
           (commit "03662c39cf1cf62d6bf234b771102b36f72cf278")))
     (sha256
      (base32 "0hyyxyh712r58s7zb71rzxldibz1hdzlvxw4lwkbbwrbn6k0fbih"))))
   (propagated-inputs
    (list dmenu))
          ; maim
          ; xclip
          ; xprop
          ; xsetroot
          ; font-awesome
          ; font-jetbrains-mono
          ; picom
          ; xwallpaper))
   (home-page "https://github.com/s4izh/dwm")
   (synopsis "My own version of dwm")))

dwm-custom
