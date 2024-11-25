(define-module (sergio utils global)
  #:export (dotfiles)
  #:export (dotfiles-dir))

;; relative to the guix directory in this repo
(define dotfiles-dir (canonicalize-path "../dotfiles"))

(define (dotfile name)
  (string-append dotfiles-dir "/" name))
