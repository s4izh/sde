(define-module (sergio packages neovim)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages vim)
  #:use-module (sergio packages tree-sitter))

(define-public neovim-latest
  (package
    (inherit neovim)
    (name "neovim-latest")
    (version "0.10.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/neovim/neovim")
                    (commit (string-append "v" version))))
              (file-name(git-file-name name version))
              (sha256
               (base32
                "0r5mjfsgrllxi44i9k6lb8b99rpzrwhkg18aiqmby8wwzflbqdy3"))))
    (inputs (modify-inputs (package-inputs neovim)
                (replace "tree-sitter" tree-sitter)))))

neovim-latest
