(define-module (sergio packages tree-sitter)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module (gnu packages pkg-config)
  #:use-module ((guix licenses) #:prefix license:))

(define-public tree-sitter
  (package
    (name "tree-sitter")
    (version "0.24.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tree-sitter/tree-sitter")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256 (base32 "0swsxb495737xlbq0fmvrmd1h7xl70a8shz0jm9s3zwicj12wsmd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "CC=" ,(cc-for-target))
                          (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'bootstrap)
         (delete 'check)
         (replace 'build (lambda* _ (invoke "env")))
         (delete 'configure))))
    
    (synopsis "Incremental parsing system for programming tools")
    (description "Tree-sitter is a parser generator tool and an incremental parsing library. It can build a concrete syntax tree for a source file and efficiently update the syntax tree as the source file is edited.")
    (home-page "https://tree-sitter.github.io/")
    (license license:expat)))
