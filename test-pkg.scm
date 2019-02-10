(define-module (test-pkg)
  #:use-module (gnu packages)
  #:use-module (gnu packages lisp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages m4)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system asdf)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libffcall)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages libsigsegv)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages ed)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xorg)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))


(define-public corenlp
  ;; given up, because the default build.xml does not have a install
  ;; target. And I don't know how to pack a jar file for it.
  (package
    (name "corenlp")
    (version "3.9.2")
    ;; (source
    ;;  (origin
    ;;    (method url-fetch)
    ;;    (uri (string-append "http://central.maven.org/maven2/"
    ;;                        "edu/stanford/nlp/stanford-corenlp/" version
    ;;                        "/stanford-corenlp-" version "-sources.jar"))
    ;;    (sha256
    ;;     (base32
    ;;      "0lwidpid45f60gbc30j8fxyqz203wkgl5f04ykfn0rnwhphi9fqg"))))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lihebi/CoreNLP")
             ;; (commit "a5766c35a9d0629485e11dce9aeeb22692411a6e")
             ;; the corresponding hash for this commit:
             ;; "1ffkg5179dc4xrpwy4jc2647dxcbqqnj66r47vj2sz7lmij5yp7w"
             ;; I'm getting a random commit, since it does not have any tag
             (commit "v3.9.2")))
       (sha256
        (base32
         "07mp6i74zc6hnsg2b7885pfsdh4fgs8dypnzsz1180wyykjng4kd"))
       (file-name (string-append "corenlp-" version "-checkout"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       ;; If supply the jar-name, the default build.xml will not be used
       ;; #:jar-name "stanford-corenlp.jar"
       ))
    (home-page "https://stanfordnlp.github.io/CoreNLP")
    (synopsis "Lalala")
    (description "Lalala")
    (license license:gpl3)))

