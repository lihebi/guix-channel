(define-module (vnc)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gettext)
  #:use-module (guix gexp)
  #:use-module (gnu packages image)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (ice-9 match))


(define-public tigervnc
  (package
   (name "tigervnc")
   (version "1.9.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://github.com/TigerVNC/tigervnc/archive/v1.9.0.tar.gz")
     (sha256
      (base32
       "0h7p86262jmnx81p271n328p50zdb0pgawgj7dn3ampc022ysp7i"))
     ;; a gexp that 1. download the xorg-server source code to a
     ;; subfolder 2. apply patch based on version
     ;; (snippet (let ((origin-uri (package-source xorg-server)))
     ;;            #~()))
     (file-name "tigervnc-1.9.0-checkout")))
   (inputs `(("cmake" ,cmake)
             ("zlib" ,zlib)
             ("fltk" ,fltk)
             ("libjpeg-turbo" ,libjpeg-turbo)
             ("gettext" ,gnu-gettext)
             ("libxrandr" ,libxrandr)
             ("libxdamage" ,libxdamage)))
   (propagated-inputs `(("xauth" ,xauth)))
   (build-system cmake-build-system)
   (arguments
    `(#:tests? #f))
   (home-page "https://tigervnc.org")
   (synopsis "TODO")
   (description "TODO")
   (license license:gpl2)))
