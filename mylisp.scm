(define-module (gnu packages hebi)
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

;; (define-public cl-aa
;;   (package "http://tuxee.net/cl-aa.lisp"))

(define-public sbcl-cl-aa
  (package
   (name "sbcl-cl-aa")
   (version "1.0.3")
   (source
    ;; github link: https://github.com/fjolliton/cl-vectors. But it
    ;; does not have the version number. Maybe use a commit.
    (origin
     (method url-fetch)
     (uri "http://projects.tuxee.net/cl-vectors/files/cl-vectors-0.1.5.tar.gz")
     (sha256
      (base32
       "04lhwi0kq8pkwhgd885pk80m1cp9sfvjjn5zj70s1dnckibhdmqh"))
     (file-name (string-append "cl-vectors-" version "-checkout"))))
   (build-system asdf-build-system/sbcl)
   (arguments '(#:asd-file "cl-aa.asd"))
   (home-page "https://www.xach.com/lisp/zpb-ttf/")
   (synopsis "ZPB-TTF is a TrueType file parser.")
   (description "TrueType fonts have a publicly documented file
format. ZPB-TTF is a TrueType file parser that provides an interface
for reading typographic metrics, glyph outlines, and other information
from the file.")
   (license license:bsd-style)))


(define-public sbcl-cl-paths
  (package
   (name "sbcl-cl-paths")
   (version "1.0.3")
   (source
    ;; github link: https://github.com/fjolliton/cl-vectors. But it
    ;; does not have the version number. Maybe use a commit.
    (origin
     (method url-fetch)
     (uri "http://projects.tuxee.net/cl-vectors/files/cl-vectors-0.1.5.tar.gz")
     (sha256
      (base32
       "04lhwi0kq8pkwhgd885pk80m1cp9sfvjjn5zj70s1dnckibhdmqh"))
     (file-name (string-append "cl-vectors-" version "-checkout"))))
   (build-system asdf-build-system/sbcl)
   (arguments '(#:asd-file "cl-paths.asd"))
   (home-page "https://www.xach.com/lisp/zpb-ttf/")
   (synopsis "ZPB-TTF is a TrueType file parser.")
   (description "TrueType fonts have a publicly documented file
format. ZPB-TTF is a TrueType file parser that provides an interface
for reading typographic metrics, glyph outlines, and other information
from the file.")
   (license license:bsd-style)))

(define-public sbcl-cl-paths-ttf
  (package
   (name "sbcl-cl-paths-ttf")
   (version "1.0.3")
   (source
    ;; github link: https://github.com/fjolliton/cl-vectors. But it
    ;; does not have the version number. Maybe use a commit.
    (origin
     (method url-fetch)
     (uri "http://projects.tuxee.net/cl-vectors/files/cl-vectors-0.1.5.tar.gz")
     (sha256
      (base32
       "04lhwi0kq8pkwhgd885pk80m1cp9sfvjjn5zj70s1dnckibhdmqh"))
     (file-name (string-append "cl-vectors-" version "-checkout"))))
   (build-system asdf-build-system/sbcl)
   (arguments '(#:asd-file "cl-paths-ttf.asd"))
   (inputs `(("sbcl-cl-paths" ,sbcl-cl-paths)
             ("sbcl-zpb-ttf" ,sbcl-zpb-ttf)))
   (home-page "https://www.xach.com/lisp/zpb-ttf/")
   (synopsis "ZPB-TTF is a TrueType file parser.")
   (description "TrueType fonts have a publicly documented file
format. ZPB-TTF is a TrueType file parser that provides an interface
for reading typographic metrics, glyph outlines, and other information
from the file.")
   (license license:bsd-style)))

(define-public sbcl-cl-vectors
  (package
   (name "sbcl-cl-vectors")
   (version "1.0.3")
   (source
    ;; github link: https://github.com/fjolliton/cl-vectors. But it
    ;; does not have the version number. Maybe use a commit.
    (origin
     (method url-fetch)
     (uri "http://projects.tuxee.net/cl-vectors/files/cl-vectors-0.1.5.tar.gz")
     (sha256
      (base32
       "04lhwi0kq8pkwhgd885pk80m1cp9sfvjjn5zj70s1dnckibhdmqh"))
     (file-name (string-append "cl-vectors-" version "-checkout"))))
   (build-system asdf-build-system/sbcl)
   (inputs `(("sbcl-cl-aa" ,sbcl-cl-aa)
             ("sbcl-cl-paths" ,sbcl-cl-paths)))
   (home-page "https://www.xach.com/lisp/zpb-ttf/")
   (synopsis "ZPB-TTF is a TrueType file parser.")
   (description "TrueType fonts have a publicly documented file
format. ZPB-TTF is a TrueType file parser that provides an interface
for reading typographic metrics, glyph outlines, and other information
from the file.")
   (license license:bsd-style)))

(define-public sbcl-zpb-ttf
  (package
   (name "sbcl-zpb-ttf")
   (version "1.0.3")
   (source
    (origin
     (method git-fetch)
     (uri
      (git-reference
       (url "https://github.com/xach/zpb-ttf")
       (commit "release-1.0.3")))
     (sha256
      (base32
       "1wh66vjijzqlydnrihynpwp6796917xwrh0i9li93c17kyxa74ih"))
     (file-name (string-append "zpb-ttf-" version "-checkout"))))
   (build-system asdf-build-system/sbcl)
   (home-page "https://www.xach.com/lisp/zpb-ttf/")
   (synopsis "ZPB-TTF is a TrueType file parser.")
   (description "TrueType fonts have a publicly documented file
format. ZPB-TTF is a TrueType file parser that provides an interface
for reading typographic metrics, glyph outlines, and other information
from the file.")
   (license license:bsd-style)))

(define-public sbcl-cl-store
  (let ((revision "1")
        (commit "cd01f2610d3360dc01ab972bd9317407aaea7745"))
    (package
     (name "sbcl-cl-store")
     (version (string-append "0.0.0-" revision "." (string-take commit 7)))
     (source
      (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/skypher/cl-store")
         (commit commit)))
       (sha256
        (base32
         "05b7kh5af2ax7vlmphpac4vbqr84j5ivppj96qzb64fxpjpqglm4"))
       (file-name (string-append "cl-store-" version "-checkout"))))
     (build-system asdf-build-system/sbcl)
     (inputs `(("sbcl-rt" ,sbcl-rt)))
     (home-page "http://frisbeejun.ru/clx-truetype/")
     (synopsis "CLX truetype drawing library for Common Lisp")
     (description "Pure Common Lisp TrueType antialiased fonts
rendering using CLX and Xrender extension.")
     (license license:public-domain))))

;; note that clx-truetype sets +font-cache-filename+ to a path in
;; (user-homedir-pathname), and write font cache into that
;; dir. However, when using asdf-build-system/sbcl, the path seems to
;; be set to /gnu/store/sbcl-clx-truetype-xxxx/.fonts/font-cache.sexp,
;; which is read-only on Guix. Thus you probably want to use
;; cl-clx-truetype instead, which installs source files, and the files
;; are compiled the first time it is loaded, and that loads set the
;; path correctly to user dir.
(define-public sbcl-clx-truetype
  (let ((revision "1")
        (commit "c6e10a918d46632324d5863a8ed067a83fc26de8"))
    (package
     (name "sbcl-clx-truetype")
     (version (string-append "0.0.0-" revision "." (string-take commit 7)))
     (source
      (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/filonenko-mikhail/clx-truetype")
         (commit commit)))
       (sha256
        (base32
         "079hyp92cjkdfn6bhkxsrwnibiqbz4y4af6nl31lzw6nm91j5j37"))
       (file-name (string-append "clx-truetype-" version "-checkout"))))
     (build-system asdf-build-system/sbcl)
     (inputs `(("clx" ,sbcl-clx)
               ("sbcl-cl-fad" ,sbcl-cl-fad)
               ("cl-trivial-features" ,cl-trivial-features)
               ("sbcl-zpb-ttf" ,sbcl-zpb-ttf)
               ("sbcl-cl-vectors" ,sbcl-cl-vectors)
               ("sbcl-cl-paths-ttf" ,sbcl-cl-paths-ttf)
               ("sbcl-cl-aa" ,sbcl-cl-aa)
               ("sbcl-cl-store" ,sbcl-cl-store)))
     (home-page "http://frisbeejun.ru/clx-truetype/")
     (synopsis "CLX truetype drawing library for Common Lisp")
     (description "Pure Common Lisp TrueType antialiased fonts
rendering using CLX and Xrender extension.")
     (license license:public-domain))))

(define-public cl-clx-truetype
  (sbcl-package->cl-source-package sbcl-clx-truetype))

(define-public sbcl-salza2
  (package
   (name "sbcl-salza2")
   (version "2.0.9")
   (source
    (origin
     (method git-fetch)
     (uri
      (git-reference
       (url "https://github.com/xach/salza2")
       (commit "release-2.0.9")))
     (sha256
      (base32
       "0p38rj4gq7j5k807php7hrz7l2zyyfshv8i9yms7i8lkgg3433ki"))
     (file-name (string-append "salza2-" version "-checkout"))))
   (build-system asdf-build-system/sbcl)
   (home-page "http://frisbeejun.ru/clx-truetype/")
   (synopsis "CLX truetype drawing library for Common Lisp")
   (description "Pure Common Lisp TrueType antialiased fonts
rendering using CLX and Xrender extension.")
   (license license:public-domain)))

(define-public sbcl-zpng
  (package
   (name "sbcl-zpng")
   (version "1.2.2")
   (source
    (origin
     (method git-fetch)
     (uri
      (git-reference
       (url "https://github.com/xach/zpng")
       (commit "release-1.2.2")))
     (sha256
      (base32
       "0b3ag3jhl3z7kdls3ahdsdxsfhhw5qrizk769984f4wkxhb69rcm"))
     (file-name (string-append "zpng-" version "-checkout"))))
   (build-system asdf-build-system/sbcl)
   (inputs `(("sbcl-salza2" ,sbcl-salza2)))
   (home-page "http://frisbeejun.ru/clx-truetype/")
   (synopsis "CLX truetype drawing library for Common Lisp")
   (description "Pure Common Lisp TrueType antialiased fonts
rendering using CLX and Xrender extension.")
   (license license:public-domain)))

;; sbcl-clx-truetype
;; sbcl-cl-vectors
;; sbcl-cl-aa
;; sbcl-cl-store
;; sbcl-zpng
;; cl-clx-truetype
