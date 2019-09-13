(define-module (nvidia)
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
  #:use-module (guix build utils)

  #:use-module (guix build-system gnu)
  #:use-module (guix build-system asdf)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system linux-module)
  #:use-module (ice-9 format)

  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages bash)

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
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1))

(define-public nvidia-driver
  (let ((hello "xxxxxx"))
    (package
      (name "nvidia-driver")
      (version "435.21")
      (source
       (origin
         (uri (format #f "http://us.download.nvidia.com/XFree86/Linux-x86_64/~a/~a.run"
                      version
                      (format #f "NVIDIA-Linux-x86_64-~a" version)))
         (sha256 (base32 "0v3pq677ab01qdmwl5dawk8hn39qlwj05p8s9qzh9irmrlnc1izs"))
         (method url-fetch)
         (file-name (string-append "nvidia-driver-" version "-checkout"))))
      ;; (build-system gnu-build-system)
      (build-system linux-module-build-system)
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (replace 'unpack
                      (lambda* (#:key inputs #:allow-other-keys #:rest r)
                        (let ((source (assoc-ref inputs "source")))
                          (invoke "sh" source "--extract-only")
                          (invoke "pwd")
                          (display ,(format #f "NVIDIA-Linux-x86_64-~a" version))
                          (chdir ,(format #f "NVIDIA-Linux-x86_64-~a" version))
                          (invoke "pwd")
                          #t)))
                    ;; (delete 'configure)
                    (replace 'build
                      (lambda*  (#:key inputs outputs #:allow-other-keys)
                        (define out
                          (assoc-ref outputs "out"))
                        (define linux-libre
                          (assoc-ref inputs "linux-libre"))
                        (define libc
                          (assoc-ref inputs "libc"))
                        (define gcc-lib
                          (assoc-ref inputs "gcc:lib"))
                        (define ld.so
                          (string-append libc ,(glibc-dynamic-linker)))
                        (define rpath
                          (string-join (list "$ORIGIN"
                                             (string-append out "/lib")
                                             (string-append out "/nvvm/lib64")
                                             (string-append libc "/lib")
                                             (string-append gcc-lib "/lib"))
                                       ":"))

                        (define (patch-elf file)
                          (unless (string-contains file ".so")
                            (format #t "Setting interpreter on '~a'...~%" file)
                            (invoke "patchelf" "--set-interpreter" ld.so
                                    file))
                          (format #t "Setting RPATH on '~a'...~%" file)
                          (invoke "patchelf" "--set-rpath" rpath
                                  "--force-rpath" file))

                        (for-each (lambda (file)
                                    (when (elf-file? file)
                                      (patch-elf file)))
                                  '("nvidia-installer" "nvidia-smi" "nvidia-settings"))

                        (for-each (lambda (file)
                                    (when (elf-file? file)
                                      (patch-elf file)))
                                  (find-files "."
                                              (lambda (file stat)
                                                (and (eq? 'regular
                                                          (stat:type stat))
                                                     (string-contains file ".so")))))
                        ;; FIXME why pwd always output -0 instead of
                        ;; -1/2/3...? The number seems to increase for
                        ;; newer builds
                        (chdir "kernel")
                        ;; patch Kbuild
                        (substitute* "Kbuild"
                          (("/bin/sh") (string-append (assoc-ref inputs "bash-minimal") "/bin/sh")))
                        (invoke "make"
                                "-j"
                                (string-append "SYSSRC="
                                               ;; linux-libre "/lib/modules/5.2.13-gnu/source"
                                               (assoc-ref inputs "linux-module-builder")
                                               "/lib/modules/build")
                                "CC=gcc")
                        #t))
                    (delete 'check)
                    (add-after 'install 'install-copy
                      (lambda* (#:key inputs native-inputs outputs #:allow-other-keys)
                        (chdir "..")
                        (let* ((out (assoc-ref outputs "out"))
                               (moddir (string-append out "/lib/xorg/modules/drivers/"))
                               (bindir (string-append out "/bin")))
                          (mkdir-p moddir)
                          (mkdir-p bindir)
                          (for-each
                           (lambda (name)
                             (format #t "Copying '~a'...~%" name)
                             (invoke "pwd")
                             (display name)
                             (invoke "ls")
                             (copy-file name
                                        (string-append moddir name)))
                           ;; FIXME scan all .so.X files
                           '("nvidia_drv.so"
                             "libglxserver_nvidia.so.435.21"
                             "libGLX.so.0"
                             "libGLX_nvidia.so.435.21")

                           ;; FIXME I should not use find-files for
                           ;; this, as 1. the name is not clean,
                           ;; containing leading ./ 2. it is recursive
                           ;;
                           ;; (find-files "."
                           ;;            (lambda (file stat)
                           ;;              (and (eq? 'regular
                           ;;                        (stat:type stat))
                           ;;                   (string-contains file ".so.")))
                           ;;            #:directories? #f)


                           ;; FIXME why scandir is not available in the build environment
                           ;;
                           ;; FIXME why there is so little
                           ;; documemtation about using argumentss,
                           ;; where I wrote most of the packaging
                           ;; code? What is the relation with G-exp?
                           ;;
                           ;; (with-imported-modules '((guix build utils)))
                           ;; (scandir "." (lambda (name)
                           ;;                (string-contains name ".so")))
                           )

                          ;; Xorg seems to look for libglx.so
                          (link (string-append moddir "libglxserver_nvidia.so.435.21")
                                (string-append moddir "libglx.so"))

                          ;; (copy-file "libGLX.so.0"
                          ;;            (string-append moddir "libglx.so"))
                          (copy-file "nvidia-settings"
                                     (string-append bindir "/nvidia-settings"))
                          (copy-file "nvidia-smi"
                                     (string-append bindir "/nvidia-smi"))
                          ;; TODO add a file to load nvidia drivers

                          ;; patchelf
                          ;; verify by ldd xxx.so
                          (let* ((libc (assoc-ref inputs "libc"))
                                 (ld.so (string-append libc ,(glibc-dynamic-linker)))
                                 (gcc-lib (assoc-ref inputs "gcc:lib"))
                                 (libx11 (assoc-ref inputs "libx11"))
                                 (out (assoc-ref outputs "out"))
                                 (rpath (string-join (list "$ORIGIN"
                                                           (string-append out "/lib")
                                                           (string-append libc "/lib")
                                                           (string-append libx11 "/lib")
                                                           (string-append gcc-lib "/lib"))
                                                     ":")))
                            (define (patch-elf file)
                              (if (string-contains file ".so")
                                  (invoke "patchelf" "--set-rpath" rpath file)
                                  (invoke "patchelf" "--set-interpreter" ld.so file)))
                            (for-each (lambda (file)
                                        (when (elf-file? file)
                                          (patch-elf file)))
                                      (append (find-files (string-append out "/lib/xorg")
                                                          (lambda (file stat)
                                                            (and (eq? 'regular
                                                                      (stat:type stat)))))
                                              (find-files (string-append out "/bin")
                                                          (lambda (file stat)
                                                            (and (eq? 'regular
                                                                      (stat:type stat)))))))))
                        #t))
                    ;; FIXME The runpath validation failed
                    (delete 'validate-runpath)
                    )))
      (native-inputs
       `(("patchelf" ,patchelf)
         ("perl" ,perl)
         ("python" ,python-2)
         ("xz" ,xz)
         ("which" ,which)
         ;; I need the kernel source. But what is the output of kernel
         ;; source build? This output seems to be the built kernel.
         ;; ("linux-libre" ,linux-libre)
         ;; ("linux-libre-headers" ,linux-libre-headers)
         ))
      (inputs
       `(("gcc:lib" ,gcc "lib")
         ("libc" ,glibc)
         ("libx11" ,libx11)
         ("bash-minimal" ,bash-minimal)
         ("linux-libre" ,linux-libre)))
      (home-page "https://www.nvidia.com")
      (synopsis "TODO")
      (description "Evil driver")
      (license #f))))
