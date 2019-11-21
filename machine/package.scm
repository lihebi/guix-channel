;; Install by guix package -m package.scm

(specifications->manifest
 (append
  ;; essential packages
  '("emacs" "git" "nss-certs" "glibc-locales" "subversion")
  ;; cmd utilities
  '("grep" "sed" "coreutils" "bc"
    "aspell" "aspell-dict-en" ; for ispell executable in flyspell-mode
    "msmtp" "iptables"
    "cloc" "patchelf" "youtube-dl"
    "the-silver-searcher" "translate-shell"
    "htop" "curl" "tmux" "parted" "gparted" "unzip")
  ;; system
  '("linux-pam" "file" "shepherd")
  '("openssh" "openssl")
  ;; languages
  '("python" "bash" "guile" "sbcl")
  ;; compiler toolchain
  '("gcc-toolchain"
    "bison" "flex"
    "pkg-config" "make" "cmake" "autoconf" "automake")
  ;; tk of python. Seems the python:tk does the trick
  '("tk" "tklib")
  ;; X11 utilities
  '("alsa-utils" "pavucontrol"
    "ncurses"
    "xinput"
    "thunar" "tumbler"
    ;; "nautilus"
    "feh"
    "xrandr"
    "xrdb" "xmodmap"
    "rxvt-unicode"
    ;; fonts (chromium is not getting monospace fonts correctly)
    "xlsfonts" "xset" "fontconfig"
    "font-anonymous-pro" "font-fira-mono" "font-dejavu"
    "font-wqy-microhei" "font-wqy-zenhei"
    ;; screenshot. Also: maim
    "gnome-screenshot" "arandr")
  ;; pdftools
  '("imagemagick" "cairo" "libpng" "zlib" "poppler")
  ;; stumpwm required libraries
  '("sbcl-slime-swank" "cl-trivial-features")
  ;; need my channel
  '("cl-clx-truetype")
  ;; FIXME ungoogled-chromium is failing to build. I cannot remove it either,
  ;; because then I'll have no browser.
  '("ungoogled-chromium")
  '("racket")
  ;; other
  '("picocom"                           ; serial program
    )))
