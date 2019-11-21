;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules (gnu)
             (srfi srfi-1)
             (guix profiles)
             (guix packages)
             (nvidia))
(use-service-modules desktop networking ssh xorg web)
(use-package-modules base bash linux ssh perl lisp)

(define %root (file-system
               (mount-point "/")
               (device "/dev/sda2")
               (type "ext4")))
(define %home (file-system
               (mount-point "/home")
               (device "/dev/sda4")
               (type "ext4")))
(define %boot (file-system
               (mount-point "/boot/efi")
               (device "/dev/sda1")
               (type "vfat")))
(define %file-system (cons* %root %home %boot
                            %base-file-systems))

(define %user (user-account
               (name "hebi")
               (comment "Hebi Li")
               (group "users")
               (home-directory "/home/hebi")
               (supplementary-groups
                '("wheel" "netdev" "audio" "video"))))

(define %system-packages (list (specification->package "openbox")
                               (specification->package "ratpoison")
                               (specification->package "stumpwm")
                               ;; (specification->package "xmonad")
                               ;; (specification->package "sway")
                               (specification->package "nss-certs")))

(define %nginx-service
  ;; when not using guix, use /etc/nginx/nginx.conf
  (service nginx-service-type
           (nginx-configuration
            (server-blocks
             (list
              (nginx-server-configuration
               ;; FIXME ssl https. This is critical, otherwise nginx
               ;; config test fail and service cannot be started
               (listen '("80"))
               ;; FIXME localhost? This does not seem important
               (server-name '("localhost"))
               ;; Cannot use a home directory such as ~/git/www,
               ;; because x permission is not set for ~ for user
               ;; nginx
               (root "/srv/www")))))))

(operating-system
 (locale "en_US.utf8")
 (timezone "America/Chicago")
 (kernel linux-libre)
 (initrd-modules %base-initrd-modules)

 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (target "/boot/efi")))
 (file-systems %file-system)
 (host-name "antelope")
 (users (cons* %user
               %base-user-accounts))
 (packages
  (append
   %system-packages
   %base-packages))
 (services
  (append
   (list
    ;; (service gnome-desktop-service-type)
    %nginx-service
    ;; (service hello-service-type)
    (service openssh-service-type))
   %desktop-services)))
