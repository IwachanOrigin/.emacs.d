;;; init.el --- summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(org-babel-load-file (concat user-emacs-directory "readme.org"))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-vc-selected-packages
   '((vterm :vc-backend Git :url
            "https://github.com/IwachanOrigin/emacs-libvterm"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
