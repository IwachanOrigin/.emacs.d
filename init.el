
;;============================================================================
;;                                 init.el                                  ;;
;;============================================================================

;; profile
;;(require 'profiler)
;;(profiler-start 'cpu)

;; GC
(setq gc-cons-threshold most-positive-fixnum)
;; Run GC every 120 seconds if emacs is idle.
(run-with-idle-timer 120.0 t #'garbage-collect)
(add-hook 'emacs-startup-hook
  (lambda ()
  ;; recover default value
    (setq gc-cons-threshold 800000)))

;; magic file name
(defconst my-saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq file-name-handler-alist my-saved-file-name-handler-alist)))

;; package
(eval-when-compile
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

  ;; use-package
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)
    (package-install 'diminish))

  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)

  (require 'use-package))

;; A little transparent.
(set-frame-parameter (selected-frame) 'alpha '(0.90))

;; cl-lib
;;(eval-when-compile
;;  (use-package cl-lib
;;    :ensure t))

;; No need? 様子を見る。 2022/11/17
;; サブプロセスに渡すパラメータの文字コードを cp932 にする
;; ref: https://w.atwiki.jp/ntemacs/pages/16.html
;;(cl-loop for (func args-pos) in '((call-process        4)
;;                                  (call-process-region 6)
;;                                  (start-process       3))
;;         do (eval `(advice-add ',func
;;                               :around (lambda (orig-fun &rest args)
;;                                         (setf (nthcdr ,args-pos args)
;;                                               (mapcar (lambda (arg)
;;                                                         (if (multibyte-string-p arg)
;;                                                             (encode-coding-string arg 'cp932)
;;                                                           arg))
;;                                                       (nthcdr ,args-pos args)))
;;                                         (apply orig-fun args))
;;                               '((depth . 99))))
;;)

;; c++ mode
(add-hook 'c++-mode-hook
 #'(lambda ()
     (c-set-style "linux")
     (c-set-offset 'innamespace 0)
     (setq indent-tabs-mode nil) ;; indent use space.
     (setq c-basic-offset 2) ;; basic indent value
     (setq tab-width 2)      ;; tab width
))
;; c mode
(add-hook 'c-mode-hook
 #'(lambda ()
    (c-set-style "linux")
    (setq indent-tabs-mode nil) ;; indent use space.
    (setq c-basic-offset 2) ;; basic indent value
    (setq tab-width 2)      ;; tab width
    (setq c-set-offset 'innamespace 0)
))


;; glsl-mode
(use-package glsl-mode
  :defer 5
  :config
  (add-to-list 'auto-mode-alist '("\.vsh$" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\.fsh$" . glsl-mode)))

;; editorconfig
(use-package editorconfig
  :defer 2
  :config
  (editorconfig-mode)
  (setq edconf-exec-path "~/.emacs.d/editorconfig"))

;; company
(use-package company
  :defer 2
  :config
  (global-company-mode)
  ;; C-n, C-p to select next/previous candidate for completion
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  ;; TAB to set candidates
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  ;; Narrow by C-s
  (define-key company-active-map (kbd "C-s") 'company-filter-candidates))

;; all-the-icons
(use-package all-the-icons
  :defer 1)

;; autorevert
;; Check for file updates and update buffers as well.
(use-package autorevert
  :defer 3
  :diminish
  :hook (after-init . global-auto-revert-mode))

;; hungry-delete
(use-package hungry-delete
  :defer 3
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

;; counsel
(use-package counsel
  :defer 2
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-x C-r" . counsel-recentf)
  ("C-x b" . counsel-switch-buffer))

;; flycheck
(use-package flycheck
  :defer 1
  :config
  (add-hook 'c-mode-hook 'flycheck-mode)
  (add-hook 'c++-mode-hook 'flycheck-mode))

(use-package flycheck-posframe
  :after flycheck
  :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

;; eglot
(use-package eglot
  :defer 1
  :config
  (add-to-list 'eglot-server-programs '(c-mode . ("clangd")))
  (add-to-list 'eglot-server-programs '(c++-mode . ("clangd")))
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  ;; C-M(ESC)=WindowsKey, so change define-key for input completion
  (define-key eglot-mode-map (kbd "C-c <tab>") #'company-complete))

;; markdown
(use-package markdown-mode
  :defer 3
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.txt\\'" . gfm-mode))
  ;; need to installed "pandoc.exe" and set environment path for pandoc.exe.
  :config (setq markdown-command "pandoc.exe -s --standalone --metadata pagetitle=markdown -t html5 -c https://cdn.jsdelivr.net/npm/github-markdown-css@3.0.1/github-markdown.css"))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :defer 3
  :hook (prog-mode . rainbow-delimiters-mode))

;; recentf
(use-package recentf
  :defer 1
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 100)
  (recentf-auto-cleanup 'never)
  (recentf-exclude '((expand-file-name package-user-dir)
                     "recentf"
                     "COMMIT_EDITMSG\\'")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              Enhance C-s settings                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; swiper
(use-package swiper
  :defer 2
  :ensure t
  :config
  (defun isearch-forward-or-swiper (use-swiper)
    (interactive "p")
    (let (current-prefix-arg)
      (call-interactively (if use-swiper 'swiper 'isearch-forward))))
  (global-set-key (kbd "C-s") 'isearch-forward-or-swiper))

;; migemo
;; This package can use the Roman alphabet to search  the japanese language.
;; We need to install cmigemo for Windows [migemo-kaoriya-64](https://www.kaoriya.net/software/cmigemo/)
;; Please add path cmigemo.exe.
(use-package migemo
  :defer 2
  :config
  ;; use to C/Migemo
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs" "-i" "\a"))
  ;; The following description is the art of treating relative paths as absolute paths
  ;; (expand-file-name "~/.emacs.d/init.el")
  ;; dictionary path
  (setq migemo-dictionary (expand-file-name "~/.emacs.d/cmigemo-default-win64/dict/cp932/migemo-dict"))
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  ;; charset encoding
  ;;(setq migemo-coding-system 'utf-8-unix)
  (setq migemo-coding-system 'cp932-unix)
)

;; ivy-migemo
;; Make migemo available for ivy-based search
(use-package ivy-migemo
  :defer 2
  :config
  ;; toggle migemo
  (define-key ivy-minibuffer-map (kbd "M-m") #'ivy-migemo-toggle-migemo)
  ;; If you want to defaultly use migemo on swiper and counsel-find-file:
  (setq ivy-re-builders-alist '((t . ivy--regex-plus)
                                   (swiper . ivy-migemo--regex-plus)
                                   (counsel-find-file . ivy-migemo--regex-plus))))

;; dashboard
(use-package dashboard
  :defer 2
  :diminish
  (dashboard-mode page-break-lines-mode)
  :custom
  (dashboard-set-heading-icons t)
  (dashboard-set-navigator t)
  (dashboard-set-file-icons t)
  (dashboard-startup-banner 2)
  (dashboard-center-content t)
  (dashboard-items '((recents . 15)))
  :hook
  (after-init . dashboard-setup-startup-hook))

;; dimmer
(use-package dimmer
  :defer 1
  :custom
  (dimmer-fraction 0.3)
  :config
  (dimmer-mode t))

;; ace-window
(use-package ace-window
  :defer 3
  :bind
  ("C-x o" . ace-window)
  :config
  (setq aw-keys '(?j ?k ?l ?u ?i ?o ?h ?y ?n))
  :custom-face
  (aw-leading-char-face ((t (:height 4.0 :foreground "#f1fa8c")))))

;; cmake-mode
(use-package cmake-mode
  :defer 3
  :config
  (setq auto-mode-alist (append '(("CMakeLists\\.txt\\'" . cmake-mode)) '(("\\.cmake\\'" . cmake-mode)) auto-mode-alist)))

;; centaur tabs
(use-package centaur-tabs
  :defer 1
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-set-icons t)       ;; icon
  (setq centaur-tabs-cycle-scope 'tabs) ;; Circulate within a tab group
  :bind
  ("C-x ," . centaur-tabs-backward)
  ("C-x ." . centaur-tabs-forward)
  ("C-x n" . centaur-tabs--create-new-tab))

;; doom-modeline
(use-package doom-modeline
  :defer 2
  :hook (after-init . doom-modeline-mode))

;; setting modus themes
(setq modus-themes-syntax 'faint)
(setq modus-themes-tabs-accented t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       server configuration for emacsclient       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (eq window-system 'w32)
  (use-package server
    :defer 1
    :config (server-start)
    ;; Assign kill buffer to C-x C-c
    (global-set-key (kbd "C-x C-c") 'kill-this-buffer)
    ;; Allow Emacs to exit with M-x exit
    (defalias 'exit 'save-buffers-kill-emacs)
    ;; yes/no query on exit
    (setq confirm-kill-emacs 'yes-or-no-p)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(editorconfig modus-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:height 4.0 :foreground "#f1fa8c")))))

;; profile
;;(profiler-report)
;;(profiler-stop)

