
;;============================================================================
;;                                 init.el                                  ;;
;;============================================================================

;; profile
;;(require 'profiler)
;;(profiler-start 'cpu)

(defconst my-saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold most-positive-fixnum)

(eval-and-compile
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

  ;; use-package
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)
    (package-install 'diminish))

  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)

  (require 'use-package))

;; ちょっと透過する
(set-frame-parameter (selected-frame) 'alpha '(0.90))

;; cl-lib
(eval-when-compile
  (use-package cl-lib
    :ensure t))

;; サブプロセスに渡すパラメータの文字コードを cp932 にする
;; ref: https://w.atwiki.jp/ntemacs/pages/16.html
(cl-loop for (func args-pos) in '((call-process        4)
                                  (call-process-region 6)
                                  (start-process       3))
         do (eval `(advice-add ',func
                               :around (lambda (orig-fun &rest args)
                                         (setf (nthcdr ,args-pos args)
                                               (mapcar (lambda (arg)
                                                         (if (multibyte-string-p arg)
                                                             (encode-coding-string arg 'cp932)
                                                           arg))
                                                       (nthcdr ,args-pos args)))
                                         (apply orig-fun args))
                               '((depth . 99))))
)

;; c++ mode
(add-hook 'c++-mode-hook
 #'(lambda ()
    (c-set-style "linux")
    (setq indent-tabs-mode nil) ;; indent use space.
    (setq c-basic-offset 4) ;; basic indent value
    (setq tab-width 4)      ;; tab width
))
;; c mode
(add-hook 'c-mode-hook
 #'(lambda ()
    (c-set-style "linux")
    (setq indent-tabs-mode nil) ;; indent use space.
    (setq c-basic-offset 4) ;; basic indent value
    (setq tab-width 4)      ;; tab width
))

;; typescript
(use-package typescript-mode
  :defer t
  :init
  (add-hook 'typescript-mode-hook '(lambda () (setq typescript-indent-level 2)))
  (add-to-list 'auto-mode-alist '("\.ts$" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\.tsx$" . typescript-mode))
)

;; glsl-mode
(use-package glsl-mode
  :defer 5
  :init
  (add-to-list 'auto-mode-alist '("\.vsh$" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\.fsh$" . glsl-mode))
)

;; editorconfig
(use-package editorconfig
  :defer 2
  :init
  (editorconfig-mode)
)
(setq edconf-exec-path "~/.emacs.d/editorconfig")

;; company
(use-package company
  :defer 2
  :init (global-company-mode)
  :config
  (define-key company-active-map (kbd "C-n") 'company-select-next) ;; C-n, C-pで補完候補を次/前の候補を選択
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection) ;; TABで候補を設定
  (define-key company-active-map (kbd "C-s") 'company-filter-candidates) ;; C-sで絞り込む
)

;; all-the-icons
(use-package all-the-icons
  :defer 2)

;; autorevert
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
  ("C-x b" . counsel-switch-buffer)
)

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
  (define-key eglot-mode-map (kbd "C-c <tab>") #'company-complete) ;; C-M(ESC)=WindowsKeyなので入力補完を行うためにdefine-keyを変える
)

;; markdown
(use-package markdown-mode
  :defer 3
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.txt\\'" . gfm-mode))
  ;; need to installed "pandoc.exe" and set environment path for pandoc.exe.
  :init (setq markdown-command "pandoc.exe -s --standalone --metadata pagetitle=markdown -t html5 -c https://cdn.jsdelivr.net/npm/github-markdown-css@3.0.1/github-markdown.css")
)

;; rainbow-delimiters
(use-package rainbow-delimiters
  :defer 3
  :hook (prog-mode . rainbow-delimiters-mode)
)

;; recentf
(use-package recentf
  :defer 1
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 100)
  (recentf-auto-cleanup 'never)
  (recentf-exclude '((expand-file-name package-user-dir)
                     "recentf"
                     "COMMIT_EDITMSG\\'"))
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              C-sの設定を強化する                    ;;
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
  (global-set-key (kbd "C-s") 'isearch-forward-or-swiper)
)

;; migemo
;; This package can use the Roman alphabet to search  the japanese language.
;; We need to install cmigemo for Windows [migemo-kaoriya-64](https://www.kaoriya.net/software/cmigemo/)
;; Please add path cmigemo.exe.
(use-package migemo
  :defer 2
  :config
  ;; C/Migemo を使う
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs" "-i" "\a"))
  ;; 以下の記述は相対パスを絶対パスとして扱う術
  ;; (expand-file-name "~/.emacs.d/init.el")
  (setq migemo-dictionary (expand-file-name "~/.emacs.d/cmigemo-default-win64/dict/cp932/migemo-dict"))  ;; 辞書のパス
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  ;; charset encoding
  ;;(setq migemo-coding-system 'utf-8-unix)
  (setq migemo-coding-system 'cp932-unix)
)

;; ivy-migemo
;; ivy系検索でmigemoを利用できるようにする
(use-package ivy-migemo
  :defer 2
  :config
  ;; toggle migemo
  (define-key ivy-minibuffer-map (kbd "M-m") #'ivy-migemo-toggle-migemo)
  ;; If you want to defaultly use migemo on swiper and counsel-find-file:
  (setq ivy-re-builders-alist '((t . ivy--regex-plus)
                                   (swiper . ivy-migemo--regex-plus)
                                   (counsel-find-file . ivy-migemo--regex-plus)))
)

;; dashboard
(use-package dashboard
  :defer 1
  :diminish
  (dashboard-mode page-break-lines-mode)
  :custom
  (dashboard-startup-banner 2)
  (dashboard-center-content t)
  (dashboard-items '((recents . 15)))
  :hook
  (after-init . dashboard-setup-startup-hook)
)

;; dimmer
(use-package dimmer
  :defer 1
  :custom
  (dimmer-fraction 0.3)
  :config
  (dimmer-mode t)
)

;; ace-window
(use-package ace-window
  :defer 3
  :bind
  ("C-x o" . ace-window)
  :config
  (setq aw-keys '(?j ?k ?l ?u ?i ?o ?h ?y ?n))
  :custom-face
  (aw-leading-char-face ((t (:height 4.0 :foreground "#f1fa8c"))))
)

;; cmake-mode
(use-package cmake-mode
  :defer 3
  :config
  (setq auto-mode-alist (append '(("CMakeLists\\.txt\\'" . cmake-mode)) '(("\\.cmake\\'" . cmake-mode)) auto-mode-alist))
)

;; centaur tabs
(use-package centaur-tabs
  :defer 1
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-set-icons t)       ;; icon
  (setq centaur-tabs-cycle-scope 'tabs) ;; tab group内で循環
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
;;           emacsclientのためのserver設定             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (eq window-system 'w32)
  (use-package server
    :defer 1
    :config (server-start)
    ;; C-x C-cに割り当てる(好みに応じて)
    (global-set-key (kbd "C-x C-c") 'kill-this-buffer)
    ;; M-x exitでEmacsを終了できるようにする
    (defalias 'exit 'save-buffers-kill-emacs)
    ;; 終了時にyes/noの問い合わせ
    (setq confirm-kill-emacs 'yes-or-no-p)))


(setq file-name-handler-alist my-saved-file-name-handler-alist)
;; Run GC every 60 seconds if emacs is idle.
(run-with-idle-timer 120.0 t #'garbage-collect)
(setq gc-cons-threshold 16777216)

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

