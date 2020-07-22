
;; Ignore split window horizontally
(setq split-width-threshold nil)
(setq split-width-threshold 160)

;;============================================================================
;;                                 package                                  ;;   
;;============================================================================
(require 'package)
;; MELPAを追加
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; MELPA-stableを追加
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;; Orgを追加
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;; ELPAを追加
(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/") t)

;; 初期化
(package-initialize)

;; font
(setq default-frame-alist
      (append (list
              '(font . "Cica-12"))
              default-frame-alist))

;; c-kで行全体を削除する
(setq kill-whole-line t)

;; cursorの点滅をやめる
(blink-cursor-mode 0)

;; cu, cuh
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cuh\\'" . c++-mode))

;; Ctrl + h をバックスペースに変える
(global-set-key "\C-h" `delete-backward-char)

;; スタートアップメッセージを表示させない
(setq inhibit-startup-message t)

;; バックアップファイルを作成させない
(setq make-backup-files nil)

;; 終了時にオートセーブファイルを削除する
(setq delete-auto-save-files t)

;; C-kで行全体を削除する
(setq kill-whole-line t)

;; 改行コードを表示する
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")

;; タブにスペースを使用する
(setq-default tab-width 4 indent-tabs-mode nil)

;; メニューバーを非表示
(menu-bar-mode 0)

;; ツールバーを非表示
(tool-bar-mode 0)

;; scroll bar false
(scroll-bar-mode 0)

;; beep とフラッシュを消す
(setq ring-bell-function 'ignore)

;; 現在ポイントがある関数名をモードラインに表示
(which-function-mode 1)

;; 環境を日本語、UTF-8にする
(set-locale-environment nil)
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

;;-------------------------------------
;; hydra Start
;;-------------------------------------

;; hydra yank
(defhydra hydra-yank-pop ()
  "yank"
  ("C-y" yank nil)
  ("M-y" yank-pop nil)
  ("y" (yank-pop 1) "next")
  ("Y" (yank-pop -1) "prev")
  ("l" helm-show-kill-ring "list" :color blue))   ; or browse-kill-ring
(global-set-key (kbd "M-y") #'hydra-yank-pop/yank-pop)
(global-set-key (kbd "C-y") #'hydra-yank-pop/yank)

;; ace-window
;; hydra-frame-window is designed from ace-window (C-x f) and
;; matches aw-dispatch-alist with a few extra
(defhydra hydra-frame-window (:color red :hint nil)
  "
^Delete^                       ^Frame resize^             ^Window^                Window Size^^^^^^   ^Text^                         (__)
_0_: delete-frame              _g_: resize-frame-right    _t_: toggle               ^ ^ _k_ ^ ^        _K_                           (oo)
_1_: delete-other-frames       _H_: resize-frame-left     _e_: ace-swap-win         _h_ ^+^ _l_        ^+^                     /------\\/
_2_: make-frame                _F_: fullscreen            ^ ^                       ^ ^ _j_ ^ ^        _J_                    / |    ||
_d_: kill-and-delete-frame     _n_: new-frame-right       _w_: ace-delete-window    _b_alance^^^^      ^ ^                   *  /\\---/\\  ~~  C-x f ;
"
  ("0" delete-frame :exit t)
  ("1" delete-other-frames :exit t)
  ("2" make-frame  :exit t)
  ("b" balance-windows)
  ("d" kill-and-delete-frame :exit t)
  ("e" ace-swap-window)
  ("F" toggle-frame-fullscreen)   ;; is <f11>
  ("g" resize-frame-right :exit t)
  ("H" resize-frame-left :exit t)  ;; aw-dispatch-alist uses h, I rebind here so hjkl can be used for size
  ("n" new-frame-right :exit t)
  ;; ("r" reverse-windows)
  ("t" toggle-window-spilt)
  ("w" ace-delete-window :exit t)
  ("x" delete-frame :exit t)
  ("K" text-scale-decrease)
  ("J" text-scale-increase)
  ("h" shrink-window-horizontally)
  ("k" shrink-window)
  ("j" enlarge-window)
  ("l" enlarge-window-horizontally))

;;-------------------------------------
;; hydra End
;;-------------------------------------

;; use ace-window
(use-package ace-window
  :functions hydra-frame-window/body
  :bind
    ("C-x f" . hydra-frame-window/body)
  :custom
    (aw-keys '(?j ?k ?l ?i ?o ?h ?y ?u ?p))
  :custom-face
    (aw-leading-char-face ((t (:height 4.0 :foreground "#f1fa8c"))))
)

;; use ivy
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
)

;; use swiper
(use-package swiper
  :ensure t
  :config
  (global-set-key "\C-s" 'swiper)
)

;; use counsel
(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c a") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  )

;; use ivy-rich
(use-package ivy-rich
  :ensure t
  :after (ivy)
  :init
  (setq ivy-rich-path-style 'abbrev
        ivy-virtual-abbreviate 'full)
  :config (ivy-rich-mode 1))

;; use which-key
(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode)
)

;; use amx
(use-package amx)

;; whitespaceを利用する。1行の最大長は200文字にする。
(use-package whitespace)
(setq whitespace-line-column 200) ;; limit line length
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

;; use git timemachine
(use-package git-timemachine
  :bind ("M-g t" . git-timemachine-toggle))

;; use Hungry deletion
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

;; use rainbow deli
(use-package rainbow-delimiters
    :hook
    (prog-mode . rainbow-delimiters-mode))

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

;; use line number
(use-package display-line-numbers
  :ensure nil
  :hook
  ((prog-mode yaml-mode systemd-mode) . display-line-numbers-mode))

;; smart move
(use-package mwim
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

;; use nyan mode
(use-package nyan-mode
   :custom
   (nyan-cat-face-number 6)
   (nyan-animate-nyancat t)
   :hook
   (doom-modeline-mode . nyan-mode))

;; use doom-themes
(use-package doom-themes
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  :custom-face
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-neotree-config)
  (doom-themes-org-config)
  ;; use doom-modeline
  (use-package doom-modeline
    :custom
    (doom-modeline-buffer-file-name-style 'truncate-with-project)
    (doom-modeline-icon t)
    (doom-modeline-major-mode-icon nil)
    (doom-modeline-minor-modes nil)
    :hook
    (after-init . doom-modeline-mode)
    :config
    (set-cursor-color "cyan")
    (line-number-mode 0)
    (column-number-mode 0)
    (doom-modeline-def-modeline 'main
  '(bar workspace-name window-number modals matches buffer-info remote-host buffer-position word-count parrot selection-info)
  '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker))
  )
)

;; use yaml-mode
(use-package yaml-mode
  :mode ("\\.yaml\\'" "\\.yml\\'")
  :custom-face
  (font-lock-variable-name-face ((t (:foreground "violet")))))

;; use Dockerfile-mode
(use-package dockerfile-mode
  :mode "\\Dockerfile\\'")

;; use magit
(use-package magit
  :custom
  (magit-auto-revert-mode nil)
  :bind
  ("M-g s" . magit-status))

;; c/c++ mode
(use-package cc-mode
  :bind (:map c-mode-base-map
         ("C-c c" . compile))
  :hook (c-mode-common . (lambda ()
                           (c-set-style "stroustrup")
                           (setq indent-tabs-mode nil)     ; インデントは空白文字で行う
                           (c-set-offset 'arglist-close 0) ; 関数の引数リストの閉じ括弧はインデントしない
                           (setq tab-width 4)
                           (setq c-base-offset 4))))
(use-package ccls
  :custom
  (ccls-executable "/usr/local/bin/ccls")
  (ccls-sem-highlight-method 'font-lock)
  :config
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-bullets
      :custom (org-bullets-bullet-list '("" "󿢤" "󿢧" "󿢪" "󿢭" "󿢰" "󿢳" "󿢶" "󿢹" "󿢼"))
      :hook (org-mode . org-bullets-mode))

;; 見出しの余分な*を消す
(setq org-hide-leading-stars t)

;; .orgファイルは自動的にorg-mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; ショートカットキー
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/OneDrive/Org/gtd.org" "Tasks")
     "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/OneDrive/Org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))

;; use cua-mode
(cua-mode t)
(setq cua-enable-cua-keys nil) ; デフォルトキーバインドを無効化
(define-key global-map (kbd "C-x SPC") 'cua-set-rectangle-mark)

;; window を透明にする
(add-to-list 'default-frame-alist '(alpha . (0.90 0.90)))

;; emacs を fullscreen mode で起動する
(add-to-list 'default-frame-alist '(fullscreen . fullboth))

;; server start for emacs-client
(when window-system ; GUI時
  (require 'server)
  (unless (eq (server-running-p) 't)
    (server-start)
    (defun iconify-emacs-when-server-is-done ()
      (unless server-clients (iconify-frame)))

    ;; C-x C-cに割り当てる
    (global-set-key (kbd "C-x C-c") 'server-edit)
    ;; M-x exitでEmacsを終了できるようにする
    (defalias 'exit 'save-buffers-kill-emacs)
    ;; 終了時にyes/noの問い合わせ
    (setq confirm-kill-emacs 'y-or-n-p)
  )
)

;; use markdown mode
(use-package markdown-mode
  :custom
  (markdown-hide-markup nil)
  (markdown-bold-underscore t)
  (markdown-italic-underscore t)
  (markdown-header-scaling t)
  (markdown-indent-function t)
  (markdown-enable-math t)
  (markdown-hide-urls nil)
  :custom-face
  (markdown-header-delimiter-face ((t (:foreground "mediumpurple"))))
  (markdown-header-face-1 ((t (:foreground "violet" :weight bold :height 1.0))))
  (markdown-header-face-2 ((t (:foreground "lightslateblue" :weight bold :height 1.0))))
  (markdown-header-face-3 ((t (:foreground "mediumpurple1" :weight bold :height 1.0))))
  (markdown-link-face ((t (:background "#0e1014" :foreground "#bd93f9"))))
  (markdown-list-face ((t (:foreground "mediumpurple"))))
  (markdown-pre-face ((t (:foreground "#bd98fe"))))
  :mode "\\.md\\'")

;; use dashboard
(use-package dashboard
    :diminish
    (dashboard-mode page-break-lines-mode)
    :custom
    (dashboard-startup-banner 4)
    (dashboard-items '((recents . 15)
               (projects . 5)
               (bookmarks . 5)
               (agenda . 5)))
    :hook
    (after-init . dashboard-setup-startup-hook)
    :config
    (add-to-list 'dashboard-items '(agenda) t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(all-the-icons-scale-factor 1.0)
 '(aw-keys (quote (106 107 108 105 111 104 121 117 112)) t)
 '(counsel-grep-base-command
   "ag -S --noheading --nocolor --nofilename --numbers '%s' %s")
 '(counsel-yank-pop-height 15 t)
 '(counsel-yank-pop-separator "
-------
")
 '(doom-themes-enable-bold t)
 '(doom-themes-enable-italic t)
 '(enable-recursive-minibuffers t)
 '(ivy-format-function (quote ivy-format-function-arrow) t)
 '(ivy-on-del-error-function nil)
 '(ivy-use-selectable-prompt t)
 '(ivy-use-virtual-buffers t)
 '(nyan-animate-nyancat t)
 '(nyan-cat-face-number 4)
 '(package-selected-packages
   (quote
    (org-bullets swiper ivy hydra amx which-key ivy-posframe counsel ace-window all-the-icons-ivy-rich all-the-icons-dired ccls magit dockerfile-mode yaml-mode dashboard ivy-rich markdown-mode ido-vertical-mode org-plus-contrib org git-timemachine mwim hungry-delete nyan-mode doom-modeline doom-themes rainbow-delimiters)))
 '(swiper-action-recenter t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:height 4.0 :foreground "#f1fa8c"))))
 '(font-lock-variable-name-face ((t (:foreground "violet"))))
 '(markdown-header-delimiter-face ((t (:foreground "mediumpurple"))))
 '(markdown-header-face-1 ((t (:foreground "violet" :weight bold :height 1.0))))
 '(markdown-header-face-2 ((t (:foreground "lightslateblue" :weight bold :height 1.0))))
 '(markdown-header-face-3 ((t (:foreground "mediumpurple1" :weight bold :height 1.0))))
 '(markdown-link-face ((t (:background "#0e1014" :foreground "#bd93f9"))))
 '(markdown-list-face ((t (:foreground "mediumpurple"))))
 '(markdown-pre-face ((t (:foreground "#bd98fe")))))
