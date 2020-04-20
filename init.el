
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

;; find grep コマンドはgitに依存するように変更
(setq find-program "\"C:\\Program Files\\Git\\usr\\bin\\find.exe\""
      grep-program "\"C:\\Program Files\\Git\\usr\\bin\\grep.exe\""
      null-device "/dev/null")

;; diff コマンドもgitに依存するように変更
(setq diff-command "\"C:\\Program Files\\Git\\usr\\bin\\diff.exe\"")

;; use bash
(setq shell-file-name "C:\\Program Files\\Git\\bin\\bash.exe")
(setq explicit-bash.exe-args '("--login" "-i"))

;; C-kで行全体を削除する
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

;;-------------;;
;; org-mode    ;;
;;-------------;;
;; 画像をインラインで表示
(setq org-startup-with-inline-images t)

;; 見出しの余分な*を消す
(setq org-hide-leading-stars t)

;; LOGBOOK drawerに時間を格納する
(setq org-clock-into-drawer t)

;; .orgファイルは自動的にorg-mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; save file 
(setq org-directory "~/OneDrive/Org")
(setq org-default-notes-file "notes.org")

; Org-captureのテンプレート（メニュー）の設定
(setq org-capture-templates
    '(("n" "Note" entry (file+headline "~/OneDrive/Org/notes.org" "Notes")
    "* %?\nEntered on %U\n %i\n %a")
    ))

;; ショートカットキー
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; use ido-mode
(ido-mode t)

;; use ido-vertical-mode
(use-package ido-vertical-mode
    :config
    (ido-vertical-mode 1)
    (setq ido-vertical-define-keys 'C-n-and-C-p-only) ;; C-n/C-p で選択
    (setq ido-vertical-show-count t)
)

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
   (nyan-cat-face-number 4)
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
  (doom-themes-org-config))

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
    (column-number-mode 0))

;; use beacon
(use-package beacon
    :custom
    (beacon-color "yellow")
    :config
    (beacon-mode 1))

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
                            (c-set-style "bsd")
                            (setq tab-width 4)
                            (setq c-base-offset 4))))
(use-package ccls
  :custom
  (ccls-executable "/usr/local/bin/ccls")
  (ccls-sem-highlight-method 'font-lock)
  :config
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))

;; eshell setting
(setq eshell-prompt-regexp "^[^#$\n]*[#$] "
      eshell-prompt-function
      (lambda nil
        (concat
	 "[" (user-login-name) "@" (system-name) " "
	 (if (string= (eshell/pwd) (getenv "HOME"))
	     "~" (eshell/basename (eshell/pwd)))
	 "]"
	 (if (= (user-uid) 0) "# " "$ "))))

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
 '(doom-themes-enable-bold t)
 '(doom-themes-enable-italic t)
 '(nyan-animate-nyancat t)
 '(nyan-cat-face-number 4)
 '(package-selected-packages
   (quote
    (ccls magit dockerfile-mode yaml-mode dashboard ivy-rich ob-mermaid beacon uuidgen markdown-mode ido-vertical-mode org-plus-contrib org git-timemachine mwim hungry-delete nyan-mode doom-modeline doom-themes rainbow-delimiters))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-variable-name-face ((t (:foreground "violet"))))
 '(markdown-header-delimiter-face ((t (:foreground "mediumpurple"))))
 '(markdown-header-face-1 ((t (:foreground "violet" :weight bold :height 1.0))))
 '(markdown-header-face-2 ((t (:foreground "lightslateblue" :weight bold :height 1.0))))
 '(markdown-header-face-3 ((t (:foreground "mediumpurple1" :weight bold :height 1.0))))
 '(markdown-link-face ((t (:background "#0e1014" :foreground "#bd93f9"))))
 '(markdown-list-face ((t (:foreground "mediumpurple"))))
 '(markdown-pre-face ((t (:foreground "#bd98fe")))))
