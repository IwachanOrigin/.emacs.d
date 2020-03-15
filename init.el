
;; Ignore split window horizontally
(setq split-width-threshold nil)
(setq split-width-threshold 160)

;; package
(require 'package)
(package-initialize)

;; package管理
(setq package-archives
 '(("gnu" . "http://elpa.gnu.org/packages/")
   ("melpa" . "http://melpa.org/packages/")
   ("org" . "http://orgmode.org/elpa/")))

;; find grep コマンドはgitに依存するように変更
(setq find-program "\"C:\\Program Files\\Git\\usr\\bin\\find.exe\""
      grep-program "\"C:\\Program Files\\Git\\usr\\bin\\grep.exe\""
      null-device "/dev/null")

;; diff コマンドもgitに依存するように変更
(setq diff-command "\"C:\\Program Files\\Git\\usr\\bin\\diff.exe\"")

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
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dracula t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; use doom-modeline
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

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
    ;; 起動時に最小化する
    (add-hook 'after-init-hook 'iconify-emacs-when-server-is-done)

    ;; 終了時にyes/noの問い合わせ
    (setq confirm-kill-emacs 'y-or-n-p)
  )
)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(nyan-animate-nyancat t)
 '(nyan-cat-face-number 4)
 '(package-selected-packages
   (quote
    (ido-vertical-mode org-plus-contrib org git-timemachine mwim hungry-delete nyan-mode doom-modeline doom-themes rainbow-delimiters))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
