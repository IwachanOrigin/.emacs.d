
;;============================================================================
;;                                 package                                  ;;   
;;============================================================================
(require 'package)
;; MELPAを追加
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; Orgを追加
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;; ELPAを追加
(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/") t)

;; 初期化
(package-initialize)

;; c-kで行全体を削除する
(setq kill-whole-line t)

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

;; use ido
;; 中間/あいまい一致
(ido-mode 1)
(use-package ido-vertical-mode
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)    ;; C-n/C-pで候補選択する
  (setq ido-vertical-show-count t)
)

;; whitespaceを利用する。1行の最大長は200文字にする。
(use-package whitespace
  :defer t
)
(setq whitespace-line-column 200) ;; limit line length
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

;; use Hungry deletion
(use-package hungry-delete
  :defer t
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

;; use rainbow deli
(use-package rainbow-delimiters
    :defer t
    :hook
    (prog-mode . rainbow-delimiters-mode))

;; Automatically reload files was modified by external program
(use-package autorevert
  :defer t
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

;; use line number
(use-package display-line-numbers
  :defer t
  :ensure nil
  :hook
  ((prog-mode yaml-mode systemd-mode) . display-line-numbers-mode))

;; smart move
(use-package mwim
  :defer t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

;; use yaml-mode
(use-package yaml-mode
  :defer t
  :mode ("\\.yaml\\'" "\\.yml\\'")
  :custom-face
  (font-lock-variable-name-face ((t (:foreground "violet")))))

;; use Dockerfile-mode
(use-package dockerfile-mode
  :defer t
  :mode "\\Dockerfile\\'")

;; c/c++ mode
(use-package cc-mode
  :defer t
  :bind (:map c-mode-base-map
         ("C-c c" . compile))
  :hook (c-mode-common . (lambda ()
                           (c-set-style "stroustrup")
                           (setq indent-tabs-mode nil)     ; インデントは空白文字で行う
                           (c-set-offset 'arglist-close 0) ; 関数の引数リストの閉じ括弧はインデントしない
                           (setq tab-width 4)
                           (setq c-base-offset 4))))

;; c# mode
(use-package omnisharp
  :after csharp-mode
  :preface
  (progn
    (defun my/configure-omnisharp ()
      (omnisharp-mode)
      (add-to-list 'company-backends #'company-omnisharp)
      (company-mode)
      (local-set-key (kbd "C-c C-c") #'recompile)))
  :init
  (progn
    (add-hook 'csharp-mode-hook #'my/configure-omnisharp))
  :config
  (progn
    (bind-key "C-c r r" #'omnisharp-run-code-action-refactoring omnisharp-mode-map))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use cua-mode
(cua-mode t)
(setq cua-enable-cua-keys nil) ; デフォルトキーバインドを無効化
(define-key global-map (kbd "C-x SPC") 'cua-set-rectangle-mark)

;; use markdown mode
(package-install 'markdown-mode)
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;; コードブロックのハイライト化
(setq markdown-fontify-code-blocks-natively t)

;; use dashboard
(use-package dashboard
    :defer t
    :diminish
    (dashboard-mode page-break-lines-mode)
    :custom
    (dashboard-startup-banner 4)
    (dashboard-items '(
               (recents . 15)
               (agenda . 5)))
    :hook
    (after-init . dashboard-setup-startup-hook)
    :config
    (add-to-list 'dashboard-items '(agenda) t))

;; font
(setq default-frame-alist
      (append (list
              '(font . "Cica-12"))
              default-frame-alist))

;; fullscreen mode
(set-frame-parameter nil 'fullscreen 'maximized)

;; color theme
(load-theme 'misterioso t)

;;
;; end
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (qml-mode markdown-mode yaml-mode use-package transient rainbow-delimiters projectile org omnisharp mwim lsp-mode leaf ido-vertical-mode hungry-delete dockerfile-mode dashboard all-the-icons-dired))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-variable-name-face ((t (:foreground "violet")))))
