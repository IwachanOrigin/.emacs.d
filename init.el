
;;============================================================================
;;                                 init.el                                  ;;
;;============================================================================

(require 'package)
;; package-archivesを上書き
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

;; 初期化
(package-initialize)

;; use-package
(when (not (package-installed-p 'use-package))
   (package-refresh-contents)
   (package-install 'use-package)
)
(setq use-package-always-ensure t)
(require 'use-package)

;; c-kで行全体を削除する
(setq kill-whole-line t)

;; 空白文字を表示する
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")

;; cu, cuh
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cuh\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))

;; Ctrl + h をバックスペースに変える
(global-set-key "\C-h" `delete-backward-char)

;; スタートアップメッセージを表示させない
(setq inhibit-startup-message t)

;; 自動保存リストファイルは作らない
(setq auto-save-list-file-prefix nil)

;; バックアップしないようにする
(setq backup-inhibited t)

;; 自動保存ファイルは作らない
(setq auto-save-default nil)

;; バックアップファイルを作成させない
(setq make-backup-files nil)

;; 終了時にオートセーブファイルを削除する
(setq delete-auto-save-files t)

;; タブにスペースを使用する
(setq-default indent-tabs-mode nil)

;; beep とフラッシュを消す
(setq ring-bell-function 'ignore)

;; line number
(if(version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode)
)

;; ちょっと透過する
(set-frame-parameter (selected-frame) 'alpha '(0.90))

;; カーソル行をハイライトする
(global-hl-line-mode t)

;; ff-find-other-fileで利用する拡張子の関連付けを行う
(setq cc-other-file-alist
  '(
     ("\\.c"   (".h"))
     ("\\.cpp"   (".h"))
     ("\\.h"   (".c"".cpp"))
   )
  )

;; ff-find-other-filesで探す際に対象となるディレクトリを設定する
(setq ff-search-directories
  '("." "../src" "../include")
)

;; ff-find-other-fileをMeta+tで動くように設定する
(global-set-key "\M-t" 'ff-find-other-file)

;; バッファのキーを変える
(global-set-key "\M-p" 'previous-buffer)
(global-set-key "\M-n" 'next-buffer)

;; ガベージコレクションの実行頻度を下げる
(setq gc-cons-threshold 1073741824)
(setq garbage-collection-messages t)

;; emacsが利用されてから60s経っても入力がない場合はガベコレ
(run-with-idle-timer 60.0 t #'garbage-collect)

;; 環境を日本語、UTF-8にする
(setenv "LANG" "ja_JP.UTF-8")
(prefer-coding-system 'utf-8-unix)
(set-file-name-coding-system 'cp932)
(setq locale-coding-system 'utf-8-unix)
;; プロセスが出力する文字コードを判定して、process-coding-system の DECODING の設定値を決定する
(setq default-process-coding-system '(undecided-dos . utf-8-unix))
;; cl-lib
(use-package cl-lib
  :ensure t
)
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

;; indent
(setq-default c-basic-offset 4      ;; basic indent value
              tab-width 4           ;; tab width
              indent-tabs-mode nil  ;; tab or space
)

;; c++ style
(defun add-c++-mode-conf ()
  (c-set-style "stroustrup")  ;;スタイルはストラウストラップ
  (show-paren-mode t)         ;;カッコを強調表示する
)
(add-hook 'c++-mode-hook 'add-c++-mode-conf)

;; C style
(defun add-c-mode-common-conf ()
  (c-set-style "stroustrup") ;;スタイルはストラウストラップ
  (show-paren-mode t)        ;;カッコを強調表示する
)
(add-hook 'c-mode-common-hook 'add-c-mode-common-conf)

;; smart hungry delete
(use-package smart-hungry-delete
  :ensure t
  :bind (("C-h" . smart-hungry-delete-backward-char)
         ("C-d" . smart-hungry-delete-forward-char))
  :defer nil ;; dont defer so we can add our functions to hooks
  :config (smart-hungry-delete-add-default-hooks)
)

;; flymake
(use-package flymake
  :ensure t
  :defer t
  :init
  (add-hook 'c-mode-common-hook 'flymake-mode)
  :commands flymake-mode
)

;; eglot
;;
(use-package eglot
  :defer t
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '(c-mode . ("clangd")))
  (add-to-list 'eglot-server-programs '(c++-mode . ("clangd")))
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  ;; format on save
  (add-hook 'c-mode-hook '(lambda() (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
  (add-hook 'c++-mode-hook '(lambda() (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
)

;; markdown
(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.txt\\'" . gfm-mode))
  ;; need to installed "pandoc.exe" and set environment path for pandoc.exe.
  :init (setq markdown-command "pandoc.exe -s --self-contained -t html5 -c https://cdn.jsdelivr.net/npm/github-markdown-css@3.0.1/github-markdown.min.css")
)

;; volatile-highlights
;; yank等の操作時、該当箇所を強調する
(use-package volatile-highlights
  :ensure t
  :defer t
  :config (volatile-highlights-mode t)
)

;; rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode-hook . raibow-delimiters-mode)
)

;; smartparens
;; 閉じかっこなどを自動で入れてくれる
(use-package smartparens
  :ensure t
  :requires smartparens-config
  :diminish smartparens-mode
  :hook (prog-mode-hook . turn-on-smartparens-mode)
  :config (show-smartparens-global-mode t)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              C-sの設定を強化する                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; swiper
(use-package swiper
  :ensure t
  :config
  (defun isearch-forward-or-swiper (use-swiper)
    (interactive "p")
    ;; (interactive "P") ;; 大文字のPだと，C-u C-sでないと効かない
    (let (current-prefix-arg)
      (call-interactively (if use-swiper 'swiper 'isearch-forward))))
  (global-set-key (kbd "C-s") 'isearch-forward-or-swiper)
)

;; ivy
(use-package ivy
  :ensure t
  ;; :config
  ;; (fset 'ivy--regex 'identity)
)

;; migemo
;; 日本語をローマ字検索できるようにする
;; Windows版 migemoが必要 [migemo-kaoriya-64](https://www.kaoriya.net/software/cmigemo/)
(use-package migemo
  :ensure t
  :config
  ;; C/Migemo を使う
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs" "-i" "\a"))
  (setq migemo-dictionary "C:/Users/iwana/.emacs.d/cmigemo-default-win64/dict/cp932/migemo-dict")  ;; 辞書のパス
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  ;; charset encoding
  ;;(setq migemo-coding-system 'utf-8-unix)
  (setq migemo-coding-system 'cp932-unix)
)

;; ivy-migemo
;; ivy系検索でmigemoを利用できるようにする
(use-package ivy-migemo
  :ensure t
  :config
  ;; toggle migemo
  (define-key ivy-minibuffer-map (kbd "M-m") #'ivy-migemo-toggle-migemo)
  ;; If you want to defaultly use migemo on swiper and counsel-find-file:
  (setq ivy-re-builders-alist '((t . ivy--regex-plus)
                              (swiper . ivy-migemo--regex-plus)
                              (counsel-find-file . ivy-migemo--regex-plus))
                              ;(counsel-other-function . ivy-migemo--regex-plus)
                              )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   web browser                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; counsel-web
;; ivy系の検索でweb検索ができる
;; todo : migemoと連携
(use-package counsel-web
  :ensure t
  :defer t
  :config
  (setq counsel-web-engine 'google)
  (setq counsel-web-search-dynamic-update t)
  (setq counsel-web-search-action #'browse-url)
  :init
  ;; Define "C-c w" as a prefix key.
  (defvar counsel-web-map
    (let ((map (make-sparse-keymap "counsel-web")))
      (define-key map (kbd "w") #'counsel-web-suggest)
      (define-key map (kbd "s") #'counsel-web-search)
      (define-key map (kbd ".") #'counsel-web-thing-at-point)
      map))
  (global-set-key (kbd "C-c w") counsel-web-map)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     dashboard                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             rainbow-delimiter                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             Color Identifiers Mode               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package color-identifiers-mode
  :ensure t
  :defer t
  :config
  (add-hook 'after-init-hook 'global-color-identifiers-mode)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           emacsclientのためのsever設定             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (eq window-system 'w32)
  (when (require 'server nil t)
    (server-start)
    ;; C-x C-cに割り当てる(好みに応じて)
    (global-set-key (kbd "C-x C-c") 'kill-this-buffer)
    ;; M-x exitでEmacsを終了できるようにする
    (defalias 'exit 'save-buffers-kill-emacs)
    ;; 終了時にyes/noの問い合わせ
    (setq confirm-kill-emacs 'yes-or-no-p)
))

;; 最大化 <=> 元に戻す
(global-set-key (kbd "<f2>") 'toggle-frame-maximized)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 set color theme                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package modus-themes
  :ensure t                        ; omit this to use the built-in themes
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend))

  ;; Load the theme files before enabling a theme (else you get an error).
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-vivendi) ;; OR (modus-themes-load-operandi)
  :bind ("<f5>" . modus-themes-toggle))

;; custom message
(setq initial-scratch-message "\
;; This buffer is for notes you don't want to save, and for Ruby code.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(modus-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
