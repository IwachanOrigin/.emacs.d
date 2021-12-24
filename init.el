
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

;; どんなものだろうと、タブ幅は4にする
(setq default-tab-width 4)

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

;; set color theme
(use-package modus-themes
  :ensure                         ; omit this to use the built-in themes
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend))

  ;; Load the theme files before enabling a theme (else you get an error).
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-operandi) ;; OR (modus-themes-load-vivendi)
  :bind ("<f5>" . modus-themes-toggle))

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
