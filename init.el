
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

;; 空白文字を表示する
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")

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

;; タブにスペースを使用する
(setq-default indent-tabs-mode nil)

;; タブ幅は2にする
(setq-default tab-width 2)

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

;; set color theme
(load-theme 'light-blue t)

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

;;; ff-find-other-fileをMeta+tで動くように設定する
(global-set-key "\M-t" 'ff-find-other-file)

;; custom message
(setq initial-scratch-message "\
;; This buffer is for notes you don't want to save, and for Ruby code.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.")

