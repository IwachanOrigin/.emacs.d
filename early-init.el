
;;============================================================================
;;                           early-init.el                                  ;;
;;============================================================================

;; For Emacs 27+
(setq package-enable-at-startup nil)
;; Always load newest byte code
(setq load-prefer-newer t)

;; menu bar false
(push '(menu-bar-lines . nil) default-frame-alist)
;; tool bar false
(push '(tool-bar-lines . nil) default-frame-alist)
;; scroll bar false
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(horizontal-scroll-bars . nil) default-frame-alist)
;; 
(setq frame-inhibit-implied-resize t)

;; c-kで行全体を削除する
(setq kill-whole-line t)

;; 空白文字を表示する
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")

;; Ctrl + h をバックスペースに変える
(global-set-key "\C-h" `delete-backward-char)

;; quiet start
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)
(defun display-startup-echo-area-message ()
  (message ""))

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

;; カーソル行をハイライトする
(global-hl-line-mode t)

;; cu, cuh
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cuh\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))

;; line number
(if(version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode)
)

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

;; 環境を日本語、UTF-8にする
(setenv "LANG" "ja_JP.UTF-8")
(prefer-coding-system 'utf-8-unix)
(set-file-name-coding-system 'cp932)
(setq locale-coding-system 'utf-8-unix)
;; プロセスが出力する文字コードを判定して、process-coding-system の DECODING の設定値を決定する
(setq default-process-coding-system '(undecided-dos . utf-8-unix))

;; yes or no は y or n にする
(defalias 'yes-or-no-p #'y-or-n-p)

;; grep/find
(setq find-program "\"C:\\Program Files\\Git\\usr\\bin\\find.exe\""
      grep-program "\"C:\\Program Files\\Git\\usr\\bin\\grep.exe\""
      null-device "/dev/null")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 set color theme                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-theme 'modus-vivendi t)

