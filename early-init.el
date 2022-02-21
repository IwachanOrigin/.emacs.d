
;;============================================================================
;;                           early-init.el                                  ;;
;;============================================================================

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

;; カーソル行をハイライトする
(global-hl-line-mode t)

;; ガベージコレクションの実行頻度を下げる
(setq gc-cons-threshold 1073741824)
(setq garbage-collection-messages t)

;; emacsが利用されてから60s経っても入力がない場合はガベコレ
(run-with-idle-timer 60.0 t #'garbage-collect)

;; cu, cuh
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cuh\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))

;; line number
(if(version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode)
)
