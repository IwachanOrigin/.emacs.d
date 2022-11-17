
;;============================================================================
;;                           early-init.el                                  ;;
;;============================================================================

;; GC
(setq gc-cons-threshold most-positive-fixnum)
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
;; No implicit resizing
(setq frame-inhibit-implied-resize t)

;; Delete an entire line with c-k
(setq kill-whole-line t)

;; Displaying Blank Characters
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")
;; default tab-width 2
(setq-default tab-width 2)
;; Use spaces for tabs
(setq-default indent-tabs-mode nil)

;; Change "Ctrl + h" to backspace
(global-set-key "\C-h" `delete-backward-char)

;; quiet start
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)
(defun display-startup-echo-area-message ()
  (message ""))

;; No automatic save list file is created.
(setq auto-save-list-file-prefix nil)

;; Do not back up
(setq backup-inhibited t)

;; No autosave files are created.
(setq auto-save-default nil)
(setq make-backup-files nil)

;; Delete auto-save file on exit
(setq delete-auto-save-files t)

;; Turn off beep and flash
(setq ring-bell-function 'ignore)

;; Highlight cursor line
(global-hl-line-mode t)

;; cu, cuh
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cuh\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))

;; line number
(if(version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode))

;; column number
(column-number-mode)

;; Associate extensions to be used with ff-find-other-file
(setq cc-other-file-alist
  '(
   ("\\.c"   (".h"))
   ("\\.cpp"   (".h"))
   ("\\.h"   (".c"".cpp"))))

;; Set target directories to look for with ff-find-other-files
(setq ff-search-directories
  '("." "../src" "../include" "../main" "../*"))

;; Set ff-find-other-file to work with Meta+t
(global-set-key "\M-t" 'ff-find-other-file)

;; environment to Japanese, UTF-8
(setenv "LANG" "ja_JP.UTF-8")
(prefer-coding-system 'utf-8-unix)
(set-file-name-coding-system 'cp932)
(setq locale-coding-system 'utf-8-unix)
;; Determine the DECODING setting of process-coding-system by determining the character encoding output by the process.
(setq default-process-coding-system '(undecided-dos . utf-8-unix))

;; yes or no to y or n
(defalias 'yes-or-no-p #'y-or-n-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 set color theme                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-theme 'modus-vivendi t)

