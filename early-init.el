;;; Personal configuration -*- lexical-binding: t -*-

;;============================================================================
;;                           early-init.el                                  ;;
;;============================================================================

;; GC
(setq gc-cons-threshold most-positive-fixnum)
;; For Emacs 27+
(setq package-enable-at-startup nil
      package-quickstart nil)
;; Always load newest byte code
(setq load-prefer-newer t)

;; menu bar false
(push '(menu-bar-lines . nil) default-frame-alist)
;; tool bar false
(push '(tool-bar-lines . nil) default-frame-alist)
;; scroll bar false
(push '(scroll-bar-mode . nil) default-frame-alist)
;; vertical scroll bars false
(push '(vertical-scroll-bars . nil) default-frame-alist)
;; display fullscreen
(push '(fullscreen . maximized) default-frame-alist)
;; No implicit resizing
(setq frame-inhibit-implied-resize t)
;; No use file select dialog
(setq use-file-dialog nil)

;; yes or no to y or n
(if (version<= "27.0.00" emacs-version)
    (setq use-short-answers t)
  (defalias 'yes-or-no-p #'y-or-n-p))

;; quiet start
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
;; No use x resources
(setq inhibit-x-resources t)
(defun display-startup-echo-area-message ()
  (message ""))

;; Displaying Blank Characters
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")
;; Explicit end of buffer
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; default tab-width 2
(setq-default tab-width 2)
;; Use spaces for tabs
(setq-default indent-tabs-mode nil)

;; No automatic save list file is created.
(setq auto-save-list-file-prefix nil)
;; Do not back up
(setq backup-inhibited t)
;; No autosave files are created.
(setq auto-save-default nil)
(setq make-backup-files nil)
;; Delete auto-save file on exit
(setq delete-auto-save-files t)
;; Disable lockfiles. i.e. #foo.txt
(setq create-lockfiles nil)
;; Turn off beep and flash
(setq ring-bell-function 'ignore)

;; Highlight cursor line
(global-hl-line-mode t)

;; Automatically pair parentheses
(electric-pair-mode 1)

;; Ignore case when sorting
(custom-set-variables
 '(sort-fold-case t t))

;; Always add a newline automatically at the end of a file while saving
(setq-default require-final-newline t)

;; cu, cuh
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.cuh\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))

;; line number
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode t))

;; column number off
(column-number-mode nil)

;; Output custom setting to custom.el.
;; i.e. (custom-set-variables~~~ (custom-set-faces~~~~
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file t)

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
;; Change "Ctrl + h" to backspace
(global-set-key "\C-h" `delete-backward-char)
;; Delete an entire line with c-k
(setq kill-whole-line t)

;; Highlight matching pairs of parentheses.
(setq show-paren-delay 0)
(show-paren-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 set color theme                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; if emacs-version more than 28, load-theme is modus-vivendi.
;; if not emacs-version more than 28, load-theme is wonbat. 
(if (version<= "28.0.00" emacs-version)
    (load-theme 'modus-vivendi t)
  (load-theme 'wombat t))

;; Set scratch buffer screen
(setq initial-scratch-message
      ";;
;; Setup finished.
;;
")

(provide 'early-init)

