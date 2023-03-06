;;; Personal configuration -*- lexical-binding: t -*-

;;============================================================================
;;                                 init.el                                  ;;
;;============================================================================

;; profile
;;(require 'profiler)
;;(profiler-start 'cpu)

;; GC
(setq gc-cons-threshold most-positive-fixnum)
;; Run GC every 120 seconds if emacs is idle.
(run-with-idle-timer 120.0 t #'garbage-collect)
(add-hook 'emacs-startup-hook
  (lambda ()
  ;; recover default value
    (setq gc-cons-threshold 800000)))

;; magic file name
(defconst my-saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq file-name-handler-alist my-saved-file-name-handler-alist)))

;; package
(eval-and-compile
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

  ;; use-package
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)
    (package-install 'diminish))

  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)
  (setq use-package-compute-statistics t) ;; For "M-x use-package-report"

  (require 'use-package))
(require 'diminish) ;; if you use :diminish
(require 'bind-key) ;; if you use any :bind variant

;; A little transparent.
(set-frame-parameter (selected-frame) 'alpha '(0.90))

;; c/c++ mode
(add-hook 'c-mode-common-hook
 #'(lambda ()
     (c-set-style "linux")
     (setq indent-tabs-mode nil) ;; indent use space.
     (setq c-basic-offset 2) ;; basic indent value
     (setq tab-width 2)      ;; tab width
     (c-set-offset 'innamespace 0)
     ))

;; glsl-mode
(use-package glsl-mode
  :defer 5
  :config
  (add-to-list 'auto-mode-alist '("\.vsh$" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\.fsh$" . glsl-mode)))

;; editorconfig
(use-package editorconfig
  :defer 2
  :config
  (editorconfig-mode)
  (setq editorconfig-exec-path "~/.emacs.d/editorconfig"))

;; all-the-icons
(use-package all-the-icons
  :defer 0.1)

;; autorevert
;; Check for file updates and update buffers as well.
(use-package autorevert
  :defer 3
  :hook (after-init . global-auto-revert-mode))

;; hungry-delete
(use-package hungry-delete
  :defer 3
  :hook
  (after-init . global-hungry-delete-mode)
  :config
  (setq hungry-delete-chars-to-skip " \t\f\v"))

;; corfu
(use-package corfu
  :defer 1
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-scroll-margin 2)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :config
  (global-corfu-mode))

(use-package consult
  :defer 2)

;; vertico
(use-package vertico
  :defer 1
  :config
  (vertico-mode)
  :custom
  ;; Show more candidates
  (setq vertico-count 20))

;; marginalia
(use-package marginalia
  :defer 2
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :config
  (marginalia-mode))

;; orderless
(use-package orderless
  :defer 2
  :custom
  (completion-styles '(orderless)))

;; cape
(use-package cape
  :defer 2
  :config
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  (add-to-list 'completion-at-point-functions #'cape-symbol))

;; markdown
(use-package markdown-mode
  :defer 3
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.txt\\'" . gfm-mode))
  ;; need to installed "pandoc.exe" and set environment path for pandoc.exe.
  :config (setq markdown-command "pandoc.exe -s --standalone --metadata pagetitle=markdown -t html5 -c https://cdn.jsdelivr.net/npm/github-markdown-css@3.0.1/github-markdown.css"))

;; cmake-mode
(use-package cmake-mode
  :defer 3
  :config
  (setq auto-mode-alist (append '(("CMakeLists\\.txt\\'" . cmake-mode)) '(("\\.cmake\\'" . cmake-mode)) auto-mode-alist)))

;; shortcut key map of emacs
(defhydra hydra-shortcut-of-emacs (:hint nil)
  "
^
^shortcut-of-emacs(M-C は C-Mと同じ)
^
^Move^                            ^Select^                              ^Others^
^-----------------------------------------------------------------------------------------------
_M-<_: バッファの先頭へ移動    _C-x h_: 全選択                      _M-x replace-string_: 文字列置換
_M->_: バッファの末尾へ移動    _C-x SPC_: C-o > 空白挿入            _C-x C-r_: emacs restart
_M-f_: 次の単語へ移動                : C-t 文字列 > 文字列置換
_M-b_: 前の単語へ移動         _M-k_: 行を切り取り
_M-C-a_: 関数定義の先頭へ移動  _M-SPC_: 連続スペースを1つにまとめる
_M-C-e_: 関数定義の末尾へ移動  _M-C-h_: 関数単位で選択
_M-C-n_: 次の括弧終わりへ移動
_M-C-p_: 前の括弧始まりへ移動
"
  ; move
  ("M-<" beginning-of-buffer)
  ("M->" end-of-buffer)
  ("M-f" forward-word)
  ("M-b" backward-word)
  ("M-C-a" c-beginning-of-defun)
  ("M-C-e" c-end-of-defun)
  ("M-C-n" forward-list)
  ("M-C-p" backward-list)
  ; select
  ("C-x h" mark-whole-buffer)
  ("C-x SPC" rectangle-mark-mode)
  ("M-k" kill-sentence)
  ("M-SPC" just-one-space)
  ("M-C-h" c-mark-function)
  ; replace
  ("M-x replace-string" replace-string)
  ("C-x C-r" restart-emacs))

;; hydra
(use-package hydra
  :defer 2
  :bind ("C-c SPC" . hydra-shortcut-of-emacs/body))

;; restart-emacs
(use-package restart-emacs
  :defer 2
  :bind ("C-x C-r" . restart-emacs))

;; rst.el
(use-package rst
  :defer 1
  :load-path "~/.emacs.d/external/rst"
  :config
  (add-to-list 'auto-mode-alist '("\.rst$" . rst-mode))
  (add-to-list 'auto-mode-alist '("\.rest$" . rst-mode))
  (setq frame-background-mode 'dark)
  (add-hook 'rst-mode-hook #'(lambda() (setq indent-tabs-mode nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              Enhance C-s settings                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; swiper
(use-package swiper
  :defer 1
  :config
  (defun isearch-forward-or-swiper (use-swiper)
    (interactive "p")
    (let (current-prefix-arg)
      (call-interactively (if use-swiper 'swiper 'isearch-forward))))
  (global-set-key (kbd "C-s") 'isearch-forward-or-swiper))

;; migemo
;; This package can use the Roman alphabet to search  the japanese language.
;; We need to install cmigemo for Windows [migemo-kaoriya-64](https://www.kaoriya.net/software/cmigemo/)
;; Please add path cmigemo.exe.
(use-package migemo
  :defer 1
  :config
  ;; use to C/Migemo
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs" "-i" "\a"))
  ;; The following description is the art of treating relative paths as absolute paths
  ;; (expand-file-name "~/.emacs.d/init.el")
  ;; dictionary path
  (setq migemo-dictionary (expand-file-name "~/.emacs.d/cmigemo-default-win64/dict/cp932/migemo-dict"))
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  ;; charset encoding
  ;;(setq migemo-coding-system 'utf-8-unix)
  (setq migemo-coding-system 'cp932-unix)
)

;; ivy-migemo
;; Make migemo available for ivy-based search
(use-package ivy-migemo
  :defer 1
  :config
  ;; toggle migemo
  (define-key ivy-minibuffer-map (kbd "M-m") #'ivy-migemo-toggle-migemo)
  ;; If you want to defaultly use migemo on swiper and counsel-find-file:
  (setq ivy-re-builders-alist '((t . ivy--regex-plus)
                                   (swiper . ivy-migemo--regex-plus)
                                   (counsel-find-file . ivy-migemo--regex-plus))))

;; profile
;;(profiler-report)
;;(profiler-stop)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(corfu all-the-icons centaur-tabs cmake-mode company counsel dap-mode dashboard diminish dimmer doom-modeline editorconfig eglot flycheck-posframe glsl-mode hungry-delete ivy-migemo lsp-ui rainbow-delimiters restart-emacs use-package which-key)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
