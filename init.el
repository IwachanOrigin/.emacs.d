
;; -*- lexical-binding: t -*-

;;============================================================================
;;                                 init.el                                  ;;
;;============================================================================

;; profile
;;(require 'profiler)
;;(profiler-start 'cpu)

;; Kind of OS determine
(defconst IS-MAC (eq system-type 'darwin))
(defconst IS-LINUX (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

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

;; environment to Japanese, UTF-8
(setenv "LANG" "ja_JP.UTF-8")
(prefer-coding-system 'utf-8-unix)
(set-file-name-coding-system 'cp932)
(setq locale-coding-system 'utf-8-unix)
;; Determine the DECODING setting of process-coding-system by determining the character encoding output by the process.
(when IS-WINDOWS
  (setq-default default-process-coding-system '(utf-8-unix . japanese-cp932-dos)))

;; package
(eval-and-compile
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

  ;; set package-archives priority
  (setq package-archive-priorities
        '(("melpa-stable" . 10)
          ("melpa" . 5)
          ("gnu" . 5)))

  (setq package-install-upgrade-built-in t)
  (setq package-native-compile t)

  ;; use-package
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)
    (package-install 'diminish))

  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)
  (setq use-package-compute-statistics t) ;; For "M-x use-package-report"

  (require 'use-package)
)

;; all-the-icons
(use-package all-the-icons
  :defer 0.01
  :if (display-graphic-p)
  :config
  ;; Use 'prepend for the NS and Mac ports or Emacs will crash.
  (when (member "all-the-icons" (font-family-list))
    (set-fontset-font t 'unicode (font-spec :family "all-the-icons") nil 'append))
  (when (member "file-icons" (font-family-list))
    (set-fontset-font t 'unicode (font-spec :family "file-icons") nil 'append))
  (when (member "FontAwesome" (font-family-list))
    (set-fontset-font t 'unicode (font-spec :family "FontAwesome") nil 'append))
  (when (member "Material Icons" (font-family-list))
    (set-fontset-font t 'unicode (font-spec :family "Material Icons") nil 'append))
  (when (member "github-octicons" (font-family-list))
    (set-fontset-font t 'unicode (font-spec :family "github-octicons") nil 'append))
  (when (member "Weather Icons" (font-family-list))
    (set-fontset-font t 'unicode (font-spec :family "Weather Icons") nil 'append)))

;; useful to IME
(when (eq window-system 'w32)
  (use-package tr-ime
    :defer 0.01
    :config
    (tr-ime-standard-install)
    (setq default-input-method "W32-IME")
    (w32-ime-initialize)
    ;; IME のモードライン表示設定
    (setq-default w32-ime-mode-line-state-indicator "[--]")
    (setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
    ;; IME Init
    (w32-ime-initialize)
    ;; IME Control (Turn off IME when typing yes/no, etc)
    (wrap-function-to-control-ime 'universal-argument t nil)
    (wrap-function-to-control-ime 'read-string nil nil)
    (wrap-function-to-control-ime 'read-char nil nil)
    (wrap-function-to-control-ime 'read-from-minibuffer nil nil)
    (wrap-function-to-control-ime 'y-or-n-p nil nil)
    (wrap-function-to-control-ime 'yes-or-no-p nil nil)
    (wrap-function-to-control-ime 'map-y-or-n-p nil nil)
    (wrap-function-to-control-ime 'register-read-with-preview nil nil)))

;; dashboard
(use-package dashboard
  :defer 0.02
  :config
  (setq dashboard-banner-logo-title "Welcome to EmacStraylight Dashboard")
  (setq dashboard-startup-banner (cons "~/.emacs.d/logo/straylight_mark.png" "~/.emacs.d/logo/emacstraylight.txt"))
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-icon-type 'all-the-icons)
  (dashboard-setup-startup-hook)
  (dashboard-refresh-buffer))

;; Corfu
(use-package corfu
  :ensure t
  :defer 3
  :custom
  (corfu-cycle t)                ;; 候補リストをループする
  (corfu-auto t)                 ;; 自動的に候補を表示
  (corfu-auto-prefix 2)          ;; 2文字以上入力で補完を開始
  (corfu-auto-delay 0)           ;; 補完の遅延時間（0秒）
  (completion-ignore-case t)     ;; 大文字小文字を無視する
  :bind
  (:map corfu-map
        ("C-n" . corfu-next)     ;; C-nで次の候補を選択
        ("C-p" . corfu-previous) ;; C-pで前の候補を選択
        ("C-s" . corfu-info-documentation) ;; C-sで候補の絞り込み
        ("C-i" . corfu-complete) ;; C-iまたはTABで候補を確定
        ([tab] . corfu-complete))
  :init
  (global-corfu-mode))

;; Orderless
(use-package orderless
  :ensure t
  :defer 2)

;; Support corfu popup
(use-package corfu-popupinfo
  :after corfu
  :ensure nil ;; `corfu-popupinfo` was part of `corfu` package
  :hook (corfu-mode . corfu-popupinfo-mode)
  :config
  (setq corfu-popupinfo-delay 0.2)) ;; popup delay time

;; font settings
(when (member "UDEV Gothic NF" (font-family-list))
  (set-fontset-font t 'unicode (font-spec :family "UDEV Gothic NF") nil 'append)
  (set-face-attribute 'default nil :family "UDEV Gothic NF" :height 110 :weight 'Regular))

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

;; counsel
;(use-package counsel
;  :defer 2
;  :bind
;  ("M-x" . counsel-M-x)
;  ("C-x C-f" . counsel-find-file)
;  ("C-x C-r" . counsel-recentf)
;  ("C-x b" . counsel-switch-buffer))

;; dired-sidebar
(use-package dired-sidebar
  :defer 1
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  (setq dired-sidebar-theme 'ascii)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-resize-on-open t)
  :bind
  (("C-x C-n" . dired-sidebar-toggle-sidebar)))

;;
;; programing language config
;;

;; c/c++ mode
(use-package cc-mode
  :defer 1
  :config
  (setq c-default-style "bsd")
  (setq c-basic-offset 2) ;; basic indent value
  (setq tab-width 2)      ;; tab width
  (setq indent-tabs-mode nil)  ;; indent use space.
  (c-set-offset 'innamespace 0) ;; namespace indent pos is 0
  )

;; js mode
;(add-hook 'js-mode-hook
; #'(lambda ()
;     (make-local-variable 'js-indent-level)
;     (setq js-indent-level 2)))

;; glsl-mode
;(use-package glsl-mode
;  :defer 5
;  :config
;  (add-to-list 'auto-mode-alist '("\.vsh$" . glsl-mode))
;  (add-to-list 'auto-mode-alist '("\.fsh$" . glsl-mode)))

;; markdown
(use-package markdown-mode
  :defer 3
  :mode ("\\.md\\'" . gfm-mode)
  ;; need to installed "pandoc.exe" and set environment path for pandoc.exe.
  :config
  (when (eq system-type 'windows-nt)
    (setq markdown-command "pandoc.exe -s --standalone --metadata pagetitle=markdown -t html5 -c https://cdn.jsdelivr.net/npm/github-markdown-css@3.0.1/github-markdown.css"))
  (unless (eq system-type 'windows-nt)
    (setq markdown-command "pandoc -s --standalone --metadata pagetitle=markdown -t html5 -c https://cdn.jsdelivr.net/npm/github-markdown-css@3.0.1/github-markdown.css"))
  (setq markdown-fontify-code-blocks-natively t))

;; editorconfig
(use-package editorconfig
  :defer 2
  :config
  (editorconfig-mode)
  (setq editorconfig-exec-path "~/.emacs.d/editorconfig/.editorconfig"))

;; eglot
(progn
  (customize-set-variable 'eglot-autoshutdown t)
  (customize-set-variable 'eglot-extend-to-xref t)
  (customize-set-variable 'eglot-ignored-server-capabilities
    (quote (:documentFormattingProvider :documentRangeFormattingProvider)))

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
       '((c-mode c++-mode)
         . ("clangd"
            "-j=8"
            "--log=error"
            "--background-index"
            "--clang-tidy"
            "--cross-file-rename"
            "--completion-style=detailed"
            "--pch-storage=memory"
            "--header-insertion=never"
            "--header-insertion-decorators=0"))))

  (with-eval-after-load 'flymake
    (define-key flymake-mode-map (kbd "C-c ! n") nil)
    (define-key flymake-mode-map (kbd "C-c ! p") nil)
    (define-key flymake-mode-map (kbd "C-c n") 'flymake-goto-next-error)
    (define-key flymake-mode-map (kbd "C-c p") 'flymake-goto-prev-error))

  (add-hook 'c-mode-hook #'eglot-ensure)
  (add-hook 'c++-mode-hook #'eglot-ensure)
  (add-hook 'c-ts-mode-hook
            (lambda ()
              (eglot-ensure)
              (c-ts-mode-set-global-style 'bsd)))
  (add-hook 'c++-ts-mode-hook
            (lambda ()
              (eglot-ensure)
              (c-ts-mode-set-global-style 'bsd))))

;; tree-sitter
(when (and (fboundp 'treesit-available-p)
           (treesit-available-p))
  (setq treesit-font-lock-level 4))
(use-package treesit-auto
  :ensure t
  :defer 2
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;
;; Custom functions
;;

;; need to pandoc, latex, eisvogel.latex
;; eisvogel.latex => https://github.com/enhuiz/eisvogel
(defun pandoc-markdown-slides-pdf ()
  "create beamer slides."
  (interactive)
  (setq infilename (buffer-file-name))
  (setq outfilename (replace-regexp-in-string ".md" ".pdf" infilename))
  (when (eq system-type 'windows-nt)
    (setq cmd-str (concat "pandoc.exe " infilename " -o " outfilename " --from markdown --to beamer --template eisvogel.latex --listings --pdf-engine \"xelatex\" -V CJKmainfont=\"Meiryo UI\"")))
  (unless (eq system-type 'windows-nt)
    (setq cmd-str (concat "pandoc " infilename " -o " outfilename " --from markdown --to beamer --template eisvogel.latex --listings --pdf-engine \"xelatex\" -V CJKmainfont=\"Noto Sans CJK JP\"")))
  (shell-command-to-string cmd-str))
(global-set-key (kbd "C-x C-l") 'pandoc-markdown-slides-pdf)

;; Generate buffer to pdf use pandoc.
(defun pandoc-buffer-pdf ()
  "create buffer to pdf."
  (interactive)
  (let* ((buffer-content (buffer-string))
         (tempfile (make-temp-file "pandoc-buffer" nil ".md"))
         (outfilename (concat (file-name-sans-extension tempfile) ".pdf"))
         (cmd-str (if (eq system-type 'windows-nt)
                      (format "pandoc.exe \"%s\" -o \"%s\" --pdf-engine=xelatex -V documentclass=bxjsarticle -V classoption=pandoc" tempfile outfilename)
                    (format "pandoc.exe \"%s\" -o \"%s\" --pdf-engine=xelatex -V documentclass=bxjsarticle -V classoption=pandoc" tempfile outfilename))))
    (with-temp-file tempfile
      (insert buffer-content))
    (shell-command-to-string cmd-str)
    (message "PDF created: %s" outfilename)))

;; display-fill-column-indicator-mode toggle
(defun toggle-display-fill-column-indicator-mode ()
  "toggle display-fill-column-indicator-mode"
  (interactive)
  (cond (display-fill-column-indicator-mode (display-fill-column-indicator-mode -1))
        (t (display-fill-column-indicator-mode 1))))
(global-set-key (kbd "C-c h") 'toggle-display-fill-column-indicator-mode)

;;
;; Enhance C-s settings
;;

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
  ;; dictionary path and charset encoding
  (when (eq system-type 'windows-nt)
    (setq migemo-dictionary (expand-file-name "~/.emacs.d/cmigemo-default-win64/dict/cp932/migemo-dict"))
    (setq migemo-coding-system 'cp932-unix))
  (unless (eq system-type 'windows-nt)
    (setq migemo-dictionary (expand-file-name "~/.emacs.d/cmigemo-default-win64/dict/utf-8/migemo-dict"))
    (setq migemo-coding-system 'utf-8-unix))
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil))

;; ivy-migemo
;; Make migemo available for ivy-based search
;(use-package ivy-migemo
;  :defer 1
;  :config
;  ;; toggle migemo
;  (define-key ivy-minibuffer-map (kbd "M-m") #'ivy-migemo-toggle-migemo)
;  ;; If you want to defaultly use migemo on swiper and counsel-find-file:
;  (setq ivy-re-builders-alist '((t . ivy--regex-plus)
;                                   (swiper . ivy-migemo--regex-plus)
;                                   (counsel-find-file . ivy-migemo--regex-plus))))
;
;;; swiper
;(use-package swiper
;  :defer 1
;  :config
;  (defun isearch-forward-or-swiper (use-swiper)
;    (interactive "p")
;    (let (current-prefix-arg)
;      (call-interactively (if use-swiper 'swiper 'isearch-forward))))
;  ;(global-set-key (kbd "C-s") 'isearch-forward-or-swiper)
;  )

;; Vertico
(use-package vertico
  :ensure t
  :defer 2
  :init
  (vertico-mode)
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :custom
  (vertico-cycle t)) ;; 候補リストを循環する

;; Consult
(use-package consult
  :ensure t
  :defer 2
  :bind
  (("C-c M-x" . consult-mode-command)
   ("C-x C-r" . consult-recent-file)
   ("C-x b" . consult-buffer)
   ("C-s" . consult-line)  ;; Swiperの代替として
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   )
  :config
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  ;; M-y (yank-pop) でconsult-yank-from-kill-ringを使用
  (advice-add #'yank-pop :override #'consult-yank-pop))

;; find-fileでプレビュー
(setq read-file-name-function #'consult-find-file-with-preview)
(defun consult-find-file-with-preview (prompt &optional dir default mustmatch initial pred)
  (interactive)
  (let ((default-directory (or dir default-directory))
        (minibuffer-completing-file-name t))
    (consult--read #'read-file-name-internal :state (consult--file-preview)
                   :prompt prompt
                   :initial initial
                   :require-match mustmatch
                   :predicate pred)))

;; Marginalia
(use-package marginalia
  :ensure t
  :defer 2
  :init
  (marginalia-mode))

;; Use orderless. Config is top.

;; Embark
(use-package embark
  :ensure t
  :defer 2
  :bind
  (("C-'" . embark-act)         ;; コンテキストメニュー
   ("C-;" . embark-dwim)        ;; 項目を実行
   ("C-x B" . embark-bindings)) ;; Embarkのキーバインド一覧
  :init
  ;; Embarkからの非推奨のミニバッファインターフェイスのサポートを無効化
  (setq prefix-help-command #'embark-prefix-help-command))

;; EmbarkとConsultの統合
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; 保存されたコマンドの履歴を使うための設定
(use-package savehist
  :init
  (savehist-mode))

;; Setting completion style
(use-package emacs
  :init
  (setq completion-styles '(orderless basic) ;; orderlessスタイルを使用
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))
        read-extended-command-predicate #'command-completion-default-include-p
        enable-recursive-minibuffers t
        completion-in-region-function #'consult-completion-in-region))

;;
;; Hydra config
;;

;; helper func to hydra menu
(defun my/hydra-disable-dimmer ()
  (when (bound-and-true-p dimmer-mode)
    (dimmer-mode -1)))

(defun my/hydra-enable-dimmer ()
  (unless (bound-and-true-p dimmer-mode)
    (dimmer-mode 1)))

;; hydra
(use-package hydra
  :defer 2
  :bind ("C-c SPC" . hydra-shortcut-of-emacs/body))

;; shortcut key map of emacs
(defhydra hydra-shortcut-of-emacs (:hint nil
                                   :pre (my/hydra-disable-dimmer)
                                   :post (my/hydra-enable-dimmer))
  "
^
^shortcut-of-emacs(M-C は C-Mと同じ)
^
^Move^                            ^Select^                              ^Others^
^-----------------------------------------------------------------------------------------------
_M-<_: バッファの先頭へ移動    _C-x h_: 全選択                      _M-x replace-string_: 文字列置換
_M->_: バッファの末尾へ移動    _C-x SPC_: C-o > 空白挿入            _C-x r_: emacs restart
_M-f_: 次の単語へ移動                : C-t 文字列 > 文字列置換     _M-x sort-lines_: 選択領域の並び替え
_M-b_: 前の単語へ移動         _M-k_: 行を切り取り                   _M-<f10>_: fullscreen/default
_M-C-a_: 関数定義の先頭へ移動  _M-SPC_: 連続スペースを1つにまとめる   _C-x x t_: toggle-truncate-lines
_M-C-e_: 関数定義の末尾へ移動  _M-C-h_: 関数単位で選択               _C-c n_: flymake next error
_M-C-n_: 次の括弧終わりへ移動  _C-x C-r_: Recentfの起動             _C-c p_: flymake prev error
_M-C-p_: 前の括弧始まりへ移動                                       _C-x C-n_: dired-sidebar-toggle-sidebar
                                                             _C-x C-l_: pandoc-markdown-pdf
                                                             _C-c h_: toggle-display-fill-column-indicator-mode
"
  ; Move
  ("M-<" beginning-of-buffer)
  ("M->" end-of-buffer)
  ("M-f" forward-word)
  ("M-b" backward-word)
  ("M-C-a" c-beginning-of-defun)
  ("M-C-e" c-end-of-defun)
  ("M-C-n" forward-list)
  ("M-C-p" backward-list)
  ; Select
  ("C-x h" mark-whole-buffer)
  ("C-x SPC" rectangle-mark-mode)
  ("M-k" kill-sentence)
  ("M-SPC" just-one-space)
  ("M-C-h" c-mark-function)
  ("C-x C-r" recentf-open-files)
  ; Others
  ("M-x replace-string" replace-string)
  ("C-x r" restart-emacs)
  ("M-x sort-lines" sort-lines)
  ("M-<f10>" toggle-frame-maximized)
  ("C-x x t" toggle-truncate-lines)
  ("C-c n" flymake-goto-next-error)
  ("C-c p" flymake-goto-prev-error)
  ("C-x C-n" dired-sidebar-toggle-sidebar)
  ("C-x C-l" pandoc-markdown-pdf)
  ("C-c h" toggle-display-fill-column-indicator-mode))

;;
;; server configuration for emacsclient
;;

(when (eq system-type 'windows-nt)
  (use-package server
    :defer 0.01
    :config (server-start)
    ;; Assign kill buffer to C-x C-c
    (global-set-key (kbd "C-x C-c") 'kill-this-buffer)
    ;; Allow Emacs to exit with M-x exit
    (defalias 'exit 'save-buffers-kill-emacs)
    ;; yes/no query on exit
    (setq confirm-kill-emacs 'yes-or-no-p)))

;; restart-emacs
(use-package restart-emacs
  :defer 2
  :bind ("C-x r" . restart-emacs))

;; rst.el
(use-package rst
  :defer 2
  :load-path "~/.emacs.d/external/rst"
  :config
  (add-to-list 'auto-mode-alist '("\.rst$" . rst-mode))
  (add-to-list 'auto-mode-alist '("\.rest$" . rst-mode))
  (setq frame-background-mode 'dark)
  (add-hook 'rst-mode-hook #'(lambda() (setq indent-tabs-mode nil))))

;; hlsl-mode.el
(use-package hlsl-mode
  :defer 5
  :load-path "~/.emacs.d/external/hlsl"
  :config
  (add-to-list 'auto-mode-alist '("\.fx$" . hlsl-mode))
  (add-to-list 'auto-mode-alist '("\.fxh$" . hlsl-mode))
  (add-to-list 'auto-mode-alist '("\.hlsl$" . hlsl-mode))
  (setq frame-background-mode 'dark)
  (add-hook 'hlsl-mode-hook #'(lambda() (setq indent-tabs-mode nil))))

;;
;; Centaur-tabs config
;;

;; centaur-tabs
(use-package centaur-tabs
  :defer 0.03
  :demand
  :config
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-height 32)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-set-bar 'over)
  (setq centaur-tabs-set-modified-marker t)
  (setq centaur-tabs-modified-marker " **")
  (setq centaur-tabs-set-close-button t)
  (setq centaur-tabs-close-button " ×")
  (setq centaur-tabs-label-fixed-length 40)
  (when (member "UDEV Gothic NF" (font-family-list))
    (centaur-tabs-change-fonts "UDEV Gothic NF" 100))
  (centaur-tabs-headline-match)
  (centaur-tabs-mode t)
  :bind
  ("C-," . centaur-tabs-backward)
  ("C-." . centaur-tabs-forward))

;; Re-implementation of centaur-tabs-buffer-tab-label.
;; Omit the middle of long filenames, as in Visual Studio.
(defun my/centaur-tabs-buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar, truncating the middle if it's too long."
  ;; Render tab.
  (format " %s"
          (let* ((bufname (if centaur-tabs--buffer-show-groups
                              (centaur-tabs-tab-tabset tab)
                            (buffer-name (car tab))))
                 (maxlen centaur-tabs-label-fixed-length))
            (if (> (length bufname) maxlen)
                (let ((start (substring bufname 0 (/ maxlen 3)))
                      (end (substring bufname (- (length bufname) (/ maxlen 3)))))
                  (concat start "..." end))
              bufname))))

;; Set up your own function after centaur-tabs loading is complete.
(with-eval-after-load 'centaur-tabs
  (setq centaur-tabs-tab-label-function #'my/centaur-tabs-buffer-tab-label))

;; Vertical partitioning is preferred over horizontal partitioning
(setq split-width-threshold 160)
(setq split-height-threshold nil)

;; Display a bar that clearly indicates the number of characters per line
(setq-default display-fill-column-indicator-column 100)
(global-display-fill-column-indicator-mode)

;; A little transparent.
(set-frame-parameter (selected-frame) 'alpha '(0.85))

;; profile
;;(profiler-report)
;;(profiler-stop)

(provide 'init)

