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
(setq gc-cons-percentage 0.2
      gc-cons-threshold (* 128 1024 1024))
(add-hook 'focus-out-hook #'garbage-collect)
(setq garbage-collection-messages t)

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

;; native-compの警告を表示しない
(setq native-comp-async-report-warnings-errors 'silent)

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

  (require 'use-package))

;; vc-use-package
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

;; so-long
(use-package so-long
  :init
  (global-so-long-mode +1))

;; Optimizing performance
;; https://ayatakesi.github.io/lispref/25.2/html/Output-from-Processes.html
(setq process-adaptive-read-buffering t)
;; protesilaos
;; 閉じ括弧を入力しても点滅させない
(setq blink-matching-paren nil)
;; vcのバックエンドをGitのみに変更
(setq vc-handled-backends '(Git))
;; doomemacs
;; ファイル検索を2回行わないようにする
(setq auto-mode-case-fold nil)
;; 双方向の並び替えを抑制する
(setq-default bidi-display-reordering 'left-to-right)
;; 長い行の双方向スキャン
(setq bidi-inhibit-bpa t)
;; フォーカスされていないウィンドウのカーソルを削除
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
;; 高速なスクロール
(setq fast-but-imprecise-scrolling t)
;; ドメインにpingを送信しない
(setq ffap-machine-p-known 'reject)
;; UIの更新頻度を下げる
(setq idle-update-delay 1.0)
;; 不要なフォント表示化を抑制
(setq redisplay-skip-fontification-on-input t)
;; Windowsの最適化
(when IS-WINDOWS
  (setq w32-get-true-file-attributes nil
        w32-pipe-read-delay 0
        w32-pipe-buffer-size (* 64 1024)))
;; Centaur Emacs
;; 各OSの最適化
(when IS-WINDOWS
  (setq w32-use-native-image-API t))
(unless IS-MAC
  (setq command-line-ns-option-alist nil))
(unless IS-LINUX
  (setq command-line-x-option-alist nil))

;; Org
(use-package org
  :init
  (setq org-return-follows-link t  ; Returnキーでリンク先を開く
        org-mouse-1-follows-link t ; マウスクリックでリンク先を開く
        ))
;; アンダースコアを入力しても下付き文字にならないようにする
(setq org-use-sub-superscripts '{}
      org-export-with-sub-superscripts nil)

;; org-indent
(use-package org-indent
  :after org
  :ensure nil
  :hook (org-mode . org-indent-mode))

;; org-modern
(use-package org-modern
  :after org
  :config
  (setopt
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")

  ;; Ellipsis styling
  (setopt org-ellipsis "…")
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)

  (global-org-modern-mode))

;; org-modern-indent
(use-package org-modern-indent
  :after org
  :vc ( :fetcher github :repo "jdtsmith/org-modern-indent")
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

;; fontaine
(use-package fontaine
  :config
  (cond (IS-LINUX
         (setq fontaine-presets
               '((regular
                  :default-family "CommitMono Nerd Font Mono"
                  :fixed-pitch-family "CommitMono Nerd Font Mono"
                  :variable-pitch-family "CommitMono Nerd Font Mono"
                  :italic-family "CommitMono Nerd Font Mono Italic")
                 (large
                  :default-family "CommitMono Nerd Font Mono"
                  :variable-pitch-family "CommitMono Nerd Font Mono"))))

        (IS-WINDOWS
         (setq fontaine-presets
               '((regular
                  :default-family "CommitMono Nerd Font Mono"
                  :fixed-pitch-family "CommitMono Nerd Font Mono"
                  :variable-pitch-family "CommitMono Nerd Font Mono"
                  :italic-family "CommitMono Nerd Font Mono")
                 (large
                  :default-family "CommitMono Nerd Font Mono"
                  :variable-pitch-family "CommitMono Nerd Font Mono")))))

  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset))

;; icons
(use-package nerd-icons)
(use-package nerd-icons-completion
  :hook (after-init . nerd-icons-completion-mode))
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))
(use-package nerd-icons-corfu
  :vc ( :fetcher github :repo "LuigiPiucco/nerd-icons-corfu")
  :after corfu nerd-icons
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; useful to IME
(when (eq window-system 'w32)
  (use-package tr-ime
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
  :config
  (setq dashboard-banner-logo-title "Welcome to EmacStraylight Dashboard")
  (setq dashboard-startup-banner (cons "~/.emacs.d/logo/straylight_mark.png" "~/.emacs.d/logo/emacstraylight.txt"))
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-icon-type 'all-the-icons)
  (dashboard-setup-startup-hook)
  (dashboard-refresh-buffer))

;; corfu
(use-package corfu
  :demand t
  :hook (prog-mode . (lambda ()
                       (global-set-key [remap c-indent-line-or-region] #'indent-for-tab-command)))
  :config
  (setopt corfu-cycle t
          corfu-auto t
          corfu-auto-delay 0.0
          corfu-auto-prefix 2
          corfu-on-exact-match 'show)

  (global-corfu-mode +1)

  (with-eval-after-load 'lsp-mode
    (setopt lsp-completion-provider :none))

  (with-eval-after-load 'orderless
    (defun my/orderless-for-corfu ()
      (setq-local orderless-matching-styles '(orderless-flex)))

    (add-hook 'corfu-mode-hook #'my/orderless-for-corfu)))
;; corfu-popup
(use-package corfu-popupinfo
  :ensure nil
  :hook (corfu-mode . corfu-popupinfo-mode))
;; corfu-magic
(with-eval-after-load 'corfu
  (setq corfu-preselect 'prompt)

  (define-key corfu-map (kbd "TAB") 'corfu-next)
  (define-key corfu-map (kbd "<tab>") 'corfu-next)
  (define-key corfu-map (kbd "S-TAB") 'corfu-previous)
  (define-key corfu-map (kbd "<backtab>") 'corfu-previous)

  (defvar corfu--index)
  (defvar corfu-magic-insert-or-next-line
    `(menu-item "" nil :filter ,(lambda (&optional _)
                                  (when (>= corfu--index 0)
                                    'corfu-insert)))
    "If we made a selection during `corfu' completion, select it.")
  (define-key corfu-map (kbd "RET") corfu-magic-insert-or-next-line)

  (defvar corfu-magic-cancel-or-backspace
    `(menu-item "" nil :filter ,(lambda (&optional _)
                                  (when (>= corfu--index 0)
                                    'corfu-reset)))
    "If we made a selection during `corfu' completion, cancel it.")
  (define-key corfu-map (kbd "DEL") corfu-magic-cancel-or-backspace)
  (define-key corfu-map (kbd "<backspace") corfu-magic-cancel-or-backspace)
  )

;; cape
(use-package cape
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add 'lsp-completion-at-point :around #'cape-wrap-buster)
  (advice-add 'lsp-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add 'lsp-completion-at-point :around #'cape-wrap-noninterruptible)

  (add-hook 'completion-at-point-functions #'tempel-complete)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))
;; dabbrevのサイズを制限
(setq dabbrev-friend-buffer-function (lambda (other-buffer)
                                       (< (buffer-size other-buffer) (* 1024 1024))))

;; TABで補完を表示する
(setq tab-always-indent 'complete)

;; vertico
(use-package vertico
  :init
  (setq vertico-cycle t)
  (vertico-mode +1))
;; vertico-repeat
(use-package vertico-repeat
  :ensure nil
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save))
;; vertico-directory
(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind ( :map vertico-map
          ("<backspace>" . vertico-directory-delete-char)))
;; vertico-buffer
(use-package vertico-buffer
  :ensure nil
  :config
  (setq vertico-buffer-display-action '(display-buffer-at-bottom))
  (vertico-buffer-mode +1))
;; Prefix current candidate with arrow
(defvar +vertico-current-arrow t)

(cl-defmethod vertico--format-candidate :around
  (cand prefix suffix index start &context ((and +vertico-current-arrow
                                                 (not (bound-and-true-p vertico-flat-mode)))
                                            (eql t)))
  (setq cand (cl-call-next-method cand prefix suffix index start))
  (let ((arrow (nerd-icons-faicon "nf-fa-hand_o_right")))
    (if (bound-and-true-p vertico-grid-mode)
        (if (= vertico--index index)
            (concat arrow " " cand)
          (concat #("_" 0 1 (display " ")) cand))
      (if (= vertico--index index)
          (concat " " arrow " " cand)
        (concat "    " cand)))))
;; vertico-truncate
(use-package vertico-truncate
  :after vertico
  :vc ( :fetcher github :repo "jdtsmith/vertico-truncate")
  :config
  (vertico-truncate-mode +1))

;; orderless
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides nil)

  (with-eval-after-load 'migemo
    ;; orderlessをmigemo対応
    (defun orderless-migemo (component)
      (let ((pattern (downcase (migemo-get-pattern component))))
        (condition-case nil
            (progn (string-match-p pattern "") pattern)
          (invalid-regexp nil))))
    (add-to-list 'orderless-matching-styles 'orderless-migemo))

  (with-eval-after-load 'corfu
    (defun orderless-fast-dispatch (word index total)
      (and (= index 0) (= total 1) (length< word 4)
           'orderless-literal-prefix))

    (orderless-define-completion-style orderless-fast
      (orderless-style-dispatchers '(orderless-fast-dispatch))
      (orderless-matching-styles '(orderless-flex)))

    (defun my/setup-corfu-for-orderless ()
      (setq-local corfu-auto-delay 0
                  corfu-auto-prefix 1
                  completion-styles '(orderless-fast)))

    (add-hook 'corfu-mode-hook #'my/setup-corfu-for-orderless)))

;; prescient
(use-package prescient
  :config
  (setq prescient-aggressive-file-save t)
  (prescient-persist-mode +1))
;; vertico-prescient
(use-package vertico-prescient
  after: vertico prescient
  :config
  (setq vertico-prescient-enable-filtering nil)
  (vertico-prescient-mode +1))
;; corfu-prescient
(use-package corfu-prescient
  :after corfu prescient
  :config
  (setq corfu-prescient-enable-filtering nil)
  (corfu-prescient-mode +1))

;; consult
;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ([remap switch-to-buffer] . consult-buffer)                ;; orig. switch-to-buffer
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ([remap bookmark-jump] . consult-bookmark)            ;; orig. bookmark-jump
         ([remap project-switch-to-buffer] . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         :map goto-map
         ("e" . consult-compile-error)
         ("f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("g" . consult-goto-line)             ;; orig. goto-line
         ("M-g" . consult-goto-line)           ;; orig. goto-line
         ("o" . consult-outline)               ;; Alternative: consult-org-heading
         ("m" . consult-mark)
         ("k" . consult-global-mark)
         ("i" . consult-imenu)
         ("I" . consult-imenu-multi)
         :map search-map
         ("d" . consult-fd)
         ("D" . consult-locate)
         ("g" . consult-grep)
         ("G" . consult-git-grep)
         ("r" . consult-ripgrep)
         ("l" . consult-line)
         ("L" . consult-line-multi)
         ("m" . consult-multi-occsur)
         ("k" . consult-keep-lines)
         ("u" . consult-focus-lines)
         ("e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-hisstory)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 1.0 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
    ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
    ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
    ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
    ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  )

;; marginalia
(use-package marginalia
  :init
  (marginalia-mode +1))

;; embark
(use-package embark
  :bind (("C-." . embark-act)         ;; pick some comfortable binding
         ("C-;" . embark-dwim)        ;; good alternative: M-.
         )
  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
;; embark-consult
(use-package embark-consult
  :after embark consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; tempel
(use-package tempel
  :defer 0.01
  :demand t
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert)))
;; tempel-collection
(use-package tempel-collection
  :after tempel)

;; git
;; magit
(use-package magit
  :config
  (when IS-WINDOWS
    (setq magit-refresh-status-buffer nil)
    (setq auto-revert-buffer-list-filter
          'magit-auto-revert-repository-buffer-p)
    (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)
    (remove-hook 'server-switch-hook 'magit-commit-diff)
    (remove-hook 'with-editor-filter-visit-hook 'magit-commit-diff)))

;; diff-hl
(use-package diff-hl
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode))
  :init
  (global-diff-hl-mode +1)
  (global-diff-hl-show-hunk-mouse-mode +1)
  (diff-hl-margin-mode +1))
;; difftastic.el
(use-package difftastic
  :demand t
  :bind (:map magit-blame-read-only-mode-map
              ("D" . difftastic-magit-show)
              ("S" . difftastic-magit-show))
  :config
  (eval-after-load 'magit-diff
    '(transient-append-suffix 'magit-diff '(-1 -1)
       [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
        ("S" "Difftastic show" difftastic-magit-show)])))

;;whick-key
(use-package which-key
  :config
  (which-key-mode +1))

;; undo
;; undo-fu
(use-package undo-fu)
;; undo-fu-session
(use-package undo-fu-session
  :config
  (undo-fu-session-global-mode +1))
;; vundo
;; https://github.com/casouri/vundo
(use-package vundo)

;; rg
(use-package rg
  :defer t
  )

;; apheleia
(use-package apheleia
  :config
  (when IS-WINDOWS
    (add-to-list 'apheleia-formatters
                 '(prettier-css
                   . (npx "prettier" "--stdin-filepath" filepath "--parser=css"
                          (apheleia-formatters-js-indent "--use-tabs" "--tab-width"))))
    (add-to-list 'apheleia-formatters
                 '(prettier-html
                   . (npx "prettier" "--stdin-filepath" filepath "--parser=html"
                          (apheleia-formatters-js-indent "--use-tabs" "--tab-width"))))
    (add-to-list 'apheleia-formatters
                 '(prettier-json
                   . (npx "prettier" "--stdin-filepath" filepath "--parser=json"
                          (apheleia-formatters-js-indent "--use-tabs" "--tab-width"))))
    (add-to-list 'apheleia-formatters
                 '(prettier-typescript
                   . (npx "prettier" "--stdin-filepath" filepath "--parser=typescript"
                          (apheleia-formatters-js-indent "--use-tabs" "--tab-width")))))

  (apheleia-global-mode +1))

;; theme
;; ef-themes
(use-package ef-themes
  :config
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t)
  (load-theme 'ef-melissa-light t))
;; modus-themes
(use-package modus-themes
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-disable-other-themes t)

  (setq modus-themes-completions
        '((t . (underline))))

  (setq modus-themes-common-palette-overrides
        '((fg-completion-match-0 blue)
          (fg-completion-match-1 magenta-warmer)
          (fg-completion-match-2 cyan)
          (fg-completion-match-3 red)
          (bg-completion-match-0 bg-blue-nuanced)
          (bg-completion-match-1 bg-magenta-nuanced)
          (bg-completion-match-2 bg-cyan-nuanced)
          (bg-completion-match-3 bg-red-nuanced)))

  ;; (load-theme 'modus-operandi-tinted t)
  )

;; puni
(use-package puni
  :config
  (puni-global-mode +1))

;; string-inflection
(use-package string-inflection
  :bind ( :map my-string-inflection-map
          ("a" . string-inflection-all-cycle)
          ("_" . string-inflection-underscore)
          ("p" . string-inflection-pascal-case)
          ("c" . string-inflection-camelcase)
          ("u" . string-inflection-upcase)
          ("k" . string-inflection-kebab-case)
          ("C" . string-inflection-capital-underscore))
  :init
  (defvar my-string-inflection-map (make-keymap)))

;; go-translate
(use-package go-translate
  :init
  (setq gts-translate-list '(("en" "ja"))))

;; avy
(use-package avy
  )

;; ace-window
(use-package ace-window
  )

;; lin
(use-package lin
  :init
  (setq lin-face 'lin-red)
  (lin-global-mode +1))

;; pulsar
(use-package pulsar
  :config
  (pulsar-global-mode +1))

;; goggles
(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t)) ;; set to nil to disable pulsing

;; spacious-padding
(use-package spacious-padding
  :config
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8))

  ;; Read the doc string of `spacious-padding-subtle-mode-line' as it
  ;; is very flexible and provides several examples.
  (setq spacious-padding-subtle-mode-line
        `( :mode-line-active 'default
           :mode-line-inactive vertical-border))

  (spacious-padding-mode +1))

;; beframe
(use-package beframe
  :config
  (defvar consult-buffer-sources)
  (declare-function consult--buffer-state "consult")

  (with-eval-after-load 'consult
    (defface beframe-buffer
      '((t :inherit font-lock-string-face))
      "Face for `consult' framed buffers.")

    (defvar beframe-consult-source
      `( :name     "Frame-specific buffers (current frame)"
         :narrow   ?F
         :category buffer
         :face     beframe-buffer
         :history  beframe-history
         :items    ,#'beframe-buffer-names
         :action   ,#'switch-to-buffer
         :state    ,#'consult--buffer-state))

    (add-to-list 'consult-buffer-sources 'beframe-consult-source))

  (beframe-mode +1))

;; aggressive-indent
(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

;; perfect-mergin
(use-package perfect-margin
  :config
  (setq perfect-margin-ignore-filters nil)
  (perfect-margin-mode +1))

;; breadcrumb
(use-package breadcrumb
  :vc ( :fetcher github :repo "joaotavora/breadcrumb")
  :config
  (breadcrumb-mode +1))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; imenu-list
(use-package imenu-list
  :bind ( :map my-toggle-map
          ("i" . imenu-list-smart-toggle))
  :init
  (setq imenu-list-position 'left))

;; autorevert
;; Check for file updates and update buffers as well.
(use-package autorevert
  :defer 3
  :hook (after-init . global-auto-revert-mode))

;;
;; programing language config
;;

;; elisp
;; highlight-defined
(use-package highlight-defined
  :hook (emacs-lisp-mode . highlight-defined-mode))
;; highlight-quoted
(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode))
;; web
;; typescript-mode
(use-package typescript-mode
  )
;; jtsx
(use-package jtsx
  :ensure t
  :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
         ("\\.tsx\\'" . jtsx-tsx-mode)
         ("\\.ts\\'" . jtsx-typescript-mode))
  :commands jtsx-install-treesit-language
  :hook ((jtsx-jsx-mode . hs-minor-mode)
         (jtsx-tsx-mode . hs-minor-mode)
         (jtsx-typescript-mode . hs-minor-mode))
  :custom
  ;; Optional customizations
  ;; (js-indent-level 2)
  ;; (typescript-ts-mode-indent-offset 2)
  ;; (jtsx-switch-indent-offset 0)
  (jtsx-indent-statement-block-regarding-standalone-parent nil)
  (jtsx-jsx-element-move-allow-step-out t)
  (jtsx-enable-jsx-electric-closing-element t)
  (jtsx-enable-electric-open-newline-between-jsx-element-tags t)
  (jtsx-enable-jsx-element-tags-auto-sync t)
  (jtsx-enable-all-syntax-highlighting-features t)
  :config
  (defun jtsx-bind-keys-to-mode-map (mode-map)
    "Bind keys to MODE-MAP."
    (define-key mode-map (kbd "C-c C-j") 'jtsx-jump-jsx-element-tag-dwim)
    (define-key mode-map (kbd "C-c j o") 'jtsx-jump-jsx-opening-tag)
    (define-key mode-map (kbd "C-c j c") 'jtsx-jump-jsx-closing-tag)
    (define-key mode-map (kbd "C-c j r") 'jtsx-rename-jsx-element)
    (define-key mode-map (kbd "C-c <down>") 'jtsx-move-jsx-element-tag-forward)
    (define-key mode-map (kbd "C-c <up>") 'jtsx-move-jsx-element-tag-backward)
    (define-key mode-map (kbd "C-c C-<down>") 'jtsx-move-jsx-element-forward)
    (define-key mode-map (kbd "C-c C-<up>") 'jtsx-move-jsx-element-backward)
    (define-key mode-map (kbd "C-c C-S-<down>") 'jtsx-move-jsx-element-step-in-forward)
    (define-key mode-map (kbd "C-c C-S-<up>") 'jtsx-move-jsx-element-step-in-backward)
    (define-key mode-map (kbd "C-c j w") 'jtsx-wrap-in-jsx-element)
    (define-key mode-map (kbd "C-c j u") 'jtsx-unwrap-jsx)
    (define-key mode-map (kbd "C-c j d") 'jtsx-delete-jsx-node))

  (defun jtsx-bind-keys-to-jtsx-jsx-mode-map ()
    (jtsx-bind-keys-to-mode-map jtsx-jsx-mode-map))

  (defun jtsx-bind-keys-to-jtsx-tsx-mode-map ()
    (jtsx-bind-keys-to-mode-map jtsx-tsx-mode-map))

  (add-hook 'jtsx-jsx-mode-hook 'jtsx-bind-keys-to-jtsx-jsx-mode-map)
  (add-hook 'jtsx-tsx-mode-hook 'jtsx-bind-keys-to-jtsx-tsx-mode-map))
;; emmet-mode
(use-package emmet-mode
  :hook ((html-mode
          css-mode
          js-mode
          typescript-mode) . emmet-mode))
;; web-beautify
(use-package web-beautify
  :defer t)
;; common-lisp
(use-package slime
  :defer t
  :init
  (setq inferior-lisp-program "sbcl"))
;; slime-company
(use-package slime-company
  :after (slime company)
  :config
  (setq slime-company-completion 'fuzzy
        slime-company-after-completion 'slime-company-just-one-space))

;; c/c++ mode
(use-package cc-mode
  :defer t
  :config
  (setq c-default-style "bsd")
  (setq c-basic-offset 2) ;; basic indent value
  (setq tab-width 2)      ;; tab width
  (setq indent-tabs-mode nil)  ;; indent use space.
  (c-set-offset 'innamespace 0) ;; namespace indent pos is 0
  )

;; glsl-mode
(use-package glsl-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\.vsh$" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\.fsh$" . glsl-mode)))

;; markdown
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  ;; need to installed "pandoc.exe" and set environment path for pandoc.exe.
  :config
  (when (eq system-type 'windows-nt)
    (setq markdown-command "pandoc.exe -s --standalone --metadata pagetitle=markdown -t html5 -c https://cdn.jsdelivr.net/npm/github-markdown-css@3.0.1/github-markdown.css"))
  (unless (eq system-type 'windows-nt)
    (setq markdown-command "pandoc -s --standalone --metadata pagetitle=markdown -t html5 -c https://cdn.jsdelivr.net/npm/github-markdown-css@3.0.1/github-markdown.css"))
  (setq markdown-fontify-code-blocks-natively t))

;; cmake-mode
(use-package cmake-mode
  :defer 1
  )

;; editorconfig
(use-package editorconfig
  :defer t
  :config
  (editorconfig-mode)
  (setq editorconfig-exec-path "~/.emacs.d/editorconfig/.editorconfig"))

;; eglot
(use-package eglot
  :defer t
  :bind ( :map eglot-mode-map
          ("C-c r" . eglot-rename)
          ("C-c o" . eglot-code-action-organize-imports)
          ("C-c a" . eglot-code-actions)
          ("C-c h" . eldoc)
          ("<f6>" . xref-find-definitions))
  :init
  (setq eglot-events-buffer-config '(:size 0  :format short)
        eglot-ignored-server-capabilities '(:documentHighlightProvider)
        eglot-stay-out-of '(flymake)
        eglot-send-changes-idle-time 1.0)

  (defun my/add-directory-to-exec-path-recursively (dir)
    "Recursively add directories and their subdirectories to `exec-path`."
    (add-to-list 'exec-path dir)
    (dolist (entry (directory-files dir t "^[^.]" t))
      (when (file-directory-p entry)
        (my/add-directory-to-exec-path-recursively entry))))

  (defun my/load-lsp-exec-path ()
    (interactive)
    (my/add-directory-to-exec-path-recursively "~/.emacs.d/.cache/"))

  (my/load-lsp-exec-path))
;; eglot-tempel
(use-package eglot-tempel
  :after (eglot tempel)
  :hook (eglot-managed-mode . eglot-tempel-mode))
;; consult-eglot
(use-package consult-eglot
  :after eglot
  :bind ( :map eglot-mode-map
          ("C-c s" . consult-eglot-symbols)))
;; jsonrpc
(use-package jsonrpc
  :config
  (setq jsonrpc-default-request-timeout 3000)
  (fset #'jsonrpc--log-event #'ignore))
;; eglot-booster
(use-package eglot-booster
  :after eglot
  :vc ( :fetcher github :repo "jdtsmith/eglot-booster")
  :config
  (eglot-booster-mode +1))
;; eglot-x
(use-package eglot-x
  :vc ( :fetcher github :repo "nemethf/eglot-x")
  :after eglot
  :config
  (eglot-x-setup))
;; eldoc-box
(use-package eldoc-box
  :hook (eglot-managed-mode . eldoc-box-hover-mode))
;; eglot-signature-eldoc-talkative
(use-package eglot-signature-eldoc-talkative
  :after eldoc-box
  :config
  (advice-add #'eglot-signature-eldoc-function
              :override #'eglot-signature-eldoc-talkative))

;; lsp-mode
(use-package lsp-mode
  ;; :hook (((typescript-ts-mode
  ;;          tsx-ts-mode
  ;;          html-ts-mode
  ;;          css-ts-mode
  ;;          json-ts-mode) . lsp))
  :init
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-keymap-prefix "M-l")
  (setq lsp-headerline-breadcrumb-enable nil
        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-text-document-color nil
        lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil
        lsp-auto-execute-action nil
        lsp-before-save-edits nil))
;; symbol-overlay
(use-package symbol-overlay
  :hook (prog-mode . symbol-overlay-mode))
;; lsp-snippet
(use-package lsp-snippet
  :vc ( :fetcher github :repo "svaante/lsp-snippet")
  :config
  (when (featurep 'lsp)
    (lsp-snippet-tempel-lsp-mode-init)))
;; emacs-lsp-booster
(progn
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))


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
(defun my-setup-frame-size-and-position ()
  "プライマリモニターの解像度の70%に設定し、中央に配置します。"
  (let* ((monitor-attrs (car (display-monitor-attributes-list)))  ; プライマリモニターの情報を取得
         (geometry (alist-get 'geometry monitor-attrs))           ; モニターのジオメトリ（位置とサイズ）
         (screen-width (nth 2 geometry))                          ; ディスプレイの幅（ピクセル）
         (screen-height (nth 3 geometry))                         ; ディスプレイの高さ（ピクセル）
         (char-width (frame-char-width))                          ; 1文字の幅（ピクセル）
         (char-height (frame-char-height))                        ; 1文字の高さ（ピクセル）
         (frame-width (round (/ (* 0.7 screen-width) char-width))) ; フレーム幅（文字単位）
         (frame-height (round (/ (* 0.7 screen-height) char-height))) ; フレーム高さ（文字単位）
         (frame-left (round (/ (- screen-width (* frame-width char-width)) 2))) ; 左端位置
         (frame-top (round (/ (- screen-height (* frame-height char-height)) 2)))) ; 上端位置
    ;; default-frame-alistに設定を追加
    (add-to-list 'default-frame-alist `(width . ,frame-width))
    (add-to-list 'default-frame-alist `(height . ,frame-height))
    (add-to-list 'default-frame-alist `(left . ,frame-left))
    (add-to-list 'default-frame-alist `(top . ,frame-top))))

;; Adjusted config when run emacs
(my-setup-frame-size-and-position)

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

;; 保存されたコマンドの履歴を使うための設定
(use-package savehist
  :init
  (savehist-mode))

;;
;; Hydra config
;;

                                        ; helper func to hydra menu
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
  ;; Move
  ("M-<" beginning-of-buffer)
  ("M->" end-of-buffer)
  ("M-f" forward-word)
  ("M-b" backward-word)
  ("M-C-a" c-beginning-of-defun)
  ("M-C-e" c-end-of-defun)
  ("M-C-n" forward-list)
  ("M-C-p" backward-list)
  ;; Select
  ("C-x h" mark-whole-buffer)
  ("C-x SPC" rectangle-mark-mode)
  ("M-k" kill-sentence)
  ("M-SPC" just-one-space)
  ("M-C-h" c-mark-function)
  ("C-x C-r" recentf-open-files)
  ;; Others
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
(when (>= emacs-major-version 29)
  (global-set-key (kbd "C-x r") #'restart-emacs)
  (advice-add 'restart-emacs :after
              (lambda (&rest _)
                (run-at-time "2.0 sec" nil #'raise-frame))))

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
                                        ;
;; Vertical partitioning is preferred over horizontal partitioning
(setq split-width-threshold 160)
(setq split-height-threshold nil)
;; Display a bar that clearly indicates the number of characters per line
(setq-default display-fill-column-indicator-column 100)
(global-display-fill-column-indicator-mode)

;; profile
;;(profiler-report)
;;(profiler-stop)

(provide 'init)


