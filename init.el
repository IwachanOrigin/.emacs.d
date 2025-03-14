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
;; 曜日/日付フォーマットは英語表示にしたい(org-scheduleなどで有効)
;; >= emacs 28
(setq system-time-locale "C")
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

;; so-long
;; 長い行を含むファイルの最適化
;; https://ayatakesi.github.io/emacs/28.1/html/Long-Lines.html
(use-package so-long
  :init
  (global-so-long-mode +1))

;; Optimizing performance
;; https://ayatakesi.github.io/lispref/25.2/html/Output-from-Processes.html
(setq process-adaptive-read-buffering t)
;; from protesilaos
;; 閉じ括弧を入力しても点滅させない
(setq blink-matching-paren nil)
;; vcのバックエンドをGitのみに変更
(setq vc-handled-backends '(Git))
;; from doomemacs
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
;; from Centaur Emacs
;; 各OSの最適化
(when IS-WINDOWS
  (setq w32-use-native-image-API t))
(unless IS-MAC
  (setq command-line-ns-option-alist nil))
(unless IS-LINUX
  (setq command-line-x-option-alist nil))

;; fontaine
;; フォントの設定
(use-package fontaine
  :defer 1
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
;; nerd-icons
(use-package nerd-icons
  :defer 1)
(use-package nerd-icons-completion
  :hook (after-init . nerd-icons-completion-mode))
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))
(use-package nerd-icons-corfu
  :vc ( :fetcher github :repo "LuigiPiucco/nerd-icons-corfu")
  :after (corfu nerd-icons)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; dashboard
(use-package dashboard
  :defer 0.1
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
  :defer 1
  :demand t
  :hook (prog-mode . (lambda ()
                       (global-set-key [remap c-indent-line-or-region] #'indent-for-tab-command)))
  :config
  (setopt corfu-cycle t
          corfu-auto t
          corfu-auto-delay 0.0
          corfu-auto-prefix 2
          corfu-on-exact-match 'show)

  (global-corfu-mode)

  (with-eval-after-load 'lsp-mode
    (setopt lsp-completion-provider :none))

  (with-eval-after-load 'orderless
    (defun my/orderless-for-corfu ()
      (setq-local orderless-matching-styles '(orderless-flex)))

    (add-hook 'corfu-mode-hook #'my/orderless-for-corfu))
  :custom
  ;; https://github.com/minad/corfu?tab=readme-ov-file#configuration
  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  ;; 参考 : https://www.grugrut.net/posts/202408192021/
  (text-mode-ispell-word-completion . nil))

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
  (define-key corfu-map (kbd "<backspace") corfu-magic-cancel-or-backspace))

;; cape
(use-package cape
  :defer 1
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
  :defer 1
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
  :after vertico
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
  :defer 1
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
  :defer 1
  :config
  (setq prescient-aggressive-file-save t)
  (prescient-persist-mode))

;; vertico-prescient
(use-package vertico-prescient
  :after (vertico prescient)
  :config
  (setq vertico-prescient-enable-filtering nil)
  (vertico-prescient-mode))

;; corfu-prescient
(use-package corfu-prescient
  :after (corfu prescient)
  :config
  (setq corfu-prescient-enable-filtering nil)
  (corfu-prescient-mode))

;; consult-line-migemo
(defun consult-line-migemo ()
  (interactive)
  (let ((input (read-string "Consult-migemo: " nil)))
    (consult-line (migemo-get-pattern input))))

;;
(global-set-key (kbd "C-s") #'consult-line-migemo)

;; consult
;; Example configuration for Consult
(use-package consult
  :defer 1
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)                                 ;; orig. repeat-complex-command
         ([remap switch-to-buffer] . consult-buffer)                           ;; orig. switch-to-buffer
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)   ;; orig. switch-to-buffer-other-frame
         ([remap bookmark-jump] . consult-bookmark)                            ;; orig. bookmark-jump
         ([remap project-switch-to-buffer] . consult-project-buffer)           ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g
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
         ;; M-s
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
         ("M-s e" . consult-isearch-hisstory)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                   ;; needed by consult-line to detect isearch
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
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help))


;; marginalia
;; vertico の候補に情報を追加する。
(use-package marginalia
  :defer 1
  :init
  (marginalia-mode))

;; embark
;; vertico の候補等に様々なアクションを提供してくれます。
(use-package embark
  :defer 1
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
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; tempel
;; tempelはEmacs用のテンプレートパッケージ
(use-package tempel
  :defer 1
  :demand t
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert)))

;; tempel-collection
(use-package tempel-collection
  :after tempel)

;; git
;; magit
(use-package magit
  :defer 1
  :config
  (when IS-WINDOWS
    (setq magit-refresh-status-buffer nil)
    (setq auto-revert-buffer-list-filter
          'magit-auto-revert-repository-buffer-p)
    (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)
    (remove-hook 'server-switch-hook 'magit-commit-diff)
    (remove-hook 'with-editor-filter-visit-hook 'magit-commit-diff)))

;; diff-hl
;; ウィンドウの左側にコミットされていない箇所を強調表示してくれます.
(use-package diff-hl
  :defer 2
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode))
  :init
  (global-diff-hl-mode +1)
  (global-diff-hl-show-hunk-mouse-mode +1)
  (diff-hl-margin-mode +1))
;; difftastic.el
;; Emacsでdifftasticを使用できるようにします。通常のコマンドとしても使用でき、magitにも統合させています。
(use-package difftastic
  :defer 2
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
;; キーバインドの可視化
(use-package which-key
  :defer 1
  :config
  (which-key-mode +1))

;; undo
;; undo-fu
;; Emacsのundoとredoを強化するパッケージです
(use-package undo-fu
  :defer 2)
;; undo-fu-session
;; undo情報をEmacs終了後も保持してくれるようになります。
(use-package undo-fu-session
  :defer 2
  :config
  (undo-fu-session-global-mode +1))
;; vundo
;; undo履歴を視覚的に分かりやすく表示してくれます。
;; https://github.com/casouri/vundo
(use-package vundo
  :defer 2)

;; rg
(use-package rg
  :defer 2
  )

;; theme
;; modus-themes
(use-package modus-themes
  :defer 1
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

  ;;(load-theme 'modus-operandi-tinted t)
  (load-theme 'modus-vivendi-tinted t)
  )

;; puni
;; 括弧等の構造を操作するパッケージです。
(use-package puni
  :defer 1
  :config
  (puni-global-mode +1))

;; alert
;; https://github.com/jwiegley/alert
;; 通知機能を利用できるようにします。主に org-pomodoro で使用します。
(use-package alert
  :init
  (setq alert-default-style 'libnotify))

;; alert-toast
;; https://github.com/gkowzan/alert-toast
;; Windowsのtoast機能を使うための設定です。
(use-package alert-toast
  :if IS-WINDOWS
  :init
  (setq alert-default-style 'toast))

;; string-inflection
;; https://github.com/akicho8/string-inflection
;; カーソル直下の単語をunderscore -> UPCASE -> CamelCase に変換してくれます。
(use-package string-inflection
  :defer 1
  :config
  (global-set-key (kbd "C-c C-u") 'string-inflection-all-cycle))

;; avy
;; https://github.com/abo-abo/avy
;; 画面上の文字へ移動できるようになります。
(use-package avy
  :defer 1
  :config
  (global-set-key (kbd "C-'") 'avy-goto-char-timer)
  (global-set-key (kbd "C-:") 'avy-goto-line)
  (setq avy-timeout-seconds 1.0))

;; ace-window
(use-package ace-window
  :defer 1
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; lin
;; hl-line-mode を強化するパッケージです。
(use-package lin
  :defer 1
  :init
  (setq lin-face 'lin-red)
  (lin-global-mode +1))

;; pulsar
;; カーソルの移動を視覚的に分かりやすくしてくれます。
(use-package pulsar
  :defer 1
  :config
  (pulsar-global-mode +1))

;; goggles
;; https://github.com/minad/goggles
;; 何処に貼り付けたのかとか、視覚的に目立ちやすくします。
(use-package goggles
  :defer 1
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t)) ;; set to nil to disable pulsing

;; spacious-padding
;; https://github.com/protesilaos/spacious-padding?tab=readme-ov-file
;; スペースを設定して、見やすくします。
(use-package spacious-padding
  :defer 1
  :config
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 6
           :tab-width 2
           :right-divider-width 30
           :scroll-bar-width 8))

  ;; Read the doc string of `spacious-padding-subtle-mode-line' as it
  ;; is very flexible and provides several examples.
  (setq spacious-padding-subtle-mode-line
        `( :mode-line-active 'default
           :mode-line-inactive vertical-border))

  (spacious-padding-mode +1))

;; aggressive-indent
;; インデントを自動的に整えてくれるパッケージ
;; emacs-list-modeの時のみ、自動インデントします。
(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

;; perfect-mergin
;; https://github.com/mpwang/perfect-margin
;; バッファが1つの時、中央に表示します。2つ以上の時は通常の表示に戻ります。
(use-package perfect-margin
  :defer 1
  :config
  (setq perfect-margin-ignore-filters nil)
  (setq perfect-margin-only-set-left-margin t)
  (perfect-margin-mode +1))

;; breadcrumb
;; バッファ上部にパンくずリストを表示してくれます。
(use-package breadcrumb
  :defer 1
  :vc ( :fetcher github :repo "joaotavora/breadcrumb")
  :config
  (breadcrumb-mode +1))

;; page-break-lines
;; https://github.com/purcell/page-break-lines
;; https://www.emacswiki.org/emacs/PageBreaks
;; ^Lの改ページ文字の表示を良くします。
(use-package page-break-lines
  :config
  (page-break-lines-mode +1))

;; rainbow-delimiters
;; かっこに色を付けて見やすくします。
(use-package rainbow-delimiters
  :defer 1
  :hook (prog-mode . rainbow-delimiters-mode))

;; imenu-list
(use-package imenu-list
  :defer 1
  :init
  (setq imenu-list-position 'left))

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

;; clang-format
;; 「.clang-format」はユーザーフォルダの直下にあれば良い様子
(use-package clang-format
  :commands (clang-format-buffer clang-format-on-save-mode)
  :hook ((c-mode . clang-format-on-save-mode)
         (c++-mode . clang-format-on-save-mode))
  :config
  (setq clang-format-style "file") ;; .clang-format を参照
  (setq clang-format-fallback-style "none")) ;; .clang-format がない場合は何もしない

;; glsl-mode
(use-package glsl-mode
  :defer 5
  :config
  (add-to-list 'auto-mode-alist '("\.vsh$" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\.fsh$" . glsl-mode)))

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

;; cmake-mode
(use-package cmake-mode
  :defer 1)

;; Dart-mode
(use-package dart-mode
  :defer 1)

;; elisp
;; highlight-defined
;; https://github.com/Fanael/highlight-defined
;; 既知のシンボルに色を付けてくれます。
(use-package highlight-defined
  :defer 1
  :hook (emacs-lisp-mode . highlight-defined-mode))

;; highlight-quoted
;; https://github.com/Fanael/highlight-quoted
;; 引用符と引用記号を色付けしてくれます。
(use-package highlight-quoted
  :defer 1
  :hook (emacs-lisp-mode . highlight-quoted-mode))

;; About Web
;; typescript-mode
(use-package typescript-mode
  :defer 1)

;; emmet-mode
;; https://github.com/smihica/emmet-mode
;; emmetとは？ => https://zenn.dev/miz_dev/articles/6cac5f2e32398d
(use-package emmet-mode
  :hook ((html-mode
          css-mode
          js-mode
          typescript-mode) . emmet-mode))

;; web-beautify
;; https://github.com/yasuyk/web-beautify
(use-package web-beautify
  :defer 1)



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
                 '((c-mode c++-mode c-ts-mode c++-ts-mode c-or-c++-ts-mode)
                   . ("clangd"
                      "-j=2"
                      "--log=error"
                      "--background-index=false"
                      "--clang-tidy"
                      "--cross-file-rename"
                      "--completion-style=detailed"
                      "--pch-storage=disk"
                      "--header-insertion=never"
                      "--header-insertion-decorators=0"))))

  (with-eval-after-load 'flymake
    (define-key flymake-mode-map (kbd "C-c ! n") nil)
    (define-key flymake-mode-map (kbd "C-c ! p") nil)
    (define-key flymake-mode-map (kbd "C-c n") 'flymake-goto-next-error)
    (define-key flymake-mode-map (kbd "C-c p") 'flymake-goto-prev-error))

  (add-hook 'c++-mode-hook
            (lambda ()
              (eglot-ensure)
              (message "called c++-mode-hook")
              (setq c-default-style "bsd")
              (setq c-basic-offset 2) ;; basic indent value
              (setq tab-width 2)      ;; tab width
              (setq indent-tabs-mode nil)  ;; indent use space.
              (c-set-offset 'innamespace 0) ;; namespace indent pos is 0
              ))
  (add-hook 'c-mode-hook
            (lambda ()
              (eglot-ensure)
              (message "called c-mode-hook")
              (setq c-default-style "bsd")
              (setq c-basic-offset 2) ;; basic indent value
              (setq tab-width 2)      ;; tab width
              (setq indent-tabs-mode nil)  ;; indent use space.
              (c-set-offset 'innamespace 0) ;; namespace indent pos is 0
              ))
  )

;; eglot-booster
;; https://github.com/jdtsmith/eglot-booster
;; emacsとlspサーバ間の通信速度を向上させるプログラムらしい
;; elispとは別で以下のリポジトリからバイナリファイルも必要になる
;; ダウンロード後、配置したらパスを通す
;; https://github.com/blahgeek/emacs-lsp-booster
(use-package eglot-booster
  :after eglot
  :vc ( :fetcher github :repo "jdtsmith/eglot-booster")
  :config
  (eglot-booster-mode +1))

;; consult-eglot
;; https://github.com/mohkale/consult-eglot
;; consultとeglotを統合するパッケージ。シンボルの検索が行えるようになる。
(use-package consult-eglot
  :after eglot
  :bind
  ("C-c s" . consult-eglot-symbols))

;; jsonrpc
;; jsonを扱うEmacsの標準パッケージ
;; デフォルトのタイムアウト時間が短いため、タイムアウトしないように時間を延ばしている
;; また、ログを無視するように設定し、パフォーマンスを向上させている。
(use-package jsonrpc
  :config
  (setq jsonrpc-default-request-timeout 3000)
  (fset #'jsonrpc--log-event #'ignore))

;; eglot-x
;; eglotでサポートされる機能が増える
(use-package eglot-x
  :vc ( :fetcher github :repo "nemethf/eglot-x")
  :after eglot
  :config
  (eglot-x-setup))

;; eldoc-box
;; ミニバッファのeldocをposframeで表示してくれる
(use-package eldoc-box
  :defer 1
  :hook (eglot-managed-mode . eldoc-box-hover-mode))

;; eglot-signature-eldoc-talkative
;; eldocの情報を追加する
(use-package eglot-signature-eldoc-talkative
  :after eldoc-box
  :config
  (advice-add #'eglot-signature-eldoc-function
              :override #'eglot-signature-eldoc-talkative))

;; lsp-mode
(use-package lsp-mode
  :defer 1
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
;; emacsの組み込み関数を利用してシンボルをハイライトする
(use-package symbol-overlay
  :defer 1
  :hook (prog-mode . symbol-overlay-mode))

;; lsp-snippet
(use-package lsp-snippet
  :defer 1
  :vc ( :fetcher github :repo "svaante/lsp-snippet")
  :config
  (when (featurep 'lsp)
    (lsp-snippet-tempel-lsp-mode-init)))

;; emacs-lsp-booster
;; https://github.com/blahgeek/emacs-lsp-booster
;; eglot-boosterの親. 高速化するらしい
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
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

;; dap
;; dape
(use-package dape
  :defer 1
  :bind-keymap
  ("C-x C-a" . dape-global-map)
  :config
  ;; Global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode))

;; Enable repeat mode for more ergonomic `dape' use
(use-package repeat
  :after dape
  :config
  (repeat-mode))

;; treemacs
;; https://github.com/Alexander-Miller/treemacs
;; 左側にディレクトリを表示し、ファイルを開いたりできます。
;; perfect-marginと互換性があります。
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  (with-eval-after-load 'treemacs
    (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

;; treemacs-nerd-icons
(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

;; treemacs-projectile
(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

;; treemacs-icons-dired
(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

;; treemacs-magit
(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;; reemacs-persp
(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

;; treemacs-tab-bar
(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))


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
  "プライマリモニターの解像度の80%に設定し、中央に配置します。"
  (let* ((monitor-attrs (car (display-monitor-attributes-list)))  ; プライマリモニターの情報を取得
         (geometry (alist-get 'geometry monitor-attrs))           ; モニターのジオメトリ（位置とサイズ）
         (screen-width (nth 2 geometry))                          ; ディスプレイの幅（ピクセル）
         (screen-height (nth 3 geometry))                         ; ディスプレイの高さ（ピクセル）
         (char-width (frame-char-width))                          ; 1文字の幅（ピクセル）
         (char-height (frame-char-height))                        ; 1文字の高さ（ピクセル）
         (frame-width (round (/ (* 0.8 screen-width) char-width))) ; フレーム幅（文字単位）
         (frame-height (round (/ (* 0.8 screen-height) char-height))) ; フレーム高さ（文字単位）
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
  (when IS-WINDOWS
    (setq migemo-dictionary (expand-file-name "~/.emacs.d/cmigemo-default-win64/dict/cp932/migemo-dict"))
    (setq migemo-coding-system 'cp932-unix))
  (unless IS-WINDOWS
    (setq migemo-dictionary (expand-file-name "~/.emacs.d/cmigemo-default-win64/dict/utf-8/migemo-dict"))
    (setq migemo-coding-system 'utf-8-unix))
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)

  :config
  (migemo-init))

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
    ;; NOTE : Until 29.4 I used [kill-this-buffer], but since 30.1 I can't turn off the buffer except via the menu.
    ;; To solve this problem, [kill-current-buffer] is used.
    ;; It was mentioned as a bug, but it was closed after the description to use [kill-current-buffer] was written in the document.
    ;; https://emacs.stackexchange.com/a/55047
    ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2024-06/msg00840.html
    (global-set-key (kbd "C-x C-c") #'kill-current-buffer)
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

;; Vertical partitioning is preferred over horizontal partitioning
(setq split-width-threshold 160)
(setq split-height-threshold nil)
;; Display a bar that clearly indicates the number of characters per line
(setq-default display-fill-column-indicator-column 100)
(global-display-fill-column-indicator-mode)

;; org
(use-package org
  :defer 1
  :init
  (setq org-return-follows-link t  ; Returnキーでリンク先を開く
        org-mouse-1-follows-link t ; マウスクリックでリンク先を開く
        ))
;;  アンダースコアを入力しても下付き文字にならないようにする
(setq org-use-sub-superscripts '{}
      org-export-with-sub-superscripts nil)

;; org-agenda
(use-package org-agenda
  :ensure nil
  :after org
  :config
  (setq org-agenda-files (file-expand-wildcards (concat org-directory "/*.org"))))

;; org-indent
(use-package org-indent
  :ensure nil
  :hook (org-mode . org-indent-mode))

;; org-pomodoro
;; https://github.com/marcinkoziej/org-pomodoro
(use-package org-pomodoro
  :after org)

;; ox-qmd
;; orgファイルをmarkdownファイルに変換してくれます。
(use-package ox-qmd
  :defer t)

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

  ;; Choose some fonts
  (set-face-attribute 'default nil :family "Iosevka NF")
  (set-face-attribute 'variable-pitch nil :family "Iosevka NFP")
  (set-face-attribute 'org-modern-symbol nil :family "Iosevka NF")

  ;; Add frame borders and window dividers
  (modify-all-frames-parameters
   '((right-divider-width . 40)
     (internal-border-width . 40)))
  (dolist (face '(window-divider
                  window-divider-first-pixel
                  window-divider-last-pixel))
    (face-spec-reset-face face)
    (set-face-foreground face (face-attribute 'default :background)))
  (set-face-background 'fringe (face-attribute 'default :background))

  (global-org-modern-mode))

;; org-modern-indent
(use-package org-modern-indent
  :vc ( :fetcher github :repo "jdtsmith/org-modern-indent")
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

;; keycast
;; https://github.com/tarsius/keycast
;; https://protesilaos.com/emacs/dotemacs
;; 5.3.1. The prot-emacs-modeline.el section about keycast
(use-package keycast
  :ensure t
  :defer 1
  :commands (keycast-mode-line-mode keycast-header-line-mode keycast-tab-bar-mode keycast-log-mode)
  :init
  (setq keycast-mode-line-format "%2s%k%c%R")
  (setq keycast-mode-line-window-predicate 'mode-line-window-selected-p)
  (setq keycast-mode-line-remove-tail-elements nil)
  (keycast-mode-line-mode)
  :config
  (dolist (input '(self-insert-command org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '( mouse-event-p mouse-movement-p mwheel-scroll handle-select-window
                    mouse-set-point mouse-drag-region))
    (add-to-list 'keycast-substitute-alist `(,event nil))))

;; time
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/time.el
;; https://protesilaos.com/emacs/dotemacs
;; 5.2.16. The prot-emacs-essentials.el configurations for the date and time (display-time-mode)
;;;; Display current time
(use-package time
  :ensure nil
  :hook (after-init . display-time-mode)
  :config
  (setq display-time-format " %a %e %b, %H:%M ")
  ;;;; Covered by `display-time-format'
  ;; (setq display-time-24hr-format t)
  ;; (setq display-time-day-and-date t)
  (setq display-time-interval 60)
  (setq display-time-default-load-average nil)
  ;; NOTE 2022-09-21: For all those, I have implemented my own solution
  ;; that also shows the number of new items, although it depends on
  ;; notmuch: the `notmuch-indicator' package.
  (setq display-time-mail-directory nil)
  (setq display-time-mail-function nil)
  (setq display-time-use-mail-icon nil)
  (setq display-time-mail-string nil)
  (setq display-time-mail-face nil)

  ;; I don't need the load average and the mail indicator, so let this
  ;; be simple:
  (setq display-time-string-forms
        '((propertize
           (format-time-string display-time-format now)
           'face 'display-time-date-and-time
           'help-echo (format-time-string "%a %b %e, %Y" now))
          " ")))


;; World clock (M-x world-clock)
;; https://protesilaos.com/emacs/dotemacs
;; 5.2.17. The prot-emacs-essentials.el settings for the world-clock
(use-package time
  :ensure nil
  :commands (world-clock)
  :config
  (setq display-time-world-list t)
  (setq zoneinfo-style-world-list ; M-x shell RET timedatectl list-timezones
        '(("America/Los_Angeles" "Los Angeles")
          ("America/Vancouver" "Vancouver")
          ("America/Chicago" "Chicago")
          ("America/Toronto" "Toronto")
          ("America/New_York" "New York")
          ("UTC" "UTC")
          ("Europe/Lisbon" "Lisbon")
          ("Europe/Brussels" "Brussels")
          ("Europe/Athens" "Athens")
          ("Asia/Riyadh" "Riyadh")
          ("Asia/Tbilisi" "Tbilisi")
          ("Asia/Singapore" "Singapore")
          ("Asia/Shanghai" "Shanghai")
          ("Asia/Seoul" "Seoul")
          ("Asia/Tokyo" "Tokyo")
          ("Australia/Brisbane" "Brisbane")
          ("Australia/Sydney" "Sydney")
          ("Pacific/Auckland" "Auckland")))

  ;; All of the following variables are for Emacs 28
  (setq world-clock-list t)
  (setq world-clock-time-format "%R	%z	%A %d %B")
  (setq world-clock-buffer-name "*world-clock*") ; Placement handled by `display-buffer-alist'
  (setq world-clock-timer-enable t)
  (setq world-clock-timer-second 60))

;; goto-chg
;; https://github.com/emacs-evil/goto-chg/tree/master
;; https://protesilaos.com/emacs/dotemacs
;; 5.2.22. The prot-emacs-essentials.el section about goto-chg (go to change)
(use-package goto-chg
  :ensure t
  :bind
  (("C-(" . goto-last-change)
   ("C-)" . goto-last-change-reverse)))

;; profile
;;(profiler-report)
;;(profiler-stop)

(provide 'init)


