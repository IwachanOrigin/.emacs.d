
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
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

  ;; use-package
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)
    (package-install 'diminish))

  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)
  (setq use-package-compute-statistics t) ;; For "M-x use-package-report"

  (require 'use-package)
  (require 'bind-key) ;; if you use any :bind variant
)

;; A little transparent.
(set-frame-parameter (selected-frame) 'alpha '(0.85))

;; Optimization
(when (eq window-system 'w32)
  (setq w32-get-true-file-attributes nil
        w32-pipe-read-delay 0
        w32-pipe-buffer-size (* 64 1024)))

(setq read-process-output-max #x10000)
(setq ffap-machine-p-known 'reject)

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
    ;; IME 初期化
    (w32-ime-initialize)
    ;; IME 制御（yes/no などの入力の時に IME を off にする）
    (wrap-function-to-control-ime 'universal-argument t nil)
    (wrap-function-to-control-ime 'read-string nil nil)
    (wrap-function-to-control-ime 'read-char nil nil)
    (wrap-function-to-control-ime 'read-from-minibuffer nil nil)
    (wrap-function-to-control-ime 'y-or-n-p nil nil)
    (wrap-function-to-control-ime 'yes-or-no-p nil nil)
    (wrap-function-to-control-ime 'map-y-or-n-p nil nil)
    (wrap-function-to-control-ime 'register-read-with-preview nil nil)))


;; M-x `treesit-install-language-grammar` to install language grammar.
(setq treesit-language-source-alist
      '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
        (c . ("https://github.com/tree-sitter/tree-sitter-c"))
        (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
        (css . ("https://github.com/tree-sitter/tree-sitter-css"))
        (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
        (csharp     . ("https://github.com/tree-sitter/tree-sitter-c-sharp"))
        (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
        (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
        (elixir "https://github.com/elixir-lang/tree-sitter-elixir" "main" "src" nil nil)
        (go . ("https://github.com/tree-sitter/tree-sitter-go"))
        (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
        (haskell "https://github.com/tree-sitter/tree-sitter-haskell" "master" "src" nil nil)
        (html . ("https://github.com/tree-sitter/tree-sitter-html"))
        (java       . ("https://github.com/tree-sitter/tree-sitter-java.git"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
        (json . ("https://github.com/tree-sitter/tree-sitter-json"))
        (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
        (make . ("https://github.com/alemuller/tree-sitter-make"))
        (markdown . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
        (markdown-inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src"))
        (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" nil "ocaml/src"))
        (org . ("https://github.com/milisims/tree-sitter-org"))
        (python . ("https://github.com/tree-sitter/tree-sitter-python"))
        (php . ("https://github.com/tree-sitter/tree-sitter-php"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
        (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
        (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
        (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
        (scala "https://github.com/tree-sitter/tree-sitter-scala" "master" "src" nil nil)
        (toml "https://github.com/tree-sitter/tree-sitter-toml" "master" "src" nil nil)
        (vue . ("https://github.com/merico-dev/tree-sitter-vue"))
        (kotlin . ("https://github.com/fwcd/tree-sitter-kotlin"))
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
        (zig . ("https://github.com/GrayJack/tree-sitter-zig"))
        (mojo . ("https://github.com/HerringtonDarkholme/tree-sitter-mojo"))))

(setq major-mode-remap-alist
      '((c-mode          . c-ts-mode)
        (c++-mode        . c++-ts-mode)
        (cmake-mode      . cmake-ts-mode)
        (conf-toml-mode  . toml-ts-mode)
        (css-mode        . css-ts-mode)
        (js-mode         . js-ts-mode)
        (js-json-mode    . json-ts-mode)
        (python-mode     . python-ts-mode)
        (sh-mode         . bash-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (rust-mode       . rust-ts-mode)
        (java-mode       . java-ts-mode)
        ))

;; c/c++ mode
;; ref : https://i-s-2.hatenadiary.org/entry/20091026/1256557730
;; ref : https://www.gnu.org/software/emacs/manual/
;; ref : https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html
(add-hook 'c-mode-common-hook
 #'(lambda ()
     (c-set-style "linux")
     (setq indent-tabs-mode nil) ;; indent use space.
     (setq c-basic-offset 2) ;; basic indent value
     (setq tab-width 2)      ;; tab width
     (c-set-offset 'innamespace 0) ;; namespace pos
     (c-set-offset 'case-label '+) ;; switch label pos
))

(use-package c-ts-mode
  :ensure nil
  :disabled t
  :if (version < "29.0" emacs-version)
  :init
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode)))

;; js mode
(add-hook 'js-ts-mode-hook
 #'(lambda ()
     (make-local-variable 'js-indent-level)
     (setq js-indent-level 2)))

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
  (setq editorconfig-exec-path "~/.emacs.d/.editorconfig"))

;; projectile
(use-package projectile
  :defer 2
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (add-hook 'projectile-after-switch-project-hook 'load-project-debug-config))

;; company
(use-package company
  :defer 2
  :config
  (global-company-mode)
  :custom
  (company-transformers '(company-sort-by-backend-importance))
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)
  (completion-ignore-case t)
  :bind
  (:map company-active-map
        ;; C-n, C-p to select next/previous candidate for completion
        ("C-n" . 'company-select-next)
        ("C-p" . 'company-select-previous)
        ;; Narrow by C-s
        ("C-s" . 'company-filter-candidates)
        ;; C-i, TAB to set candidates
        ("C-i" . 'company-complete-selection)
        ([tab] . 'company-complete-selection))
  (:map company-search-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)))

;; font settings
(when (member "UDEV Gothic" (font-family-list))
  (set-fontset-font t 'unicode (font-spec :family "UDEV Gothic") nil 'prepend)
  (set-face-attribute 'default nil :family "UDEV Gothic" :height 110 :weight 'Regular))
(when (eq window-system 'w32)
  (when (member "Segoe UI Emoji" (font-family-list))
    (set-fontset-font t 'unicode (font-spec :family "Segoe UI Emoji") nil 'prepend)))

;; all-the-icons
(use-package all-the-icons
  :defer 0.01
  :config
  (when (member "all-the-icons" (font-family-list))
    (set-fontset-font t 'unicode (font-spec :family "all-the-icons") nil 'prepend))
  (when (member "file-icons" (font-family-list))
    (set-fontset-font t 'unicode (font-spec :family "file-icons") nil 'prepend))
  (when (member "FontAwesome" (font-family-list))
    (set-fontset-font t 'unicode (font-spec :family "FontAwesome") nil 'prepend))
  (when (member "Material Icons" (font-family-list))
    (set-fontset-font t 'unicode (font-spec :family "Material Icons") nil 'prepend))
  (when (member "octicons" (font-family-list))
    (set-fontset-font t 'unicode (font-spec :family "octicons") nil 'prepend))
  (when (member "Weather Icons" (font-family-list))
    (set-fontset-font t 'unicode (font-spec :family "Weather Icons") nil 'prepend)))

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
(use-package counsel
  :defer 2
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-x C-r" . counsel-recentf)
  ("C-x b" . counsel-switch-buffer))

;; eglot
(progn
  (customize-set-variable 'eglot-autoshutdown t)
  (customize-set-variable 'eglot-extend-to-xref t)
  (customize-set-variable 'eglot-ignored-server-capabilities
    (quote (:documentFormattingProvider :documentRangeFormattingProvider)))

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
       '((c-ts-mode c++-ts-mode)
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

  (add-hook 'c-ts-mode-hook #'eglot-ensure)
  (add-hook 'c++-ts-mode-hook #'eglot-ensure))

;; markdown
(use-package markdown-mode
  :defer 3
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.txt\\'" . gfm-mode))
  ;; need to installed "pandoc.exe" and set environment path for pandoc.exe.
  :config
  (when (eq system-type 'windows-nt)
    (setq markdown-command "pandoc.exe -s --standalone --metadata pagetitle=markdown -t html5 -c https://cdn.jsdelivr.net/npm/github-markdown-css@3.0.1/github-markdown.css"))
  (unless (eq system-type 'windows-nt)
    (setq markdown-command "pandoc -s --standalone --metadata pagetitle=markdown -t html5 -c https://cdn.jsdelivr.net/npm/github-markdown-css@3.0.1/github-markdown.css"))
  (setq markdown-fontify-code-blocks-natively t))

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
(use-package ivy-migemo
  :defer 1
  :config
  ;; toggle migemo
  (define-key ivy-minibuffer-map (kbd "M-m") #'ivy-migemo-toggle-migemo)
  ;; If you want to defaultly use migemo on swiper and counsel-find-file:
  (setq ivy-re-builders-alist '((t . ivy--regex-plus)
                                   (swiper . ivy-migemo--regex-plus)
                                   (counsel-find-file . ivy-migemo--regex-plus))))

;; cmake-mode
(use-package cmake-ts-mode
  :defer 3
  :config
  (add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-ts-mode)))

;; helper func to hydra menu
(defun my/hydra-disable-dimmer ()
  (when (bound-and-true-p dimmer-mode)
    (dimmer-mode -1)))

(defun my/hydra-enable-dimmer ()
  (unless (bound-and-true-p dimmer-mode)
    (dimmer-mode 1)))

;; need to pandoc, latex, eisvogel.latex
(defun pandoc-markdown-pdf ()
  "create beamer slids."
  (interactive)
  (setq infilename (buffer-file-name))
  (setq outfilename (replace-regexp-in-string ".md" ".pdf" infilename))
  (when (eq system-type 'windows-nt)
    (setq cmd-str (concat "pandoc.exe " infilename " -o " outfilename " --from markdown --to beamer --template eisvogel.latex --listings --pdf-engine \"xelatex\" -V CJKmainfont=\"Meiryo UI\"")))
  (unless (eq system-type 'windows-nt)
    (setq cmd-str (concat "pandoc " infilename " -o " outfilename " --from markdown --to beamer --template eisvogel.latex --listings --pdf-engine \"xelatex\" -V CJKmainfont=\"Noto Sans CJK JP\"")))
  (shell-command-to-string cmd-str))
(global-set-key (kbd "C-x C-l") 'pandoc-markdown-pdf)

;; view-mode keybind func
(defun my/setup-view-mode-keymap ()
  "setup view-mode keymap"
  (let ((keymap view-mode-map))
    (define-key keymap (kbd "p") 'previous-line)
    (define-key keymap (kbd "n") 'next-line)
    (define-key keymap (kbd "b") 'backward-char)
    (define-key keymap (kbd ",") 'centaur-tabs-backward)
    (define-key keymap (kbd ".") 'centaur-tabs-forward)))

;; update keymap and cursor color
(defun my/update-cursor-color-viewmode ()
  "Checked view-mode status, if view-mode is true, cursor color to red and adjust to keymap."
  (if view-mode
      (set-cursor-color "#ff0000")
    (set-cursor-color "#ffffff")))

;; view-mode hook
(add-hook 'view-mode-hook 'my/update-cursor-color-viewmode)
(add-hook 'view-mode-hook 'my/setup-view-mode-keymap)
(add-hook 'post-command-hook 'my/update-cursor-color-viewmode)
(add-hook 'buffer-list-update-hook 'my/update-cursor-color-viewmode)

;; view-mode toggle
(defun toggle-view-mode ()
  "toggle view-mode"
  (interactive)
  (cond (view-mode (view-mode -1))
        (t (view-mode 1))))
(global-set-key (kbd "C-x x v") 'toggle-view-mode)

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
_M-C-p_: 前の括弧始まりへ移動  _C-x x v_: toggle-view-mode         _C-x C-n_: dired-sidebar-toggle-sidebar
                                                             _C-x C-l_: pandoc-markdown-pdf
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
  ("C-x x v" toggle-view-mode)
  ; Others
  ("M-x replace-string" replace-string)
  ("C-x r" restart-emacs)
  ("M-x sort-lines" sort-lines)
  ("M-<f10>" toggle-frame-maximized)
  ("C-x x t" toggle-truncate-lines)
  ("C-c n" flymake-goto-next-error)
  ("C-c p" flymake-goto-prev-error)
  ("C-x C-n" dired-sidebar-toggle-sidebar)
  ("C-x C-l" pandoc-markdown-pdf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       server configuration for emacsclient       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;; centaur-tabs
(use-package centaur-tabs
  :defer 0.01
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
  (when (member "UDEV Gothic" (font-family-list))
    (centaur-tabs-change-fonts "UDEV Gothic" 100))
  (centaur-tabs-headline-match)
  (centaur-tabs-mode t)
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.
 Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
 All buffer name start with * will group to \"Emacs\".
 Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
	    ((or (string-equal "*" (substring (buffer-name) 0 1))
	         (memq major-mode '(magit-process-mode
				                      magit-status-mode
				                      magit-diff-mode
				                      magit-log-mode
				                      magit-file-mode
				                      magit-blob-mode
				                      magit-blame-mode)))
	     "Emacs")
	    ((derived-mode-p 'prog-mode)
	     "Editing")
	    ((derived-mode-p 'dired-mode)
	     "Dired")
	    ((memq major-mode '(helpful-mode
			                    help-mode))
	     "Help")
	    ((memq major-mode '(org-mode
			                    org-agenda-clockreport-mode
			                    org-src-mode
			                    org-agenda-mode
			                    org-beamer-mode
			                    org-indent-mode
			                    org-bullets-mode
			                    org-cdlatex-mode
			                    org-agenda-log-mode
			                    diary-mode))
	     "OrgMode")
	    (t
	     (centaur-tabs-get-group-name (current-buffer))))))
  :bind
  ("C-," . centaur-tabs-backward)
  ("C-." . centaur-tabs-forward)
  ("C-c t" . centaur-tabs-counsel-switch-group))

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

  (setq dired-sidebar-theme 'icons)
  (setq dired-sidebar-use-term-integration t)
  :bind
  (("C-x C-n" . dired-sidebar-toggle-sidebar)))

;; GPG key auto update
(setq package-gnupghome-dir "~/.emacs.d/elpa/gnupg")
(use-package gnu-elpa-keyring-update)

;; Vertical partitioning is preferred over horizontal partitioning
(setq split-width-threshold 160)
(setq split-height-threshold nil)

;; profile
;;(profiler-report)
;;(profiler-stop)

