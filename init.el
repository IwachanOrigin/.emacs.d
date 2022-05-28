
;;============================================================================
;;                                 init.el                                  ;;
;;============================================================================

;; profile
;;(require 'profiler)
;;(profiler-start 'cpu)

(eval-and-compile
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

  ;; use-package
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)
    (package-install 'diminish))

  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)

  (require 'use-package))

;; ちょっと透過する
(set-frame-parameter (selected-frame) 'alpha '(0.90))

;; cl-lib
(eval-when-compile
  (use-package cl-lib
    :ensure t))

;; サブプロセスに渡すパラメータの文字コードを cp932 にする
;; ref: https://w.atwiki.jp/ntemacs/pages/16.html
(cl-loop for (func args-pos) in '((call-process        4)
                                  (call-process-region 6)
                                  (start-process       3))
         do (eval `(advice-add ',func
                               :around (lambda (orig-fun &rest args)
                                         (setf (nthcdr ,args-pos args)
                                               (mapcar (lambda (arg)
                                                         (if (multibyte-string-p arg)
                                                             (encode-coding-string arg 'cp932)
                                                           arg))
                                                       (nthcdr ,args-pos args)))
                                         (apply orig-fun args))
                               '((depth . 99))))
)

;; indent
(setq-default c-basic-offset 2      ;; basic indent value
              tab-width 2           ;; tab width
              indent-tabs-mode nil  ;; tab or space
)

;; editorconfig
(use-package editorconfig
  :init
  (editorconfig-mode)
)
(setq edconf-exec-path "~/.emacs.d/editorconfig")

;; c++ mode
(add-hook 'c++-mode-hook
 '(lambda ()
 (c-set-style "bsd")
))
;; c mode
(add-hook 'c-mode-hook
 '(lambda ()
 (c-set-style "bsd")
))

;; company
(use-package company
  :ensure t
  :defer t
  :init (global-company-mode)
  :config
  ;; C-n, C-pで補完候補を次/前の候補を選択
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  ;; TABで候補を設定
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  ;; C-sで絞り込む
  (define-key company-active-map (kbd "C-s") 'company-filter-candidates)
)

;; all-the-icons
(use-package all-the-icons
  :defer t)

;; posframe
(use-package posframe)

;; point
(use-package popwin)

;; autorevert
(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

;; hungry-delete
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

;; Hydra
(use-package hydra
  :config
  (use-package hydra-posframe
    :load-path "~/.emacs.d/github/hydra-posframe"
    :custom
    (hydra-posframe-parameters
     '((left-fringe . 5)
       (right-fringe . 5)))
    :custom-face
    (hydra-posframe-border-face ((t (:background "#6272a4"))))
    :hook
    (after-init . hydra-posframe-mode)))

;; wgrep
(use-package wgrep
  :defer t
  :custom
  (wgrep-enable-key "e")
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

;; anzu
(use-package anzu
  :diminish
  :bind
  ("C-r"   . anzu-query-replace-regexp)
  ("C-M-r" . anzu-query-replace-at-cursor-thing)
  :hook
  (after-init . global-anzu-mode))

;; counsel
(use-package counsel
  :ensure t
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-x C-r" . counsel-recentf)
  ("C-x b" . counsel-switch-buffer)
)

;; avy
(use-package avy
  :bind
  ("C-'" . avy-resume)
  ("C-;" . avy-goto-char)
  ("M-j" . hydra-avy/body)
  :preface
  ;; fixed cursor scroll-up
  (defun scroll-up-in-place (n)
    (interactive "p")
    (forward-line (- n))
    (scroll-down n))
  ;; fixed cursor scroll-down
  (defun scroll-down-in-place (n)
    (interactive "p")
    (forward-line n)
    (scroll-up n))
  ;; yank inner sexp
  (defun yank-inner-sexp ()
    (interactive)
    (backward-list)
    (mark-sexp)
    (copy-region-as-kill (region-beginning) (region-end)))

  (with-eval-after-load 'hydra
        (defhydra hydra-avy (:color pink :hint nil)
          "
                                                                        ╔════════╗
        ^^Goto^^        Kill^^        Yank^^        Move^^        Misc            ║  Jump  ║
  ──────────────────────────────────────────────────────────────────────╨────────╜
    _c_ ← char^^        [_k_] region  [_y_] region  [_m_] region  [_n_] line number
    _a_ ← char2 → _b_   [_K_] line    [_Y_] line    [_M_] line
    _w_ ← word  → _W_   ^^^^^
    _l_ ← line  → _e_   ^^^^^                                     _,_ ← f!y → _._
  ╭──────────────────────────────────────────────────────────────────────────────╯
                      [_q_]: quit, [_i_]: imenu, [_<SPC>_]: resume
"
          ("c" avy-goto-char :exit t)
          ("a" avy-goto-char-2 :exit t)
          ("b" avy-goto-char-below :exit t)
          ("w" avy-goto-word-1 :exit t)
          ("W" avy-goto-word-1-below :exit t)
          ("l" avy-goto-line :exit t)
          ("e" avy-goto-end-of-line :exit t)
          ("M" avy-move-line)
          ("m" avy-move-region)
          ("K" avy-kill-whole-line)
          ("k" avy-kill-region)
          ("Y" avy-copy-line :exit t)
          ("y" avy-copy-region :exit t)
          ("n" goto-line :exit t)
          ("z" avy-zap-to-char-dwim :exit t)
          ("v" hydra-viewer/body :exit t)
          ("<SPC>" avy-resume :exit t)
          ("i" counsel-imenu :exit t)
          ("," flymake-goto-previous-error)
          ("." flymake-goto-next-error)
          ("q" nil)))
)

;; flymake
(use-package flymake
  :ensure t
  :defer t
  :init
  (add-hook 'c-mode-common-hook 'flymake-mode)
  :commands flymake-mode
)

;; eglot
;;
(use-package eglot
  :defer t
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '(c-mode . ("clangd")))
  (add-to-list 'eglot-server-programs '(c++-mode . ("clangd")))
  (add-hook 'c-mode-hook #'eglot-ensure)
  (add-hook 'c++-mode-hook #'eglot-ensure)
  ;; C-M(ESC)=WindowsKeyなので入力補完を行うためにdefine-keyを変える
  ;;(define-key eglot-mode-map (kbd "C-c <tab>") #'completion-at-point)
  (define-key eglot-mode-map (kbd "C-c <tab>") #'company-complete)
)

;; markdown
(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.txt\\'" . gfm-mode))
  ;; need to installed "pandoc.exe" and set environment path for pandoc.exe.
  :init (setq markdown-command "pandoc.exe -s --self-contained -t html5 -c https://cdn.jsdelivr.net/npm/github-markdown-css@3.0.1/github-markdown.min.css")
)

;; rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode-hook . raibow-delimiters-mode)
)

;; recentf
(use-package recentf
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 100)
  (recentf-auto-cleanup 'never)
  (recentf-exclude '((expand-file-name package-user-dir)
                     "recentf"
                     "COMMIT_EDITMSG\\'"))
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              C-sの設定を強化する                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; swiper
(use-package swiper
  :ensure t
  :config
  (defun isearch-forward-or-swiper (use-swiper)
    (interactive "p")
    ;; (interactive "P") ;; 大文字のPだと，C-u C-sでないと効かない
    (let (current-prefix-arg)
      (call-interactively (if use-swiper 'swiper 'isearch-forward))))
  (global-set-key (kbd "C-s") 'isearch-forward-or-swiper)
)

;; ivy
(use-package ivy
  :ensure t
  ;; :config
  ;; (fset 'ivy--regex 'identity)
)

;; migemo
;; 日本語をローマ字検索できるようにする
;; Windows版 migemoが必要 [migemo-kaoriya-64](https://www.kaoriya.net/software/cmigemo/)
(use-package migemo
  :ensure t
  :config
  ;; C/Migemo を使う
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs" "-i" "\a"))
  ;; 以下の記述は相対パスを絶対パスとして扱う術
  ;; (expand-file-name "~/.emacs.d/init.el")
  (setq migemo-dictionary (expand-file-name "~/.emacs.d/cmigemo-default-win64/dict/cp932/migemo-dict"))  ;; 辞書のパス
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  ;; charset encoding
  ;;(setq migemo-coding-system 'utf-8-unix)
  (setq migemo-coding-system 'cp932-unix)
)

;; ivy-migemo
;; ivy系検索でmigemoを利用できるようにする
(use-package ivy-migemo
  :ensure t
  :config
  ;; toggle migemo
  (define-key ivy-minibuffer-map (kbd "M-m") #'ivy-migemo-toggle-migemo)
  ;; If you want to defaultly use migemo on swiper and counsel-find-file:
  (setq ivy-re-builders-alist '((t . ivy--regex-plus)
                              (swiper . ivy-migemo--regex-plus)
                              (counsel-find-file . ivy-migemo--regex-plus))
                              ;(counsel-other-function . ivy-migemo--regex-plus)
                              )
)

;; dashboard
(use-package dashboard
  :ensure t
  :diminish
  (dashboard-mode page-break-lines-mode)
  :custom
  (dashboard-center-content t)
  (dashboard-items '((recents . 15)))
  :custom-face
  (dashboard-heading ((t (:foreground "#f1fa8c" :weight bold))))
  :hook
  (after-init . dashboard-setup-startup-hook)
)

;; dimmer
(use-package dimmer
  :ensure t
  :custom
  (dimmer-fraction 0.2)
  :config
  (dimmer-mode t)
)

;; ace-window
(use-package ace-window
  :bind
  ("C-x o" . ace-window)
  :config
  (setq aw-keys '(?j ?k ?l ?u ?i ?o ?h ?y ?n))
  :custom-face
  (aw-leading-char-face ((t (:height 4.0 :foreground "#f1fa8c"))))
 )

;; neotree
(use-package neotree
  :init
  (setq-default neo-keymap-style 'concise)
  :config
  (setq neo-smart-open t)
  (setq neo-create-file-auto-open t)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (bind-key "<f8>" 'neotree-toggle)
  (bind-key "<left>" 'neotree-select-up-node neotree-mode-map)
  (bind-key "<right>" 'neotree-change-root neotree-mode-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           emacsclientのためのserver設定             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (eq window-system 'w32)
  (when (require 'server nil t)
    (server-start)
    ;; C-x C-cに割り当てる(好みに応じて)
    (global-set-key (kbd "C-x C-c") 'kill-this-buffer)
    ;; M-x exitでEmacsを終了できるようにする
    (defalias 'exit 'save-buffers-kill-emacs)
    ;; 終了時にyes/noの問い合わせ
    (setq confirm-kill-emacs 'yes-or-no-p)
))

;; cmake-mode
(use-package cmake-mode)
(setq auto-mode-alist (append '(("CMakeLists\\.txt\\'" . cmake-mode)) '(("\\.cmake\\'" . cmake-mode)) auto-mode-alist))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(editorconfig wgrep-ag modus-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:height 4.0 :foreground "#f1fa8c"))))
 '(dashboard-heading ((t (:foreground "#f1fa8c" :weight bold))))
 '(hydra-posframe-border-face ((t (:background "#6272a4")))))

;; profile
;;(profiler-report)
;;(profiler-stop)

