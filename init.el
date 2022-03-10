
;;============================================================================
;;                                 init.el                                  ;;
;;============================================================================

;; profile
;;(require 'profiler)
;;(profiler-start 'cpu)

(eval-when-compile
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
(setq-default c-basic-offset 4      ;; basic indent value
              tab-width 4           ;; tab width
              indent-tabs-mode nil  ;; tab or space
)

;; c++ style
(defun add-c++-mode-conf ()
  (c-set-style "stroustrup")  ;;スタイルはストラウストラップ
  (show-paren-mode t)         ;;カッコを強調表示する
)
(add-hook 'c++-mode-hook 'add-c++-mode-conf)

;; C style
(defun add-c-mode-common-conf ()
  (c-set-style "stroustrup") ;;スタイルはストラウストラップ
  (show-paren-mode t)        ;;カッコを強調表示する
)
(add-hook 'c-mode-common-hook 'add-c-mode-common-conf)

;; all-the-icons
(use-package all-the-icons
  :defer t)

;; posframe
(use-package posframe)

;; point
(use-package popwin)
(use-package point-history
  :load-path "~/.emacs.d/github/point-history"
  :config
  (point-history-mode t))

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

;; whick-key
(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

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

;; ag
(use-package ag
  :custom
  (ag-highligh-search t)
  (ag-reuse-buffers t)
  (ag-reuse-window t)
  :bind
  ("M-s a" . ag-project)
  :config
  (use-package wgrep-ag))

;; anzu
(use-package anzu
  :diminish
  :bind
  ("C-r"   . anzu-query-replace-regexp)
  ("C-M-r" . anzu-query-replace-at-cursor-thing)
  :hook
  (after-init . global-anzu-mode))

;; avy
(use-package avy
  :bind
  ("C-'" . avy-resume)
  ("C-;" . avy-goto-char)
  ("M-j" . hydra-avy/body)
  ("M-v" . hydra-viewer/body)
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

  (use-package avy-zap
      :bind
      ("M-z" . avy-zap-to-char-dwim)
      ("M-z" . avy-zap-up-to-char-dwim))

  (with-eval-after-load 'hydra
        (defhydra hydra-viewer (:color pink :hint nil)
          "
                                                                        ╔════════╗
   Char/Line^^^^^^  Word/Page^^^^^^^^  Line/Buff^^^^   Paren                              ║ Window ║
  ──────────────────────────────────────────────────────────────────────╨────────╜
       ^^_k_^^          ^^_u_^^          ^^_g_^^       _(_ ← _y_ → _)_
       ^^^↑^^^          ^^^↑^^^          ^^^↑^^^       _,_ ← _/_ → _._
   _h_ ← _d_ → _l_  _H_ ← _D_ → _L_  _a_ ← _K_ → _e_
       ^^^↓^^^          ^^^↓^^^          ^^^↓^
       ^^_j_^^          ^^_n_^^          ^^_G_
  ╭──────────────────────────────────────────────────────────────────────────────╯
                           [_q_]: quit, [_<SPC>_]: center
          "
          ("j" scroll-down-in-place)
          ("k" scroll-up-in-place)
          ("l" forward-char)
          ("d" delete-char)
          ("h" backward-char)
          ("L" forward-word)
          ("H" backward-word)
          ("u" scroll-up-command)
          ("n" scroll-down-command)
          ("D" delete-word-at-point)
          ("a" mwim-beginning-of-code-or-line)
          ("e" mwim-end-of-code-or-line)
          ("g" beginning-of-buffer)
          ("G" end-of-buffer)
          ("K" kill-whole-line)
          ("(" backward-list)
          (")" forward-list)
          ("y" yank-inner-sexp)
          ("." backward-forward-next-location)
          ("," backward-forward-previous-location)
          ("/" avy-goto-char :exit t)
          ("<SPC>" recenter-top-bottom)
          ("q" nil))

        (defhydra hydra-avy (:color pink :hint nil)
          "
                                                                        ╔════════╗
        ^^Goto^^        Kill^^        Yank^^        Move^^        Misc            ║  Jump  ║
  ──────────────────────────────────────────────────────────────────────╨────────╜
    _c_ ← char^^        [_k_] region  [_y_] region  [_m_] region  [_n_] line number
    _a_ ← char2 → _b_   [_K_] line    [_Y_] line    [_M_] line    [_v_] Goto viewer
    _w_ ← word  → _W_   [_z_] zap^^^^                             [_o_] Goto clock
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
          ("o" org-clock-jump-to-current-clock :exit t)
          ("z" avy-zap-to-char-dwim :exit t)
          ("v" hydra-viewer/body :exit t)
          ("<SPC>" avy-resume :exit t)
          ("o" org-clock-jump-to-current-clock :exit t)
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
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  ;; format on save
  (add-hook 'c-mode-hook '(lambda() (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
  (add-hook 'c++-mode-hook '(lambda() (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
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
  (recentf-max-saved-items 20000000)
  (recentf-auto-cleanup 'never)
  (recentf-exclude '((expand-file-name package-user-dir)
                     ".cache"
                     "cache"
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

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-x C-r") 'counsel-recentf)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   web browser                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; counsel-web
;; ivy系の検索でweb検索ができる
;; todo : migemoと連携
(use-package counsel-web
  :ensure t
  :defer t
  :functions counsel-web-map
  :bind
  ("C-c w" . counsel-web-map/body)
  :config
  (setq counsel-web-engine 'google)
  (setq counsel-web-search-dynamic-update t)
  (setq counsel-web-search-action #'browse-url)
  :init
  (with-eval-after-load 'hydra
    (defhydra counsel-web-map (:color pink :hint nil)
     "
                                 ╔═════════╗
    Key^^^^^^                          ║ browser ║
  ───────────────────────────────╨─────────╜
     _w_: suggest
     _s_: search
     _._: at-point
  ╭───────────────────────────────────────╯
     [_q_]: quit
     "
      ("w" counsel-web-suggest)
      ("s" counsel-web-search)
      ("." counsel-web-thing-at-point)
      ("q" nil)
      ))
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

;; rainbow-delimiter
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
)

;; Color Identifiers Mode
(use-package color-identifiers-mode
  :ensure t
  :defer t
  :config
  (add-hook 'after-init-hook 'global-color-identifiers-mode)
)

;; dimmer
(use-package dimmer
  :ensure t
  :custom
  (dimmer-fraction 0.2)
  :config
  (dimmer-mode t)
)

;; highlight-symbol
(use-package highlight-symbol
  :defer t
  :functions highlight-symbol
  :bind
  ("M-o h" . highlight-symbol/body)
  :init
  (with-eval-after-load 'hydra
    (defhydra highlight-symbol (:color pink :hint nil)
     "
                                 ╔══════════════════╗
    Key^^^^^^                          ║ highlight symbol ║
  ───────────────────────────────╨──────────────────╜
     _h_: highlight
     _p_: preview
     _n_: next
  ╭─────────────────────────────────────────────╯
     [_q_]: quit
     "
      ("h" highlight-symbol)
      ("p" highlight-symbol-prev)
      ("n" highlight-symbol-next)
      ("q" nil)
      )))

;; ace-window
(use-package ace-window
  :bind
  ("C-x o" . ace-window)
  :config
  (setq aw-keys '(?j ?k ?l ?u ?i ?o ?h ?y ?n))
  :custom-face
  (aw-leading-char-face ((t (:height 4.0 :foreground "#f1fa8c"))))
 )

;; projectile
(use-package projectile
  :diminish
  :bind
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1))

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

;; 最大化 <=> 元に戻す
(global-set-key (kbd "<f2>") 'toggle-frame-maximized)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 set color theme                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package modus-themes
  :ensure t                        ; omit this to use the built-in themes
  :init
  ;; Add all your customizations prior to loading the themes
  ;;(setq modus-themes-italic-constructs t
  ;;      modus-themes-bold-constructs nil
  ;;      modus-themes-region '(bg-only no-extend))
  (setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-mixed-fonts t
      modus-themes-subtle-line-numbers t
      modus-themes-mode-line '(moody borderless)
      modus-themes-syntax nil
      modus-themes-paren-match '(bold intense)
      modus-themes-diffs 'deuteranopia
      modus-themes-org-blocks 'gray-background
      modus-themes-variable-pitch-ui t
      modus-themes-variable-pitch-headings t
      modus-themes-scale-headings t
      modus-themes-scale-1 1.1
      modus-themes-scale-2 1.15
      modus-themes-scale-3 1.21
      modus-themes-scale-4 1.27
      modus-themes-scale-title 1.33)

  ;; Load the theme files before enabling a theme (else you get an error).
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-vivendi) ;; OR (modus-themes-load-operandi)
  :bind ("<f5>" . modus-themes-toggle))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(modus-themes)))
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

