;;; Personal configuration -*- lexical-binding: t -*-

;;============================================================================
;;                           early-init.el                                  ;;
;;============================================================================

;; GC
(setq gc-cons-threshold most-positive-fixnum)

(defvar startup/file-name-handler-alist file-name-handler-alist)
;; Defer garbage collection further back in the startup process
(setopt gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6

        ;; In noninteractive sessions, prioritize non-byte-compiled source files to
        ;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
        ;; to skip the mtime checks on every *.elc file.
        load-prefer-newer noninteractive

        ;; `use-package' is builtin since 29.
        ;; It must be set before loading `use-package'.
        use-package-enable-imenu-support t)
(setq file-name-handler-alist nil)

;; For android
(when (eq system-type 'android)
  (setenv "PATH" (format "%s:%s" "/data/data/com.termux/files/usr/bin"
                         (getenv "PATH")))
  (push "/data/data/com.termux/files/usr/bin" exec-path)
  (setopt image-scaling-factor 3))

