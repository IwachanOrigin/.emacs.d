;;; init.el --- summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Keep Customize-generated settings out of init.el.
(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))

(org-babel-load-file
 (expand-file-name "readme.org" user-emacs-directory))

;; Load machine-local Customize settings if present.
(load custom-file 'noerror 'nomessage)

;;; init.el ends here
