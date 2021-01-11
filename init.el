(setq inhibit-startup-message t)

(menu-bar-mode -1)	;; Removes menu
(tool-bar-mode -1)	;; Removes toolbar
(scroll-bar-mode -1)	;; Removes scrollbar
(global-linum-mode 1)	;; Add line number

(add-to-list 'before-save-hook 'delete-trailing-whitespace) ;; Remove whitespaces before save file


(define-key global-map (kbd "RET") 'newline-and-indent) ;; Auto-indent

(set-face-attribute 'default nil :font "Menlo-16")

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Initialize package sources
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))


(require 'use-package)
(setq use-package-always-ensure t)

(use-package gruvbox-theme
  :ensure t
  :init
  (load-theme 'gruvbox-dark-medium t)
  :config
  (show-paren-mode t)
  (setq show-paren-style 'expression))
