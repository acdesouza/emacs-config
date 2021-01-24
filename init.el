(setq inhibit-startup-message t)

(menu-bar-mode -1)	;; Removes menu
(tool-bar-mode -1)	;; Removes toolbar
(scroll-bar-mode -1)	;; Removes scrollbar
(global-linum-mode 1)	;; Add line number

(setq column-number-mode t) ;; Show current column

;; Remove whitespaces before save file
(add-to-list 'before-save-hook 'delete-trailing-whitespace)


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


;; Theme
(use-package gruvbox-theme
  :ensure t
  :init
  (load-theme 'gruvbox-dark-medium t)
  :config
  (show-paren-mode t)
  (setq show-paren-style 'expression))


;; Status bar

;; I don't want to download new fonts for this
;; (use-package all-the-icons)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config (progn
	    (setq doom-modeline-unicode-fallback t
		  doom-modeline-icon nil ;; (display-graphic-p)
		  doom-modeline-minor-modes t)))



;; Project
;; EDE and CEDET don't work with Ruby
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

;; IDO
;; https://www.gnu.org/software/emacs/manual/html_mono/ido.html

(use-package ido
  :ensure t
  :init (progn
	  (ido-mode t)
	  (use-package flx-ido
	    :init (flx-ido-mode 1)
	    :ensure t
	    ;; https://github.com/lewang/flx#memory-usage
	    :config (setq gc-cons-threshold 20000000))
	  (use-package ido-grid-mode
	    :ensure t
	    :init (ido-grid-mode t))))



;; Ruby
;; Rinari
;; - open rails server inside emacs to have a link on error stacktrace
;; - extract selected code to a partial

;; Enhanced Ruby mode
;; It uses the Ripper class found in ruby 1.9.2 (and later) to parse and indent the source code
(use-package enh-ruby-mode
  :ensure t
  :defer t
  :mode (("\\.rb\\'"       . enh-ruby-mode)
         ("\\.ru\\'"       . enh-ruby-mode)
	 ("\\.jbuilder\\'" . enh-ruby-mode)
         ("\\.gemspec\\'"  . enh-ruby-mode)
         ("\\.rake\\'"     . enh-ruby-mode)
         ("Rakefile\\'"    . enh-ruby-mode)
         ("Gemfile\\'"     . enh-ruby-mode)
         ("Guardfile\\'"   . enh-ruby-mode)
         ("Capfile\\'"     . enh-ruby-mode)
         ("Vagrantfile\\'" . enh-ruby-mode))
  :config (progn
	    (setq enh-ruby-indent-level 2
		  enh-ruby-add-encoding-comment-on-save nil
		  enh-ruby-deep-indent-paren nil
		  enh-ruby-bounce-deep-indent t
		  enh-ruby-hanging-indent-level 2)
	    (setq enh-ruby-program "/usr/bin/ruby")
	    (setq ruby-insert-encoding-magic-comment nil)))
