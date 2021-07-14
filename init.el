;; Use setq-default to set the common behavior
;; https://stackoverflow.com/a/18173666/436552

(setq inhibit-startup-message t)

(menu-bar-mode -1)	;; Removes menu
(tool-bar-mode -1)	;; Removes toolbar
(scroll-bar-mode -1)	;; Removes scrollbar

(global-linum-mode 1)  ;; Add line number
(column-number-mode t) ;; Show current column
(setq-default mode-line-percent-position nil) ;; Removes file percent position(Top % Bot)

(setq-default scroll-step 1)    ;; Don't crazy jump after a one line scroll
(setq-default truncate-lines 1) ;; Stop wrapping lines


;; Remove whitespaces before save file
(add-to-list 'before-save-hook 'delete-trailing-whitespace)


(define-key global-map (kbd "RET") 'newline-and-indent) ;; Auto-indent

;; Auto revert-buffer after changed outside
;; https://www.emacswiki.org/emacs/RevertBuffer
(global-auto-revert-mode 1)

(set-face-attribute 'default nil
		    :family "Menlo"
		    :height 120
		    :weight 'normal
		    :width  'normal)

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Initialize package sources
;; https://github.com/jwiegley/use-package
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

;; Evil Mode
;; https://github.com/emacs-evil/evil
(use-package evil
  :config (evil-mode 1))



;; Themes

;; gruvbox-theme
;; https://github.com/greduan/emacs-theme-gruvbox
;; (use-package gruvbox-theme
;;   :init
;;   (load-theme 'gruvbox-dark-medium t)
;;   :config (progn
;; 	    (show-paren-mode t)
;; 	    (setq show-paren-style 'expression)))

;; doom-themes
;; https://github.com/hlissner/emacs-doom-themes
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :init
  (load-theme 'doom-molokai t))


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
              ("C-c p" . projectile-command-map))
  :config (progn
            (setq initial-scratch-message
                  (concat "# Recent projects\n\n"
                          (mapconcat (apply-partially #'format " - %s")
                                     (projectile-relevant-known-projects) "\n")))
            (run-with-idle-timer 0.1 nil (lambda ()
                                           (with-current-buffer "*scratch*"
                                             (flyspell-mode-off)
                                             (local-set-key [mouse-1] #'ffap-at-mouse))))))



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
