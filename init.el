;; Use setq-default to set the common behavior
;; https://stackoverflow.com/a/18173666/436552

(setq inhibit-startup-message t)

(menu-bar-mode -1)   ;; Removes menu
(tool-bar-mode -1)   ;; Removes toolbar
(scroll-bar-mode -1) ;; Removes scrollbar

(global-linum-mode 1)  ;; Add line number
(column-number-mode t) ;; Show current column
(setq-default mode-line-percent-position nil) ;; Removes file percent position(Top % Bot)

(setq-default scroll-step 1)    ;; Don't crazy jump after a one line scroll

(setq-default truncate-lines 1) ;; Stop wrapping lines


;; Remove whitespaces before save file
(add-to-list 'before-save-hook 'delete-trailing-whitespace)


;; Don't use a TAB character(ASCII byte #9) for indentation
;; https://www.jwz.org/doc/tabs-vs-spaces.html
(setq-default indent-tabs-mode nil
      c-basic-offset 2
      tab-width 4)
;; https://www.emacswiki.org/emacs/UntabifyUponSave
;; if indent-tabs-mode is off, untabify before saving
(add-hook 'write-file-hooks
          (lambda () (if (not indent-tabs-mode)
                         (untabify (point-min) (point-max)))
            nil ))


(define-key global-map (kbd "RET") 'newline-and-indent) ;; Auto-indent

;; Auto revert-buffer after changed outside
;; https://www.emacswiki.org/emacs/RevertBuffer
(global-auto-revert-mode 1)

(set-face-attribute 'default nil
            :family "Menlo"
            :height 120
            :weight 'normal
            :width  'normal)

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
(custom-set-variables '(use-package-always-ensure t))
(custom-set-variables '(use-package-always-defer t))
(custom-set-variables '(use-package-verbose t))

;; Evil Mode
;; https://github.com/emacs-evil/evil
(use-package evil
  :defer nil
  :config
  (progn
    (add-to-list 'evil-buffer-regexps
               '("\\*xref\\*" . emacs))

    ;; set leader
    (evil-set-leader 'visual (kbd "\\"))
    (evil-define-key 'visual 'global (kbd "<leader>c") 'comment-or-uncomment-region))
  :init
  (setq evil-want-C-i-jump nil)
  (evil-mode 1))



;; Themes

;; all-the-icons
;; Used by:
;;   - doom-modeline
;;   - rg-agenda
;; https://github.com/domtronn/all-the-icons.el
(use-package all-the-icons)

;; gruvbox-theme
;; https://github.com/greduan/emacs-theme-gruvbox
;; (use-package gruvbox-theme
;;   :init
;;   (load-theme 'gruvbox-dark-medium t)
;;   :config (progn
;;      (show-paren-mode t)
;;      (setq show-paren-style 'expression)))

;; doom-themes
;; https://github.com/hlissner/emacs-doom-themes
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :init
  (load-theme 'doom-molokai t))



;; Matching Parentheses
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Matching.html
(show-paren-mode t)
(setq show-paren-style 'parenthesis)

;; Status bar
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config (progn
            (setq doom-modeline-unicode-fallback t
                  doom-modeline-icon (display-graphic-p)
                  doom-modeline-minor-modes t
                  doom-modeline-buffer-file-name-style 'relative-from-project)
            (advice-add #'fit-window-to-buffer :before (lambda (&rest _) (redisplay t)))))


;; Org-Mode
(use-package org
  :bind (("C-c a" . org-agenda)
     ("C-c c" . org-capture))
  :mode ("\\.org$" . org-mode)
  :hook
  (org-mode . (lambda ()
                (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
                (visual-line-mode)))
  :config
  (setq org-todo-keywords '((sequence "TODO(t)" "STARTED(s!)" "WAITING(w@/!)" "|" "DONE(d@/!)" "CANCELED(c@)"))
        ;; https://florianwinkelbauer.com/posts/2020-07-13-org-agenda-icons/
        org-agenda-category-icon-alist
        `(
          ("@inbox" ,(list (all-the-icons-faicon "inbox" :face 'all-the-icons-orange)) nil nil :ascent center)
          ("@home" ,(list (all-the-icons-faicon "home" :face 'all-the-icons-green)) nil nil :ascent center)
          ("@work" ,(list (all-the-icons-faicon "briefcase" :face 'all-the-icons-blue)) nil nil :ascent center)
          )
        org-agenda-ndays 7
        org-agenda-start-on-weekday nil
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-directory "~/org"
        org-agenda-files (list org-directory)
        org-agenda-include-diary t
        org-default-notes-file (concat org-directory "/notes.org")
        org-refile-targets
        '((nil :maxlevel . 1)
          (org-agenda-files :maxlevel . 1))
        org-log-into-drawer "LOGBOOK"
        ;; https://orgmode.org/manual/Template-expansion.html
        org-capture-templates
        '(("t" "Task" entry
           (file+headline "~/org/notes.org" "Inbox")
           "** TODO %^{Task}
SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))
:PROPERTIES:
:CREATED: %U
:END:"
           :empty-lines-after 1)
          ("m" "Meeting" entry
           (file+headline "~/org/notes.org" "Meetings")
           "** %^{Meeting}
%^{Starting at}T
:PROPERTIES:
:CREATED: %U
:END:"
           :empty-lines-after 1)
          ("l" "Read/Watch it Later" entry
           (file+headline "~/org/notes.org" "Read it Later")
           "** TODO [[%^{URL}][%^{Title}]]
:PROPERTIES:
:CREATED: %U
:END:"
           :empty-lines-after 1)
          )))



;; Projectile
;; https://github.com/bbatsov/projectile
;; * EDE and CEDET don't work with Ruby
;; * Ag: https://docs.projectile.mx/projectile/usage.html#installing-external-tools
(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map))
  :config (progn
            (setq projectile-switch-project-action #'projectile-dired)
            ;; https://tecosaur.github.io/emacs-config/config.html#window-title
            (defun change-frame-name-on-project-change ()
              (setq frame-title-format
                    '(""
                      (:eval
                        (if (s-contains-p org-roam-directory (or buffer-file-name ""))
                          (replace-regexp-in-string
                            ".*/[0-9]*-?" "☰ "
                            (subst-char-in-string ?_ ?  buffer-file-name))
                          "%b"))
                      (:eval
                        (let ((project-name (projectile-project-name)))
                          (unless (string= "-" project-name)
                            (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))
              ))
  :hook
  ((projectile-after-switch-project . change-frame-name-on-project-change)))


;; IDO
;; https://www.gnu.org/software/emacs/manual/html_mono/ido.html
;; https://docs.projectile.mx/projectile/usage.html#minibuffer-completion
(use-package ido
  :init (progn
      (ido-mode t)
      (use-package flx-ido
        :init (flx-ido-mode 1)
        :ensure t
        ;; https://github.com/lewang/flx#memory-usage
        :config (progn
              (setq gc-cons-threshold 20000000)
              (setq ido-enable-flex-matching t)))
      (use-package ido-grid-mode
        :ensure t
        :init (ido-grid-mode t))))



;; dired-sidebar
;; current directory tree-view
;; https://github.com/jojojames/dired-sidebar
(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar)
  :init
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-theme 'nerd
    dired-sidebar-use-term-integration t
    dired-sidebar-use-custom-font t
    dired-listing-switches "-la --group-directories-first") ;; https://www.emacswiki.org/emacs/DiredSortDirectoriesFirst
  :hook(dired-sidebar-mode . (lambda ()
          (unless (file-remote-p default-directory)
                (auto-revert-mode)))))



;; dumb-jump
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Find-Identifiers.html
;; https://github.com/jacktasia/dumb-jump
(use-package dumb-jump
  :defer nil
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))



;; Ruby
(use-package ruby-mode
  :config (setq ruby-deep-arglist nil)) ;; https://www.rubydoc.info/gems/rubocop/RuboCop/Cop/Layout/ArgumentAlignment with_fixed_indentation

;; Ruby Electric
;; Autoclose paired syntax elements like parens, quotes, etc
;; https://github.com/ruby/elisp-ruby-electric/tree/f2323cd9b5df3b34aa9810ba8109502824925d23
(use-package ruby-electric
  :after (ruby-mode)
  :config (add-hook 'ruby-mode-hook 'ruby-electric-mode))

;; Rails
;; https://github.com/asok/projectile-rails
(use-package projectile-rails
  :after (projectile)
  :bind
  (:map projectile-rails-mode-map
    ("C-c r" . projectile-rails-command-map))
  :config
  (evil-ex-define-cmd "AS" '(lambda ()
                  ;; https://github.com/antono/evil-rails
                  (interactive)
                  (evil-window-split)
                  (windmove-down)
                  (projectile-toggle-between-implementation-and-test)))
  ;; Only needed if projectile identifies a project
  :hook (projectile-after-switch-project . projectile-rails-global-mode))

;; RVM
;; https://github.com/senny/rvm.el
;; Sets the right ruby version and gemset.So, I can C-c r r
;; to open a rails console for the current project
(use-package rvm
  :after (projectile)
  :hook (
  (projectile-after-switch-project . rvm-activate-corresponding-ruby)))

;; YAML Mode
;; https://github.com/yoshiki/yaml-mode
(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)))



;; web-mode
;; https://github.com/fxbois/web-mode
(use-package web-mode
  :commands (web-mode)
  :config
  (setq web-mode-extra-auto-pairs '(("erb"  . (("beg" "end"))))
    web-mode-extra-expanders  '(("-/" . "<% | %>")
                    ("=/" . "<%= | %>"))

    web-mode-enable-auto-expanding t
    web-mode-enable-auto-pairing t
    web-mode-enable-current-element-highlight t
    web-mode-markup-indent-offset 2
    web-mode-css-indent-offset 2
    web-mode-code-indent-offset 2)
  :mode (("\\.html\\'" . web-mode)
;;   ("\\.js\\'"   . web-mode) Loses Jump To Definition. Maybe, take a look at js2-mode?
     ("\\.erb\\'"  . web-mode)))

;; js-mode
;; https://www.emacswiki.org/emacs/JavaScriptMode
(use-package js
  :config
	(setq js-indent-level 2))

;; slim-mode
;; https://github.com/slim-template/emacs-slim
(use-package slim-mode)

;; sass-mode
;; http://github.com/nex3/haml-mode
(use-package sass-mode
  :mode ("\\.sass\\'" . sass-mode))

(use-package haml-mode)





(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(when (eq system-type 'darwin)
  (setq macos-config-file (concat user-emacs-directory "init-macos.el"))
  (when (file-exists-p macos-config-file)
    (load macos-config-file)))
