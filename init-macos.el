;; Configurations for MacOS

(setq insert-directory-program "/opt/homebrew/bin/gls") ;; https://stackoverflow.com/a/56096775/436552

(set-face-attribute 'default nil :height 140)

(setq mac-pass-option-to-system 1)

;; (when (eq system-type 'darwin)
;;   ;; MacOS prefent flickering for emacs-mac, on Homebrew
;;   ;(add-to-list 'default-frame-alist '(inhibit-double-buffering . t)

;; (setq mac-option-key-is-meta nil
;;       mac-command-key-is-meta t
;;       mac-command-modifier 'meta
;;       mac-option-modifier 'none)
;;   ;; MacOS Keybinds for emacs-mac, on Homebrew
;;   ;; https://emacs.stackexchange.com/a/62229/31880
(global-set-key [(meta \`)] 'other-frame) ;; Only works on non-dead-keys keyboard
(global-set-key [(meta n)] 'make-frame)
;; (global-set-key [(meta w)] 'delete-frame)
(global-set-key [(meta q)] 'kill-emacs)
(global-set-key [(meta s)] 'save-buffer)
(global-set-key [(meta c)] 'kill-ring-save)
(global-set-key [(meta v)] 'yank)
(global-set-key [(meta z)] 'undo)

(global-set-key (kbd "M-t") 'tab-bar-new-tab)
(global-set-key (kbd "M-w") 'tab-bar-close-tab)
(global-set-key (kbd "s-{") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "s-}") 'tab-bar-switch-to-next-tab)

;; (unless (package-installed-p 'quelpa)
;;   (with-temp-buffer
;;     (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
;;     (eval-buffer)
;;     (quelpa-self-upgrade)))

;; (use-package quelpa)

;; (quelpa
;;  '(queue :version original :fetcher url
;;          :url "https://github.com/joaotavora/mac-key-mode/blob/master/mac-key-mode.el" ))


;; (quelpa '(mac-key-mode :repo "joaotavora/mac-key-mode" :fetcher github))

;; (use-package mac-key-mode
;;   :defer nil
;; :init (mac-key-mode 1))
