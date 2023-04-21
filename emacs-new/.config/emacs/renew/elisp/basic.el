;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Rational Emacs loaded in %s."
                     (emacs-init-time))))

;; Remove startup screen
(setq inhibit-startup-message t)

;; Turn off scroll bar, menu bar, and tool tip
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(set-fringe-mode 10)

;; No autoback up and lock files
(setq
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)

(fset 'yes-or-no-p 'y-or-n-p)

;; Enabling Number line
(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

;; Disable line number for some mode
(dolist (mode '(pdf-view-mode-hook
                nov-mode-hook
                treemacs-mode-hook
		shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; adding Environment path
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; Disable line number for some mode
;(dolist (mode '(term-mode-hook
                ;shell-mode-hook
                ;pdf-view-mode-hook
                ;nov-mode-hook
                ;org-mode-hook
                ;treemacs-mode-hook
                ;eshell-mode-hook))
  ;(add-hook mode (lambda () (display-line-numbers-mode 0))))

(message "Basic file loaded")

;; To import this file in init.el
(provide 'basic)
