
(add-to-list 'load-path (concat user-emacs-directory "elisp"))

(require 'basic)
(require 'packagecnf)
(require 'ui)
(require 'keybindings)
(require 'pkgevil)
(require 'completion)
(require 'base-prog)
(require 'asdf)

;; Autoclose bracket
(electric-pair-mode 1)

(defun open-init-file ()
  "Open init file of the Emacs."
  (interactive)
  (find-file "~/.dotfiles/emacs-new/.config/emacs/renew/init.el"))

(asdf-enable)
(straight-use-package 'nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(setq nov-text-width t)
(setq visual-fill-column-center-text t)
(add-hook 'nov-mode-hook 'visual-line-mode)
(add-hook 'nov-mode-hook 'visual-fill-column-mode)

(straight-use-package 'eshell-git-prompt)

(eshell-git-prompt-use-theme 'multiline2)

(defun ayan/configure-eshell ()
  ;; save command hisory when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'consult-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size 10000
	eshell-buffer-maximum-lines 10000
	eshell-hist-ignoredups t
	eshell-scroll-to-bottom-on-input t))

(add-hook 'eshell-first-time-mode-hook 'ayan/configure-eshell)


(toggle-frame-maximized)

(straight-use-package 'poke-line)
(poke-line-global-mode)

(provide 'init)
;;; init.el ends here
