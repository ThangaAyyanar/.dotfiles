
;; Install dependencies
(straight-use-package 'evil)
(straight-use-package 'undo-tree)
(straight-use-package 'evil-collection)
(straight-use-package 'evil-nerd-commenter)

;; Turn on undo-tree globally
(global-undo-tree-mode)

(setq evil-undo-system 'undo-tree)
;; `evil-collection' assumes `evil-want-keybinding' is set to
;; `nil' before loading `evil' and `evil-collection'
;; @see https://github.com/emacs-evil/evil-collection#installation
(setq evil-want-keybinding nil)

(require 'evil)
(setq evil-want-C-u-scroll t)
;; Load Evil and enable it globally
(evil-mode 1)

;; Rebind `universal-argument' to 'C-M-u' since 'C-u' now scrolls the buffer
(global-set-key (kbd "C-M-u") 'universal-argument)

(global-set-key (kbd "M-/") 'evilnc-comment-or-uncomment-lines)

;; C-h is backspace in insert state
(define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

;; Use visual line motions even outside of visual-line-mode buffers
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

;; Make sure some modes start in Emacs state
;; TODO: Split this out to other configuration modules?
(dolist (mode '(custom-mode
                eshell-mode
                term-mode))
  (add-to-list 'evil-emacs-state-modes mode))

;; Initialize evil collection
(evil-collection-init)

(provide 'pkgevil)
;;; rational-evil.el ends here
