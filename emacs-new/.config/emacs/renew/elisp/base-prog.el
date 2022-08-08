;; Basic programming related tools

(straight-use-package 'flycheck)
(add-hook 'prog-mode-hook #'global-flycheck-mode)

(straight-use-package 'company)
(add-hook 'prog-mode-hook 'global-company-mode)

(straight-use-package 'lsp-mode)
;(add-hook 'prog-mode-hook #'lsp-deferred)

(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojurescript-mode-hook 'lsp)
(add-hook 'clojurec-mode-hook 'lsp)

;(straight-use-package 'lsp-treemacs)

(straight-use-package 'clojure-mode)
(straight-use-package 'cider)

(provide 'base-prog)
