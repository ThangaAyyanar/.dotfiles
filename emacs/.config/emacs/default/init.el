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
(dolist (mode '(term-mode-hook
                shell-mode-hook
                pdf-view-mode-hook
                nov-mode-hook
                org-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 165)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(require 'hl-line)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)
(set-face-attribute 'hl-line nil :background "gray21")

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use package on non-linux based system
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;(setq use-package-verbose t)

;; (use-package auto-package-update
  ;; :custom
  ;; (auto-package-update-interval 7)
  ;; (auto-package-update-prompt-before-update t)
  ;; (auto-package-update-hide-results t)
  ;; :config
  ;; (auto-package-update-maybe)
  ;; (auto-package-update-at-time "11:00"))

(defun toggle-maximize-buffer () "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_) 
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

;; (setq erc-server "irc.libera.chat"
;;       erc-nick "GoldAyan"    ; Change this!
;;       erc-user-full-name "GoldAyan"  ; And this!
;;       erc-track-shorten-start 8
;;       erc-autojoin-channels-alist '(("irc.libera.chat" "#systemcrafters" "#emacs"))
;;       erc-kill-buffer-on-part t
;;             erc-auto-query 'bury)

(defun ayan/my-nov-font-setup ()
  (face-remap-add-relative 'variable-pitch :family "Source Sans Pro Semibold"
                                           :height 1.3))

(use-package nov
    :defer t 
    :init
    (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
    :config
    (setq nov-text-width t)
    (setq visual-fill-column-center-text t)
    (add-hook 'nov-mode-hook 'visual-line-mode)
    (add-hook 'nov-mode-hook 'visual-fill-column-mode)
    (add-hook 'nov-mode-hook 'ayan/my-nov-font-setup)
      )

(use-package pdf-tools
  :ensure t
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :config
  ;; initialise
  (pdf-tools-install)
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-page)
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t)
  ;; more fine-grained zooming
  (setq pdf-view-resize-factor 1.1)
  :hook
  (pdf-view-mode-hook . pdf-tools-enable-minor-modes))

(use-package verb
  :mode ("\\.org\\'" . org-mode))

(use-package speed-type
  :commands (speed-type-top-100 speed-type-top-1000)
)

(use-package spray
  :commands spray-mode
)

(use-package command-log-mode
  :commands command-log-mode
)

(use-package swiper
  :bind ("M-s" . swiper))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package ivy
  :ensure t
  :diminish
  :bind (:map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package counsel-dash
  :defer t
  :after counsel
  :init
  (add-hook 'swift-mode-hook (lambda () (setq-local counsel-dash-docsets '("Swift"))))
  (add-hook 'python-mode-hook (lambda () (setq-local counsel-dash-docsets '("Python 3"))))
  (add-hook 'js-mode-hook (lambda () (setq-local counsel-dash-docsets '("React"))))
  :config
  (setq counsel-dash-docsets-path "~/.docsets")
  (setq counsel-dash-common-docsets '("Bash"))
)

(use-package perspective
  :after counsel
  :config
  (persp-mode))
;; use C-x x to learn more about perspective

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

;; which key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package hydra
  :defer t)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package doom-themes
  :init (load-theme 'doom-oceanic-next t))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(use-package all-the-icons)

;; This function ensure this mode should start in insert mode
(defun ayan/evil-hook ()
  (dolist (mode '(eshell-mode
                  term-mode))
  (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  ;;(setq evil-want-C-i-jump nil)
  :custom
  (evil-vsplit-window-right t)
  (evil-split-window-below t)
  :config
  (add-hook 'evil-mode-hook 'ayan/evil-hook)
  (evil-mode 1)
  ;; Normal key mapping
  (define-key evil-insert-state-map (kbd "C-[") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "RET") nil)
  ;; (define-key evil-motion-state-map (kbd "SPC") nil)
  ;; (define-key evil-motion-state-map (kbd "TAB") nil)
)

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode +1)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  ;;(when (file-directory-p "~/Programs")
  (setq projectile-project-search-path '("~/Programs"
                                         ))
)

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode)
  :init (setq projectile-switch-project-action #'counsel-projectile-find-file))

(defun ayan/org-mode-setup ()
  (org-indent-mode)
  ;;(variable-pitch-mode 1)
  (visual-line-mode 1)
)

(use-package org
  :pin org
  :commands (org-capture org-agenda)
  :hook (org-mode . ayan/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-return-follows-link t)
  (setq org-agenda-files
        '("~/cloud/Dropbox/Tasks.org"
          "~/Documents/My Library/OrgFiles/Habits.org"
          "~/Zoho WorkDrive (Enterprise)/My Folders/Sync/Tasks.org"
          ;; "~/Documents/My Library/OrgFiles/Tasks.org"
          ))

  ;; org habit (set the propert to habit to track it)
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)" "DELEGATED(e)")
          (sequence "BACKLOG(b)" "PLAN(p)" "ISSUE(i)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "DELEGATED(d)" "COMPLETED(c)" "CANC(k@)")))

  ;; Archiving tags
  (setq org-refile-targets
        '( ;; ("Tasks.org" :maxlevel . 1)
          ("~/Zoho WorkDrive (Enterprise)/My Folders/Sync/Archive.org" :maxlevel . 4)
          ;;("OTHERFILE.org" :maxlevel . 2)
          ))

  ;; save the all org file after the refile is done
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

    ;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("W" "Work Tasks" tags-todo "+work-email")

    ;; Low-effort next actions
    ;; Org set effort related stuff (org properties for the header)
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))

    ;; Org default tags
    ;; <C-c C-q> tag addition view
    (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@work" . ?W)
       ("agenda" . ?a)
       ("planning" . ?p)
       ("publish" . ?P)
       ("batch" . ?b)
       ("note" . ?n)
       ("idea" . ?i)))

    ;; Templates (org-capture)
    (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/Documents/My Library/OrgFiles/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "~/Documents/My Library/OrgFiles/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree "~/Documents/My Library/OrgFiles/Journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

      ("w" "Workflows")
      ("we" "Checking Email" entry (file+olp+datetree "~/Documents/My Library/OrgFiles/Journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

      ("m" "Metrics Capture")
      ("mw" "Weight" table-line (file+headline "~/Documents/My Library/OrgFiles/Metrics.org" "Weight")
       "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  )

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Distraction free writing
(defun ayan/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . ayan/org-mode-visual-fill))

(with-eval-after-load 'org
  (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
        (python . t)
        (plantuml . t) 
        (dot . t) 
        ))
  (setq org-plantuml-jar-path "~/Scripts/Binary/plantuml.jar")
)
;; don't ask me conformation y/n when execute code
(setq org-confirm-babel-evaluate nil)

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

(with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
)

;; This allows you to tell org-babel to tangle you org files after
;; save by placing:
;; #+CATEGORY: Configuration
;; in the head of the org file.
;; Change this search key/term to what best suits the individual
(setq efs/tangle-search-key "CATEGORY")
(setq efs/tangle-search-term "Configuration")

(defun efs/org-global-props (&optional efs/property)
  "Get the plists of global org properties of current buffer."
  (interactive)
  (unless efs/property (setq efs/property "PROPERTY"))
  (org-element-map (org-element-parse-buffer)
      'keyword (lambda (el) (when (string-match efs/property (org-element-property :key el)) el))))

(defun efs/org-global-prop-value (key)
  "Get global org property KEY of current buffer."
  (org-element-property :value (car (efs/org-global-props key))))

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  "Used to tangle org file when it change."
  (when (or (string-equal (file-name-directory (buffer-file-name))
                          (expand-file-name user-emacs-directory))
            (string-equal (efs/org-global-prop-value efs/tangle-search-key) efs/tangle-search-term))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

;; Automatically tangle our Emacs.org config file when we save it
;; (defun efs/org-babel-tangle-config ()
;;   (when (string-equal (file-name-directory (buffer-file-name))
;;                       (expand-file-name user-emacs-directory))
;;     ;; Dynamic scoping to the rescue
;;     (let ((org-confirm-babel-evaluate nil))
;;       (org-babel-tangle))))

;; (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(use-package org-roam
      :init
      (setq org-roam-v2-ack t)
      :custom
      (org-roam-directory (file-truename "~/Zoho WorkDrive (Enterprise)/My Folders/Documentation"))
       (org-roam-completion-everywhere t)
      :bind (("C-c n l" . org-roam-buffer-toggle)
             ("C-c n f" . org-roam-node-find)
             ("C-c n i" . org-roam-node-insert)
             :map org-mode-map
             ("C-M-i"    . completion-at-point))
      :config
         (org-roam-setup))

(use-package websocket
    :after org-roam)

(use-package org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;; (use-package org-alert
;;   :custom (alert-default-style 'notifications)
;;   :config
;;   (setq org-alert-interval 300
;;         alert-default-style 'osx-notifier
;;         org-alert-notification-title "Org Alert Remainder!")
;;   (org-alert-enable)
;; )

(use-package cheat-sh
  :defer t
)

(use-package swift-mode
 :mode "\\.swift\\'"
;; :hook (swift-mode . lsp-deferred)
)

(defun xcode-build()
  (interactive)
  (shell-command-to-string
     "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'build targetProject' -e 'end tell'"))

(defun xcode-run()
  (interactive)
  (shell-command-to-string
    "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'stop targetProject' -e 'run targetProject' -e 'end tell'"))

(defun xcode-test()
  (interactive)
  (shell-command-to-string
    "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'stop targetProject' -e 'test targetProject' -e 'end tell'"))

(setq python-shell-interpreter "/usr/local/bin/python3")

(use-package markdown-mode
   :mode "\\.md\\'"
)

(use-package ledger-mode
   :init
   (setq ledger-clear-whole-transactions 1)
   (setq ledger-reports '(("bal" "gpg -d -q %(ledger-file) | %(binary) -f - bal")
                     ("reg" "gpg -d -q %(ledger-file) | %(binary) -f - reg")
                     ("payee" "gpg -d -q %(ledger-file) | %(binary) -f - reg @%(payee)")
                     ("account" "gpg -d -q %(ledger-file) | %(binary) -f - reg %(account)")))
   :mode "\\.ledger\\'")

(defun ayan/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t)
  :hook (lsp-mode . ayan/lsp-mode-setup))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(setq lsp-ui-doc-position 'bottom)

(setq lsp-ui-sideline-enable nil)
(setq lsp-ui-sideline-show-hover nil)

(use-package company-sourcekit
:hook (swift-mode . company-sourcekit)
:config
     (setq company-sourcekit-verbose nil
              sourcekit-verbose nil
              sourcekit-sourcekittendaemon-executable "/Users/thanga-6745/Scripts/Production/sourcekittend")
              (add-to-list 'company-backends 'company-sourcekit))

;; (use-package lsp-sourcekit
;;   :hook (switf-mode . lsp-deferred)
;;   :config
;;   (setq lsp-sourcekit-executable (string-trim (shell-command-to-string "xcrun --find sourcekit-lsp"))))

(use-package lsp-pyright
   :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

;; (use-package company-box
;;  :hook (company-mode . company-box-mode))

(use-package lsp-treemacs
  :after lsp)

(use-package evil-nerd-commenter
  :bind ("s-/" . evilnc-comment-or-uncomment-lines))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets)

(use-package term
  :commands term
  :config
  (setq explicit-shell-file-name "zsh")
  ;;(setq explicit-zsh-args '())
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  ;;(setq vterm-shell "zsh")
  (setq vterm-max-scrollback 10000))

(defun efs/configure-eshell ()
   ;; Save command history when commands are entered
   (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

   ;; Truncate buffer for performance
   (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

   ;; Bind some useful keys for evil-mode
   (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
   (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
   (evil-normalize-keymaps)

   (setq eshell-history-size         10000
         eshell-buffer-maximum-lines 10000
         eshell-hist-ignoredups t
         eshell-scroll-to-bottom-on-input t))

 (use-package eshell
   :hook (eshell-first-time-mode . efs/configure-eshell))

 (use-package eshell-git-prompt
   :after eshell
   :config
   (eshell-git-prompt-use-theme 'powerline))

(with-eval-after-load 'esh-opt
 (setq eshell-destroy-buffer-when-process-dies t)
 (setq eshell-visual-commands '("htop" "zsh" "vim")))

(when (eq system-type 'darwin)
  (setq insert-directory-program "/usr/local/bin/gls"))

(use-package dired
    :ensure nil
    :commands (dired dired-jump)
    ;; :bind (("C-x C-j" . dired-jump))
    :custom ((dired-listing-switches "-agho --group-directories-first"))
    :config
        (evil-collection-define-key 'normal 'dired-mode-map
            "h" 'dired-single-up-directory
            "l" 'dired-single-buffer)
)

(use-package dired-single
  :after dired
)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :after dired
  :config
  (setq dired-open-extensions '(("png" . "open")
                                ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package general
  :config
  (general-create-definer ayan/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (ayan/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme"))
)

(ayan/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(ayan/leader-keys
    "b"  '(:ignore t :which-key "buffer")
    "br" '(revert-buffer :which-key "Revert Buffer")
    "bs" '(counsel-switch-buffer :which-key "Switch Buffer")
    "bS" '(persp-switch :which-key "Perspective Switch")
    "bN" '(persp-switch-last :which-key "Perspective Switch last")

;;    "bp" '(persp-counsel-switch-buffer :which-key "Perspective Switch Buffer")
    "bw" '(counsel-switch-buffer-other-window :which-key "Switch Buffer Other Window")
    "bk" '(kill-buffer :which-key "Kill Buffer")
    "bx" '(kill-current-buffer :which-key "Kill Current Buffer")
    )

(ayan/leader-keys
    "f"  '(:ignore t :which-key "Files")
    "ff" 'counsel-find-file
    )

(ayan/leader-keys
    "g"  '(:ignore t :which-key "Git")
    "gs" '(magit-status :which-key "Status")
    )

;; Open init file function and shortcut
(defun open-init-file ()
  "Open this very file."
  (interactive)
  (find-file "~/.dotfiles/emacs/.config/emacs/default/Emacs.org"))

;; Open
(ayan/leader-keys
    "l"  '(:ignore t :which-key "Load File")
    "li" '(open-init-file :which-key "Init file")
    )

(ayan/leader-keys
    "o"  '(:ignore t :which-key "Org")
    "oa"  '(org-agenda :which-key "Org Agenda")
    "on"  '(org-narrow-to-subtree :which-key "Narrow")
    "ow"  '(widen :which-key "Widen")
    "or"  '(org-refile :which-key "Org refile")
    )

(ayan/leader-keys
    "u"  '(:ignore t :which-key "Utility")
    "ui" '(counsel-imenu :which-key "iMenu")
    "uM" '(toggle-maximize-buffer :which-key "Toggle Maximize")
    )

(ayan/leader-keys
    "p" '(:ignore t :which-key "Projectile")
    "pf" '(counsel-projectile-find-file :which-key "Find file")
    "ps" '(counsel-projectile-switch-project :which-key "Switch project")
    "pb" '(counsel-projectile-switch-to-buffer :which-key "Project buffers")
    "pg" '(counsel-projectile-rg :which-key "Grep file")
    "pq" '(projectile-switch-open-project :which-key "Switch project")
)

(ayan/leader-keys
    "h"  'help-for-help
    )

(general-define-key
 :keymaps '(override)                   ; check out `general-override-mode-map'.
 ;; Adding `nil' to the states makes these keybindings work on buffers where
 ;; they would usually not work, e.g. the *Messages* buffer or the
 ;; `undo-tree-visualize' buffer.
 :states '(normal visual insert nil)
 "M-F" #'toggle-frame-fullscreen
 ;; "M-H" 'buf-move-left
 ;; "M-J" 'buf-move-down
 ;; "M-K" 'buf-move-up
 ;; "M-L" 'buf-move-right
 "M-h" #'evil-window-left
 "M-j" #'evil-window-down
 "M-k" #'evil-window-up
 "M-l" #'evil-window-right
 )
