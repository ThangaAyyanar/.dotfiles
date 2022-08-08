
;(require 'package)

;(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ;("org" . "https://orgmode.org/elpa/")
			 ;("elpa" . "https://elpa.gnu.org/packages/")))

;(package-initialize)
;(unless package-archive-contents
  ;(package-refresh-contents))

;; Bootstrap for straight Package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(message "Package config file loaded")

(provide 'packagecnf)


