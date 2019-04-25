;;tell emacs where the personal load path directory is
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; convenience stuff
(global-set-key (kbd "<C-tab>") 'other-window)
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)
(setq column-number-mode t)
(delete-selection-mode 1)		;"insert" key
(show-paren-mode 1) 			;highlight matching parenthesis
(display-time-mode 1)                   ;show the time in the modeline
(global-set-key (kbd "C-c w") 'toggle-truncate-lines)
(setq-default fill-column 80)		;wrap at 80 columns for all text modes

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-exclude '("/org/"))
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(setq latex-run-command "latexmk")
(setq tex-start-commands "")
(setq sentence-end-double-space nil)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; org stuff (make sure org is enabled and org-agenda-files is set to
;; the correct path)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)))

(global-set-key (kbd "C-x g") 'magit-status)
(add-hook 'git-commit-setup-hook
     (lambda ()
       (flyspell-mode 1)
       )
     )

(setq dired-listing-switches "-ahl --group-directories-first")

(add-hook 'c-mode-common-hook
	  (lambda () (define-key c-mode-base-map (kbd "C-c C-f") 'recompile)))

(setq compilation-scroll-output t)

(provide 'EmacsConfig)
