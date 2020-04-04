;;tell emacs where the personal load path directory is
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; convenience stuff
(global-set-key (kbd "<C-tab>") 'other-window)
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)
(setq column-number-mode t)
(delete-selection-mode 1)
(show-paren-mode 1)			;highlight matching parenthesis
(global-set-key (kbd "C-c b") 'blink-matching-open)
(display-time-mode 1)
(global-set-key (kbd "C-c w") 'toggle-truncate-lines)
(setq dired-listing-switches "-ahl")
(global-set-key (kbd "C-c m c") 'mc/edit-lines)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-exclude '("/org/"))
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(setq-default fill-column 80)
(setq org-latex-compiler "xelatex")
(setq latex-run-command "latexmk")
(setq tex-start-commands "")
(setq sentence-end-double-space nil)

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-default-notes-file (concat org-directory '"/notes.org"))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C          . t)
   (emacs-lisp . t))
 )

(global-set-key (kbd "C-x g") 'magit-status)
(add-hook 'git-commit-setup-hook
	  (lambda () (flyspell-mode 1)))

(add-hook 'c-mode-common-hook
	  (lambda () (define-key c-mode-base-map (kbd "C-c C-f") 'recompile)))
(setq compilation-scroll-output t)

(add-hook
 'dired-before-readin-hook
 (lambda ()
   (when (file-remote-p default-directory)
     (setq dired-actual-switches "-al"))))

(require 'use-package)

(use-package company :ensure t
  :config
  (progn
    (setq company-minimum-prefix-length 2
          company-selection-wrap-around t
          company-show-numbers t
          company-tooltip-align-annotations t
          company-require-match nil
          company-transformers '(company-sort-by-occurrence)
          company-idle-delay 0.1)
    (global-company-mode)))

(use-package company-ghci :ensure t
 :config
  (progn
    (push 'company-ghci company-backends)
    (add-hook 'haskell-mode-hook 'company-mode)
    (add-hook 'haskell-interactive-mode-hook 'company-mode)))

(use-package powerline :ensure t
  :config
  (powerline-default-theme))

(use-package projectile :ensure t
  :config
  (progn
    (projectile-global-mode)
    (defun set-gopath-smart ()
      "Reset GOPATH if a vendor dir exists in the project root"
      (let ((vendor-dir (expand-file-name "vendor" (projectile-project-root))))
        (when (file-exists-p vendor-dir)
          (setenv "GOPATH" (concat vendor-dir path-separator (getenv "GOPATH"))))))
    (add-hook 'projectile-after-switch-project-hook 'set-gopath-smart))
  :bind
  (("C-c p h" . projectile-find-file)
   ("C-c p s" . projectile-switch-project)))

(use-package p4 :ensure t)

(use-package omnisharp :ensure t)

(eval-after-load
    'company
  '(add-to-list 'company-backends 'company-omnisharp))
(add-hook 'csharp-mode-hook #'company-mode)

(provide 'EmacsConfig)
