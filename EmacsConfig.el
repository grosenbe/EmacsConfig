;;tell emacs where the personal load path directory is
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; convenience stuff
(global-set-key (kbd "<C-tab>") 'other-window)
(delete-selection-mode 1)
(show-paren-mode 1)			;highlight matching parenthesis
(global-set-key (kbd "C-c b") 'blink-matching-open)
(display-time-mode 1)
(global-set-key (kbd "C-c w") 'toggle-truncate-lines)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(global-set-key (kbd "C-c C-f") 'recompile)

(setq-default indent-tabs-mode nil)
(setq column-number-mode t
      dired-listing-switches "-ahl")
(menu-bar-mode -1)
(tool-bar-mode -1)

(defun cgr-copy-file-path (&optional @dir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
Result is full path.
If `universal-argument' is called first, copy only the dir path.

If in dired, copy the file/dir cursor is on, or marked files.

If a buffer is not file and not dired, copy value of `default-directory' (which is usually the “current” dir when that buffer was created)

URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
Version 2017-09-01"
  (interactive "P")
  (let (($fpath
         (if (string-equal major-mode 'dired-mode)
             (progn
               (let (($result (mapconcat 'identity (dired-get-marked-files) "\n")))
                 (if (equal (length $result) 0)
                     (progn default-directory )
                   (progn $result))))
           (if (buffer-file-name)
               (buffer-file-name)
             (expand-file-name default-directory)))))
    (kill-new
     (if @dir-path-only-p
         (progn
           (message "Directory path copied: %s" (file-name-directory $fpath))
           (file-name-directory $fpath))
       (progn
         (message "File path copied: %s" $fpath)
         $fpath )))))
(global-set-key (kbd "C-c g") 'cgr-copy-file-path)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-exclude '("/org/"))
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(setq-default fill-column 100)
(setq org-latex-compiler "xelatex")
(setq latex-run-command "latexmk")
(setq tex-start-commands "")
(setq sentence-end-double-space nil)

(cond ((>= emacs-major-version 25)
       (require 'package)
       (add-to-list 'package-archives
                    '("melpa" . "https://melpa.org/packages/") t)
       (add-to-list 'package-archives
                    '("org" . "https://orgmode.org/elpa/") t)))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-default-notes-file (concat org-directory "/notes.org"))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C          . t)
   (emacs-lisp . t)
   (dot        . t)
   (ditaa      . t)
   (shell      . t)))
(setq org-src-fontify-natively t
      org-confirm-babel-evaluate nil)

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

(defun colorize-compilation-buffer ()
  "Colorize the compilation filter buffer from start to point-max."
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(require 'use-package)
(use-package htmlize :ensure t)

(use-package org-bullets :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package gruvbox-theme :ensure t
  :config (load-theme 'gruvbox-dark-hard t))

(use-package idle-highlight-mode :ensure t
  :config (idle-highlight-mode))

(use-package company :ensure t
  :config
  (progn
    (setq company-minimum-prefix-length 2
          company-selection-wrap-around t
          company-show-numbers t
          company-tooltip-align-annotations t
          company-require-match nil
          company-transformers '(company-sort-by-occurrence)
          company-idle-delay 0.1
          company-dabbrev-downcase nil)
    (global-company-mode)))

(use-package company-ghci :ensure t
  :config
  (progn
    (push 'company-ghci company-backends)
    (add-hook 'haskell-mode-hook 'company-mode)
    (add-hook 'haskell-interactive-mode-hook 'company-mode)))

(use-package projectile :ensure t
  :config
  (progn
    (projectile-global-mode)
    (defun set-gopath-smart ()
      "Reset GOPATH if a vendor dir exists in the project root"
      (let ((vendor-dir (expand-file-name "vendor" (projectile-project-root))))
        (when (file-exists-p vendor-dir)
          (setenv "GOPATH" (concat vendor-dir path-separator (getenv "GOPATH"))))))
    (add-hook 'projectile-after-switch-project-hook 'set-gopath-smart)
    (setq projectile-completion-system 'ivy))
  :bind
  (("C-c p h" . projectile-find-file)
   ("C-c p o" . projectile-find-other-file-other-window)
   ("C-c p s" . projectile-switch-project)
   ("C-c p t" . projectile-find-tag)
   ("C-c p g" . projectile-grep)))

(use-package p4 :ensure t)
(defun p4-tramp-workaround-find-file-hook ()
  "do not let p4.el process remote TRAMP buffers"
  (when
      (and (fboundp 'tramp-tramp-file-p)
           (not (tramp-tramp-file-p buffer-file-name)))
    (p4-update-status)))
;; p4.el adds p4-update-status to find-file-hook
;; we replace it with a wrapper that filters out remote buffers.
(remove-hook 'find-file-hook 'p4-update-status)
(add-hook 'find-file-hooks 'p4-tramp-workaround-find-file-hook)

(use-package omnisharp :ensure t)

(use-package tide :ensure t)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(use-package counsel :ensure t
  :diminish (ivy-mode . "")
  :bind
  (("M-y" . counsel-yank-pop)
   ("M-x" . counsel-M-x)
   ("C-s" . swiper-isearch)
   ("C-x C-f" . counsel-find-file)
   ("C-x b" . ivy-switch-buffer)
   ("C-x l" . counsel-locate)
   ("C-c J" . counsel-file-jump)
   :map ivy-mode-map
   ("C-'" . ivy-avy))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-count-format "")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order)))
  ;; Show recently killed buffers when calling `ivy-switch-buffer'
  (setq ivy-use-virtual-buffers t)
  (setq ivy-virtual-abbreviate 'full))   ;Show the full virtual file paths

(use-package markdown-mode
  :ensure t
  :mode ("\\.text\\'" "\\.markdown\\'" "\\.md\\'"))

(use-package clang-format
  :ensure t
  :config
  (progn
    (defun clang-format-buffer-smart ()
      "Reformat buffer if .clang-format exists in the project root."
      (when (and (or (eq major-mode 'c++-mode)
                     (eq major-mode 'c-mode))
                 (file-exists-p (expand-file-name ".clang-format" (projectile-project-root))))
        (clang-format-buffer)
        (message "ran clang format")))
    (add-hook 'before-save-hook 'clang-format-buffer-smart)))

(eval-after-load
    'company
  '(add-to-list 'company-backends 'company-omnisharp))
(add-hook 'csharp-mode-hook #'company-mode)
(add-hook 'csharp-mode-hook 'omnisharp-mode)

(use-package yasnippet
  :ensure t
  )

;; Note the use of background-index below requires installation of a *local* clangd with version >=
;; 8.0. For kicks, I have compiled LLVM from source on grosenberg-lx, because there isn't a way to
;; use yum/dnf to install a recent version of clangd on Cent 7.
(use-package lsp-mode
  :ensure t
  :config
  (add-hook 'c++-mode-hook #'lsp)
  (setq lsp-clients-clangd-args '("-j=8" "-background-index" "-log=verbose"))
  )

(use-package lsp-java
  :ensure t)
(add-hook 'java-mode-hook #'lsp)

(use-package lsp-ui
  :ensure t)

(use-package company-lsp
  :ensure t
  :config
  (progn
    (push 'company-ghci company-backends)
    ))

(use-package treemacs
  :ensure t)

(use-package lsp-treemacs
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(if (file-exists-p "~/.emacs.d/lisp/tableau-data-mode.el")
    ((require 'tableau-data-mode)
     (require 'tableau-template-mode)
     (setq auto-mode-alist
           (append '(
                     ("\\.data$"  . tableau-data-mode)
                     ("\\.schema$"  . tableau-data-mode)
                     ("\\.template$"  . tableau-template-mode))
                   auto-mode-alist))))

(provide 'EmacsConfig)
