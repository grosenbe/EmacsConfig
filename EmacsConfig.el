;;tell emacs where the personal load path directory is
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; convenience stuff
(setq lexical-binding 1)
(global-set-key (kbd "<C-tab>") 'other-window)
(delete-selection-mode 1)
(show-paren-mode 1)
(global-set-key (kbd "C-c b") 'blink-matching-open)
(display-time-mode 1)
(setq display-time-format '" %H:%M")
(global-set-key (kbd "C-c w") 'toggle-truncate-lines)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(global-set-key (kbd "C-c C-f") 'recompile)
(global-set-key (kbd "C-c t") 'whitespace-mode)
(global-set-key (kbd "C-c d") 'display-line-numbers-mode)

(defconst CGR/using-native-comp (and (fboundp 'native-comp-available-p)
                                     (native-comp-available-p)))
(defconst CGR/using-native-json (functionp 'json-serialize))

(if CGR/using-native-comp
    (progn
      (setq native-comp-deferred-compilation t)
      (setq native-comp-async-query-on-exit t)
      (setq native-comp-async-jobs-number 4)
      (setq native-comp-async-report-warnings-errors nil)))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq column-number-mode t
      dired-listing-switches "-ahl")
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq native-comp-async-report-warnings-errors '"silent")

(defun server-shutdown ()
  "Save buffers, quit, and kill server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(defun cgr-copy-file-path (&optional @dir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
Result is full path.
If `universal-argument' is called first, copy only the dir path.

If in dired, copy the file/dir cursor is on, or marked files.

If a buffer is not file and not dired, copy value of `default-directory' (which is usually the
“current” dir when that buffer was created)

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
(run-at-time nil (* 5 60) 'recentf-save-list)

(setq-default fill-column 100)
(setq org-latex-compiler "xelatex")
(setq latex-run-command "latexmk")
(setq tex-start-commands "")
(setq sentence-end-double-space nil)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(require 'bind-key)                ;; if you use any :bind variant

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

(use-package multiple-cursors)

(use-package htmlize)

(use-package dad-joke)

(use-package vterm
  :bind
  (("C-c C-u" . vterm-send-C-u)))

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-hard t))

(use-package idle-highlight-mode
  :config
  (idle-highlight-mode))

(use-package company
  :config
  (progn
    (setq company-minimum-prefix-length 1
          company-idle-delay 0.0
          company-selection-wrap-around t
          company-show-numbers t
          company-tooltip-align-annotations t
          company-require-match nil
          company-transformers '(company-sort-by-occurrence)
          company-idle-delay 0.1
          company-dabbrev-downcase nil)))

(use-package projectile
  :config
  (progn
    (defun set-gopath-smart ()
      "Reset GOPATH if a vendor dir exists in the project root"
      (let ((vendor-dir (expand-file-name "vendor" (projectile-project-root))))
        (when (file-exists-p vendor-dir)
          (setenv "GOPATH" (concat vendor-dir path-separator (getenv "GOPATH"))))))
    (add-hook 'projectile-after-switch-project-hook 'set-gopath-smart)

    (setq projectile-auto-discover nil
          projectile-completion-system 'ivy
          projectile-globally-ignored-file-suffixes '("idx")
          projectile-generic-command
          "find . -type f ! -ipath '.git*' ! -ipath '*/.git*' ! -ipath '*/build/*' ! -ipath '*/.cache/*' ! -name '*~' -print0")
    (projectile-mode))
  :bind
  (("C-c p o" . projectile-find-other-file-other-window)
   ("C-c p s" . projectile-switch-project)
   ("C-c p t" . projectile-find-tag)
   ("C-c p g" . projectile-grep)))

(use-package p4)
(defun p4-tramp-workaround-find-file-hook ()
  "do not let p4.el process remote TRAMP buffers"
  (when
      (and (executable-find "p4")
           (fboundp 'tramp-tramp-file-p)
           (not (tramp-tramp-file-p buffer-file-name)))
    (p4-update-status)))
;; p4.el adds p4-update-status to find-file-hook
;; we replace it with a wrapper that filters out remote buffers.
(remove-hook 'find-file-hook 'p4-update-status)
(add-hook 'find-file-hooks 'p4-tramp-workaround-find-file-hook)
(add-hook 'p4-form-mode-hook
          (lambda () (flyspell-mode 1)))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

(use-package selectrum)

(use-package vertico
  :bind (:map vertico-map
			  ("C-s" . consult-line))
  :custom
  (vertico--cycle t)
  :init
  (vertico-mode))

(use-package orderless ;required for consult-line
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


(use-package consult
  :after (selectrum orderless)
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ;; ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("C-s"   . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         :map isearch-mode-map
         ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
         ("C-s" . consult-line)                    ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch

  ;; Enable automatic preview at point in the *Completions* buffer.
  ;; This is relevant when you use the default completion UI,
  ;; and not necessary for Vertico, Selectrum, etc.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
  )

(use-package consult-projectile
  :bind
  (("C-c p h" . consult-projectile)))

(use-package savehist
  :init
  (savehist-mode))

(use-package markdown-mode
  :mode ("\\.text\\'" "\\.markdown\\'" "\\.md\\'"))

(use-package clang-format
  :config
  (progn
    (defun clang-format-buffer-smart ()
      "Reformat buffer if .clang-format exists in the project root."
      (when (and (or (eq major-mode 'c++-mode)
                     (eq major-mode 'c-mode))
                 (file-exists-p (expand-file-name ".clang-format" (projectile-project-root))))
        (clang-format-buffer)))
    (add-hook 'before-save-hook 'clang-format-buffer-smart)
    (if (file-directory-p "~/tableau-cache")
        (setq clang-format-executable "~/tableau-cache/devtools/clang/7.0.4/bin/clang-format"))))

(use-package csharp-mode
  :after company
  :config
  ;; (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode))
  ;; (add-hook 'csharp-mode-hook #'company-mode)
  )

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package flycheck)

(use-package typescript-mode)

(use-package prettier-js
  :config
  (add-hook 'typescript-mode-hook #'prettier-js-mode))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package lsp-mode
  :after (which-key flycheck)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-clients-clangd-args '("-j=16" "-background-index" "-clang-tidy")
        lsp-csharp-server-path '"~/dev/omnisharp-roslyn/artifacts/scripts/OmniSharp.Stdio"
        lsp-csharp-server-install-dir '"~/dev/omnisharp-roslyn"
        lsp-enable-snippet nil
        lsp-fortls-args '("-notify-init" "-hover_signature" "-enable_code_actions" "-debug_log")
        gc-cons-threshold 100000000
        read-process-output-max (* 1024 1024)
        lsp-completion-provider :capf)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection
				     '("clangd" "-j=16" "-background-index" "-clang-tidy"))
                    :major-modes '(c-mode c++-mode)
                    :remote? t
                    :server-id 'clangd-remote)))

(use-package lsp-ui
  :after (lsp-mode)
  :config
  (setq lsp-eldoc-enable-hover nil))

(use-package treemacs)

(use-package lsp-treemacs)

(use-package cmake-mode)

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package doom-modeline
  :config
  (progn
    (doom-modeline-mode)
    (setq doom-modeline-height 10
          doom-modeline-buffer-file-name-style 'file-name)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package magit)
(global-set-key (kbd "C-x g") 'magit-status)
(add-hook 'git-commit-setup-hook
          (lambda () (flyspell-mode 1)))

(use-package rust-mode)

(when (file-exists-p "~/.emacs.d/lisp/tableau-data-mode.el")
  (require 'tableau-data-mode)
  (require 'tableau-template-mode)
  (setq auto-mode-alist
        (append '(
                  ("\\.data$"  . tableau-data-mode)
                  ("\\.schema$"  . tableau-data-mode)
                  ("\\.template$"  . tableau-template-mode))
                auto-mode-alist)))

(provide 'EmacsConfig)
