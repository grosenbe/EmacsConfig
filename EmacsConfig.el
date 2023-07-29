;;tell emacs where the personal load path directory is
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; convenience stuff
(setq lexical-binding 1)
(global-set-key (kbd "<C-tab>") 'other-window)
(delete-selection-mode 1)
(global-set-key (kbd "C-c b") 'blink-matching-open)
(display-time-mode 1)
(setq display-time-format '" %H:%M")
(global-set-key (kbd "C-c w") 'toggle-truncate-lines)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(global-set-key (kbd "C-c C-f") 'recompile)
(global-set-key (kbd "C-c t") 'whitespace-mode)
(global-set-key (kbd "C-c d") 'display-line-numbers-mode)

(defconst CGR/using-native-comp (and (fboundp 'native-comp-available-p)
                                     (native-comp-available-p)))

(if CGR/using-native-comp
    (progn
      (setq native-comp-deferred-compilation t
            native-comp-async-query-on-exit t
            native-comp-async-report-warnings-errors nil)))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq column-number-mode t
      dired-listing-switches "-ahl")
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

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

(setq-default fill-column 100)
(setq latex-run-command "latexmk")
(setq tex-start-commands "")
(setq sentence-end-double-space nil)

(add-hook 'c-mode-common-hook
          (lambda () (define-key c-mode-base-map (kbd "C-c C-f") 'recompile)))
(setq compilation-scroll-output t)

(add-hook
 'dired-before-readin-hook
 (lambda ()
   (when (file-remote-p default-directory)
     (setq dired-actual-switches "-al"))))

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

(use-package org
  :config
  (setq org-default-notes-file "~/org/notes.org"
        org-latex-compiler "xelatex"
        org-confirm-babel-evaluate nil
        org-agenda-files `("~/org")
        org-directory "~/org")
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C          . t)
     (emacs-lisp . t)
     (dot        . t)
     (ditaa      . t)
     (shell      . t))))

(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package all-the-icons)

(use-package multiple-cursors
  :bind
  (("C-c m c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))

(use-package htmlize) 

(use-package dad-joke)

(if module-file-suffix
    (use-package vterm
      :bind
      (("C-q" . vterm-send-next-key))))

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-hard t))

(use-package idle-highlight-mode
  :config
  (idle-highlight-mode))

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))

(use-package rg)

(use-package projectile
  :after rg
  :config
  (progn
    (defun set-gopath-smart ()
      "Reset GOPATH if a vendor dir exists in the project root"
      (let ((vendor-dir (expand-file-name "vendor" (projectile-project-root))))
        (when (file-exists-p vendor-dir)
          (setenv "GOPATH" (concat vendor-dir path-separator (getenv "GOPATH"))))))
    (add-hook 'projectile-after-switch-project-hook 'set-gopath-smart)

    (setq projectile-auto-discover nil
          projectile-globally-ignored-file-suffixes '("idx")
          projectile-generic-command
          "find . -type f ! -ipath '.git*' ! -ipath '*/.git*' ! -ipath '*/build/*' ! -ipath '*/.cache/*' ! -name '*~' ! -name '*.js' -print0")
    (projectile-mode))
  :bind
  (("C-c p o" . projectile-find-other-file-other-window)
   ("C-c p s" . projectile-switch-project)
   ("C-c p t" . projectile-find-tag)
   ("C-c p g" . projectile-ripgrep)))

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

(use-package orderless ;required for consult-line
  :init
  (message '"loaded orderless")
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package consult-projectile
  :init
  (message '"loaded consult-projectile")
  (global-set-key (kbd "C-c p h") 'consult-projectile))

(use-package vertico
  :after orderless
  :bind (:map vertico-map
              ("C-s" . consult-line))
  :custom
  (vertico--cycle t)
  :init
  (message '"loaded vertico")
  (vertico-mode))

(use-package consult
  :after orderless
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ;; ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ;; ("C-x b"   . consult-buffer)                ;; orig. switch-to-buffer
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
   consult--source-bookmark
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

(use-package savehist
  :init
  (savehist-mode))

(use-package clang-format
  :config
  (progn
    (defun clang-format-buffer-smart ()
      "Reformat buffer if .clang-format exists in the project root."
      (when (and (or (eq major-mode 'c++-mode)
                     (eq major-mode 'c-mode))
                 (file-exists-p (expand-file-name ".clang-format" (projectile-project-root))))
        (clang-format-buffer)))
    (add-hook 'before-save-hook 'clang-format-buffer-smart)))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package flycheck)

(defun lsp-format-buffer-smart ()
  "Reformat the buffer if we're in LSP mode"
  (if prettier-js-mode (message "in prettier js mode, skipping lsp format buffer") (if lsp-mode (message "lsp formatting buffer")))
  (if (and lsp-mode (not prettier-js-mode))
      (lsp-format-buffer)))

(use-package csharp-mode
  :after (corfu prettier-js lsp-mode)
  :config
  (add-hook 'before-save-hook 'lsp-format-buffer-smart))

(use-package editorconfig)

(use-package typescript-mode
  :mode ("\\.tsx?\\'" . typescript-mode)
  :after (corfu editorconfig prettier-js lsp-mode)
  ;; :hook ((typescript-mode . 'editorconfig-apply)
  ;;        (before-save . 'lsp-format-buffer-smart))
  )

;; todo: only want to be in prettier-js-mode if we're in a project that has a .prettierrc at the LSP project root
(use-package prettier-js
  :after typescript-mode
  :config
  (add-hook 'typescript-mode-hook #'prettier-js-mode))

(use-package lsp-mode
  :after (which-key flycheck)
  :hook (lsp-mode . (lambda ()
                      (let ((lsp-keymap-prefix "C-c l"))
                        (lsp-enable-which-key-integration))))
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (setq lsp-clients-clangd-args '("-clang-tidy" "-log=verbose" "-background-index")
        lsp-enable-snippet nil
        lsp-fortls-args '("-notify-init" "-hover_signature" "-enable_code_actions" "-debug_log")
        gc-cons-threshold 100000000
        lsp-completion-provider :none
        read-process-output-max (* 1024 1024)
        lsp-use-plists 1))

(use-package lsp-ui
  :after (lsp-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-show-with-mouse t
        lsp-ui-sideline-show-code-actions nil
        lsp-eldoc-enable-hover nil))

(use-package treemacs)

(use-package lsp-treemacs
  :after (lsp-mode treemacs))

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package cmake-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package magit
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  (add-hook 'git-commit-setup-hook
            (lambda () (flyspell-mode 1))))

(use-package rust-mode)

(use-package yaml-mode)

(use-package marginalia
  :config
  (marginalia-mode))

(use-package bazel)

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
