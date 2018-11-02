;;tell emacs where the personal load path directory is
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(delete-selection-mode 1)

;; convenience stuff
(global-set-key (kbd "<C-tab>") 'other-window)
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)
(setq column-number-mode t)

(display-time-mode 1) ;; show the time in the modeline

;;pdflatex
(setq latex-run-command "latexmk")
(setq tex-start-commands "")
;; text wrapping
;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'latex-mode-hook
  '(lambda() (set-fill-column 80)))
(setq sentence-end-double-space nil)


(global-set-key (kbd "C-c w") 'toggle-truncate-lines)

;; always default to many windows in GDB
(setq gdb-many-windows t)

;; melpa packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; org stuff (make sure org is enabled and org-agenda-files is set to
;; the correct path)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
;; set org mode up for literal programming
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)))

;; Magit stuff
(global-set-key (kbd "C-x g") 'magit-status)

(setq dired-listing-switches "-aBhl --group-directories-first")

(provide 'EmacsConfig)
