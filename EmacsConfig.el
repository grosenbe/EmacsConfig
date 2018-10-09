(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(custom-enabled-themes (quote (manoj-dark)))
 '(inhibit-startup-screen t)
 '(org-agenda-files (quote ("~/BO_Home/Admin/org/Org_Notes.org")))
 '(send-mail-function (quote smtpmail-send-it))
 '(show-paren-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

(global-set-key (kbd "C-c w") 'toggle-truncate-lines)

;; always default to many windows in GDB
(setq gdb-many-windows t)

;; melpa packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;; (package-initialize)

;; org stuff
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
;; set org mode up for literal programming
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (fortran . t)))

(setq dired-listing-switches "-aBhl --group-directories-first")

(provide 'EmacsConfig)
