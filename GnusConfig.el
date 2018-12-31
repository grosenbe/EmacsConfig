(setq user-mail-address "geoff.rosenberg@gmail.com"
      user-full-name "Geoff Rosenberg")

(setq gnus-select-method
      '(nntp "free.xsusenet.com"))

(add-to-list 'gnus-secondary-select-methods
      '(nnimap "gmail"
	       (nnimap-address "imap.gmail.com")
	       (nnimap-server-port 993)
	       (nnimap-stream ssl)))

;; Gmail system labels have the prefix [Gmail], which matches
;; the default value of gnus-ignored-newsgroups. That's why we
;; redefine it.
(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(setq gnus-agent nil)

;; We don't want local, unencrypted copies of emails we write.
(setq gnus-message-archive-group nil)

;; We want to be able to read the emails we wrote.
(setq mml2015-encrypt-to-self t)

;; Attempt to encrypt all the mails we'll be sending.
;; can just delete the setting when typing the message if desired
(add-hook 'message-setup-hook 'mml-secure-message-encrypt)

(setq gnus-thread-sort-functions
      '((not gnus-thread-sort-by-date)
      gnus-thread-sort-by-number))

;; Add two key bindings for your Gmail experience.
(add-hook 'gnus-summary-mode-hook 'my-gnus-summary-keys)

;; ;; prevent the rendering of HTML emails
;; (with-eval-after-load "mm-decode"
;;        (add-to-list 'mm-discouraged-alternatives "text/html")
;;        (add-to-list 'mm-discouraged-alternatives "text/richtext"))

;;get gnus demon to scan for new email when emacs is idle.
(setq gnus-demon-timestep 10) ;; gnus demon timestep in seconds
(gnus-demon-add-handler 'gnus-demon-scan-mail 3 t) ;; scan mail every 3 timesteps, only when emacs is idle

;;gnus-desktop-notify generates notifications whenever the group buffer is updated.
(require 'gnus-desktop-notify)
(gnus-desktop-notify-mode)
(gnus-demon-add-scanmail)

;; n: Sender name from header; B: Thread level; U: unread; D: date; s: subject; F: full From header; R: Secondary mark
(setq gnus-summary-line-format "%U%R %n %B %&user-date; %s:\n"
      gnus-sum-thread-tree-false-root ""
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-leaf-with-other "├► "
      gnus-sum-thread-tree-root ""
      gnus-sum-thread-tree-single-leaf "╰► "
      gnus-sum-thread-tree-vertical "│")

(setq gnus-user-date-format-alist
      '(((gnus-seconds-today) . "Today, %H:%M")
        ((+ 86400 (gnus-seconds-today)) . "Yesterday, %H:%M")
        (604800 . "%A %H:%M") ;;that's one week
        (t . "%Y-%m-%d %H:%M"))) ;;this one is used when no other does match

;; flyspell for new messages
(add-hook 'message-mode-hook
	  (lambda ()
	    (flyspell-mode 1)
	    )
	  )

(defun my-gnus-summary-keys ()
  (local-set-key "y" 'gmail-archive)
  (local-set-key "$" 'gmail-report-spam))

(defun gmail-archive ()
  "Archive the current or marked mails.
This moves them into the All Mail folder."
  (interactive)
  (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/All Mail"))

(defun gmail-report-spam ()
  "Report the current or marked mails as spam.
This moves them into the Spam folder."
  (interactive)
  (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/Spam"))

(provide 'GnusConfig)
