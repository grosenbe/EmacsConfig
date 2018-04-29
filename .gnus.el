(setq user-mail-address "geoff.rosenberg@gmail.com"
      user-full-name "Geoff Rosenberg")

(setq gnus-select-method
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

;; n: Sender name from header; B: Thread level; U: unread; D: date; s: subject; F: full From header; R: Secondary mark
(setq gnus-summary-line-format
      "%U %n %B %D %s\n")

;; Add two key bindings for your Gmail experience.
(add-hook 'gnus-summary-mode-hook 'my-gnus-summary-keys)

;; prevent the rendering of HTML emails
(with-eval-after-load "mm-decode"
       (add-to-list 'mm-discouraged-alternatives "text/html")
       (add-to-list 'mm-discouraged-alternatives "text/richtext"))

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
