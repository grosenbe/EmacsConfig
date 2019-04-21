(setq user-mail-address "geoff.rosenberg@gmail.com"
      user-full-name "Geoff Rosenberg")

(setq gnus-select-method
      '(nnimap "gmail"
	       (nnimap-address "imap.gmail.com")
	       (nnimap-server-port 993)
	       (nnimap-stream ssl)
	       (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")  ; Move expired messages to Gmail's trash.
	       (nnmail-expiry-wait immediate)))			    ; mails marked as expired can be processed immediately

;; Gmail system labels have the prefix [Gmail], which matches
;; the default value of gnus-ignored-newsgroups. That's why we
;; redefine it.
(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(setq gnus-agent nil)
(setq gnus-use-dribble-file nil)	;no dribble file

;; We don't want local, unencrypted copies of emails we write.
(setq gnus-message-archive-group nil)

;; ; Archive outgoing email in Sent folder on imap.gmail.com:
(setq gnus-message-archive-method '(nnimap "imap.gmail.com")
      gnus-message-archive-group "[Gmail]/Sent Mail")

;; Attempt to encrypt all the mails we'll be sending.
;; can just delete the setting when typing the message if desired
(add-hook 'message-setup-hook 'mml-secure-message-encrypt)

(setq gnus-thread-sort-functions
      '((not gnus-thread-sort-by-date)
      gnus-thread-sort-by-number))

(setq gnus-demon-timestep 60) ;; gnus demon timestep in seconds
(gnus-demon-add-handler 'gnus-demon-scan-news 2 t)

(require 'gnus-notify)			;modeline notifications
;;gnus-desktop-notify generates notifications whenever the group buffer is updated.
(require 'gnus-desktop-notify)
(gnus-desktop-notify-mode)

;; n: Sender name from header; B: Thread level; U: unread; D: date; s: subject; F: full From header; R: Secondary mark
(setq gnus-extra-headers
      '(To Newsgroups))
(setq gnus-summary-line-format "%U%R %*%-30f %B %&user-date; %s\n"
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

;; BBDB: Address list
(require 'bbdb)
(bbdb-initialize 'message 'gnus 'sendmail)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(bbdb-mua-auto-update-init 'message)
(setq bbdb-mua-auto-update-p 'query)
(setq bbdb/mail-auto-create-p t
      bbdb/news-auto-create-p t)
(setq bbdb-mua-auto-complete t)

;; auto-complete emacs address using bbdb UI
(add-hook 'message-mode-hook
          '(lambda ()
             (flyspell-mode t)
             (local-set-key (kbd "TAB") 'bbdb-complete-mail)))
(setq bbdb-complete-mail-allow-cycling t)

 ;; Signature
(setq gnus-posting-styles '((".*" (signature "Geoff Rosenberg"))))

;; pgp signing
(require 'epg-config)
(setq
 epg-debug t
 mml2015-use 'epg
 mml2015-verbose t
 mml2015-encrypt-to-self t
 mml2015-always-trust nil
 mml2015-cache-passphrase t
 mml2015-passphrase-cache-expiry '36000
 mml2015-sign-with-sender t
 gnus-message-replyencrypt t
 gnus-message-replysign t
 gnus-message-replysignencrypted t
 gnus-treat-x-pgp-sig t
 mm-verify-option 'always
 mm-decrypt-option 'always
 mm-sign-option 'guided
 gnus-buttonized-mime-types
 '("multipart/alternative" "multipart/encrypted" "multipart/signed"))

(defadvice mml2015-sign (after mml2015-sign-rename (cont) act)
  (save-excursion
    (search-backward "Content-Type: application/pgp-signature")
    (goto-char (point-at-eol))
    (insert "; name=\"signature.asc\"; description=\"Digital signature\"")))

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
