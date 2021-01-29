(setq user-mail-address "geoff.rosenberg@gmail.com"
      user-full-name "Geoff Rosenberg")

(gnus-add-configuration '(article (horizontal 1.0 (summary .25 point) (article 1.0))))

(setq gnus-select-method
      '(nnimap "gmail"
	       (nnimap-address "imap.gmail.com")
	       (nnimap-server-port 993)
	       (nnimap-stream ssl)
	       (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
	       (nnmail-expiry-wait immediate)))

(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(setq gnus-agent nil
      gnus-use-dribble-file nil
      gnus-message-archive-method nil)

(add-hook 'message-setup-hook 'mml-secure-message-encrypt)

(setq gnus-thread-sort-functions
      '((not gnus-thread-sort-by-date)
      gnus-thread-sort-by-number))

(setq gnus-demon-timestep 60)
(gnus-demon-add-handler 'gnus-demon-scan-news 2 t)

(setq gnus-extra-headers
      '(To Newsgroups))
(setq gnus-ignored-from-addresses "geoff.rosenberg@gmail.com")
(setq gnus-summary-line-format "%U%R %*%-20f %B %&user-date; %s\n"
      gnus-sum-thread-tree-false-root ""
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-leaf-with-other "├► "
      gnus-sum-thread-tree-root ""
      gnus-sum-thread-tree-single-leaf "╰► "
      gnus-sum-thread-tree-vertical "│")
(setq gnus-user-date-format-alist
      '(((gnus-seconds-today) . "Today, %H:%M")
	((+ 86400 (gnus-seconds-today)) . "Yesterday, %H:%M")
	(604800 . "%A %H:%M")
	(t . "%Y-%m-%d %H:%M")))

(setq message-cite-style message-cite-style-gmail)

(use-package bbdb
  :ensure t)

(bbdb-initialize 'message 'gnus 'sendmail)
(add-hook 'gnus-startup-hook 'bbdb-initialize)
(bbdb-mua-auto-update-init 'message)
(setq bbdb-mua-auto-update-p 'query)
(setq bbdb/mail-auto-create-p t
      bbdb/news-auto-create-p t)
(setq bbdb-mua-auto-complete t)
(add-hook 'message-mode-hook
	  '(lambda ()
	     (flyspell-mode t)
	     (local-set-key (kbd "TAB") 'bbdb-complete-mail)))
(setq bbdb-complete-mail-allow-cycling t)

(setq mm-text-html-renderer 'gnus-w3m)

 ;; Signature
(setq gnus-posting-styles '((".*" (signature "Geoff Rosenberg"))))

(with-eval-after-load "mm-decode"
       (add-to-list 'mm-discouraged-alternatives "text/html")
       (add-to-list 'mm-discouraged-alternatives "text/richtext"))

(require 'epg-config)
(setq
 epg-debug t
 mml2015-use 'epg
 mml2015-verbose t
 mml-secure-openpgp-encrypt-to-self t
 mml-secure-openpgp-always-trust nil
 mml-secure-cache-passphrase t
 mml-secure-passphrase-cache-expiry '36000
 mml-secure-openpgp-sign-with-sender t
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
