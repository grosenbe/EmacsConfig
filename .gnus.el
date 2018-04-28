(setq user-mail-address "geoff.rosenberg@gmail.com"
      user-full-name "C. Geoffrey Rosenberg")

(setq gnus-select-method
      '(nnimap "gmail"
	       (nnimap-address "imap.gmail.com")
	       (nnimap-server-port "imaps")
	       (nnimap-stream ssl)))

(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(setq gnus-thread-sort-functions
      '((not gnus-thread-sort-by-number)
	(not gnus-thread-sort-by-date)))

(setq gnus-summary-line-format
      "%U %D %s\n")

;; turn threading off on gnus
;; (setq gnus-show-threads nil)
