# EmacsConfig

This repository provides the EmacsConfig package, IE global Emacs
configuration settings.

In general, use of this package involves cloning the repository, and
adding the repository to the Emacs load path.  The local .emacs will
contain all system-specific settings (hard coded paths, etc).

## Minimal .emacs
A minimal Emacs configuration might look like the following:

(add-to-list 'load-path "~/EmacsConfig")
(require 'EmacsConfig)

(setq org-agenda-files '("~/BO_Home/Admin/org"))

