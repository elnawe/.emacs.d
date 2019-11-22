;; Directory Variables
(eval-and-compile
  (defvar nemacs-emacs-dir (expand-file-name user-emacs-directory)
    "Literally, the Emacs user directory or `user-emacs-directory'.

This variable is set to be used securely across NEMACS without
the risk of changing the default value of
`user-emacs-directory'.")

  (defvar nemacs-local-dir (concat nemacs-emacs-dir ".local/")
    "The local folder where most of NEMACS stuff lives.")

  (defvar nemacs-cache-dir (concat nemacs-local-dir "cache/")
    "The folder where all the soft data is saved.

Cached folder can be deleted at any time to reset saved
information, this can be done manually or
using `(nemacs-delete-cache)'.")

  (defvar nemacs-etc-dir (concat nemacs-local-dir "etc/")
    "The folder where the hard data is saved.

This folder contains specific configuration files created by
Emacs or specific packages. This data should not be deleted.")

  (defvar nemacs-packages-dir (concat nemacs-local-dir "packages/")
    "Folder containing all the packages downloaded from MELPA.")

  (defvar nemacs-modules-dir (concat nemacs-emacs-dir "modules/")
    "The modules folder contains configurations for each module
inside NEMACS. These are loaded while `after-init-hook'.

Modules can be reloaded by `(nemacs-reload-module <modulename>)'
or interactively by selecting the module. All the modules can be
reloaded at the same time using `nemacs-reload-all-modules'.")

  (defvar nemacs-themes-dir (concat nemacs-emacs-dir "themes/")
    "Contains themes used in NEMACS at least once.")

  (defvar nemacs-elisp-dir (concat nemacs-emacs-dir "elisp/")
    "Where the custom code lives. Usually code created
specifically for NEMACS, but also code that cannot be downloaded
through the Emacs package manager.")

  (defvar nemacs-dropbox-dir (expand-file-name "~/Dropbox/")
    "Dropbox folder in the system. Some dotfiles are shared
across my systems through Dropbox. Notes and documents are also
up in the cloud.")

  (defvar nemacs-maildir-dir (expand-file-name "~/Maildir/")
    "Maildir folder where the mail is downloaded using the IMAP
    protocol.")

  (defvar nemacs-downloads-dir (expand-file-name "~/Downloads/")
    "Downloads folder from the System.")

  (defvar nemacs-directories (list nemacs-local-dir
                              nemacs-cache-dir nemacs-etc-dir
                              nemacs-packages-dir
                              nemacs-modules-dir
                              nemacs-themes-dir nemacs-elisp-dir
                              nemacs-dropbox-dir
                              nemacs-maildir-dir
                              nemacs-downloads-dir)
    "NEMACS directories. This is used on the initial setup.")

  (dolist (dir nemacs-directories)
    (unless (file-directory-p dir)
      (make-directory dir t))))

;; Configuration Variables
(eval-and-compile
  (defvar nemacs-necessary-packages '()
    "Packages needed for `NEMACS' to run. This list is `setq' in
    `packages.el'."))
