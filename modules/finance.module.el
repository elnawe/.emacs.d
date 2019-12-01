(require 'ledger-mode)

(with-eval-after-load 'ledger-mode
  (require 'nemacs-org-ledger)

  (defun nemacs-setup-ledger-mode ()
    "NEMACS Setup: Run this function in `ledger-mode-hook'."
    (company-mode)
    (setq-mode-local ledger-mode
                     complete-cycle-threshold t
                     ledger-complete-in-steps t
                     tab-always-indent 'complete))

  (add-hook 'ledger-mode-hook #'nemacs-setup-ledger-mode)

  (setq ledger-master-file (nemacs-ledger-file "booking.ledger")))
