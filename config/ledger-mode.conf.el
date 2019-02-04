(use-package ledger-mode
  :if (eq system-type 'gnu/linux)
  :custom-face
  (ledger-font-payee-cleared-face ((t (:inherit default
                                                :underline t))))
  (ledger-font-payee-uncleared-face ((t (:foreground "gold4"
                                                     :weight bold))))
  (ledger-font-posting-account-cleared-face ((t (:inherit default))))
  (ledger-font-posting-amount-face ((t (:foreground "blue"
                                                    :slant unspecified))))
  (ledger-font-posting-date-face ((t (:foreground "ForestGreen"
                                                  :weight bold))))
  (ledger-font-xact-highlight-face ((t (:inherit hl-line)))))
