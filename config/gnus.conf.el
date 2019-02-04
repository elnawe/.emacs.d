(eval-when-compile
  (defvar gnus-balloon-face-0)
  (defvar gnus-balloon-face-1))

(use-package rs-gnus-summary
  :if (eq system-type 'gnu/linux)
  :load-path "~/.emacs.d/elisp/"
  :config
  (defalias 'gnus-user-format-function-size 'rs-gnus-summary-line-message-size)

  (setq gnus-balloon-face-0 'rs-gnus-balloon-0)
  (setq gnus-balloon-face-1 'rs-gnus-balloon-1))

(use-package gnus
  :if (eq system-type 'gnu/linux)
  :ensure nil
  :preface
  (require 'nnir)

  (defun nemacs-gnus-exit-gnus-on-exit ()
    (if (and (fboundp 'gnus-group-exit)
             (gnus-alive-p))
        (with-current-buffer (get-buffer "*Group*")
          (let (gnus-interactive-exit)
            (gnus-group-exit)))))

  (defsubst dot-gnus-tos (time)
    "Convert TIME to a floating point number."
    (+ (* (car time) 65536.0)
       (cadr time)
       (/ (or (car (cdr (cdr time))) 0) 1000000.0)))

  (defun gnus-user-format-function-S (header)
    "Return how much time it's been since something was sent."
    (condition-case err
        (let ((date (mail-header-date header)))
          (if (> (length date) 0)
              (let*
                  ((then (dot-gnus-tos
                          (apply 'encode-time (parse-time-string date))))
                   (now (dot-gnus-tos (current-time)))
                   (diff (- now then))
                   (str
                    (cond
                     ((>= diff (* 86400.0 7.0 52.0))
                      (if (>= diff (* 86400.0 7.0 52.0 10.0))
                          (format "%3dY" (floor (/ diff (* 86400.0 7.0 52.0))))
                        (format "%3.1fY" (/ diff (* 86400.0 7.0 52.0)))))
                     ((>= diff (* 86400.0 30.0))
                      (if (>= diff (* 86400.0 30.0 10.0))
                          (format "%3dM" (floor (/ diff (* 86400.0 30.0))))
                        (format "%3.1fM" (/ diff (* 86400.0 30.0)))))
                     ((>= diff (* 86400.0 7.0))
                      (if (>= diff (* 86400.0 7.0 10.0))
                          (format "%3dw" (floor (/ diff (* 86400.0 7.0))))
                        (format "%3.1fw" (/ diff (* 86400.0 7.0)))))
                     ((>= diff 86400.0)
                      (if (>= diff (* 86400.0 10.0))
                          (format "%3dd" (floor (/ diff 86400.0)))
                        (format "%3.1fd" (/ diff 86400.0))))
                     ((>= diff 3600.0)
                      (if (>= diff (* 3600.0 10.0))
                          (format "%3dh" (floor (/ diff 3600.0)))
                        (format "%3.1fh" (/ diff 3600.0))))
                     ((>= diff 60.0)
                      (if (>= diff (* 60.0 10.0))
                          (format "%3dm" (floor (/ diff 60.0)))
                        (format "%3.1fm" (/ diff 60.0))))
                     (t
                      (format "%3ds" (floor diff)))))
                   (stripped
                    (replace-regexp-in-string "\\.0" "" str)))
                (concat (cond
                         ((= 2 (length stripped)) "  ")
                         ((= 3 (length stripped)) " ")
                         (t ""))
                        stripped))))
      (error "    ")))
  :hook (kill-emacs . nemacs-gnus-exit-gnus-on-exit)
  :bind (("C-x M" . gnus))
  :custom
  (gnus-select-method '(nnimap "Local"
                               (nnimap-address "localhost")
                               (nnimap-stream network)
                               (nnimap-authenticator login)
                               (nnir-search-engine imap)))
  (gnus-auto-select-next nil)
  (gnus-asynchronous t)
  (gnus-always-read-dribble-file t)
  (gnus-check-new-newsgroups nil)
  (gnus-score-expiry-days 30)
  (gnus-score-interactive-default-score 10)
  (gnus-thread-score-function 'max)
  (gnus-thread-hide-subtree t)
  (gnus-thread-ignore-subject nil)
  (gnus-treat-date 'head)
  (gnus-use-cache t)
  (gnus-use-adaptive-scoring t)
  (gnus-activate-level 3)
  (gnus-topic-display-empty-topics nil)
  (gnus-topic-line-format "%i[ %A: %(%{%n%}%) ]%v\n")
  (gnus-large-newsgroup 200)
  (gnus-secondary-select-methods '((nntp "news.gwene.org"
                                         (nntp-open-connection-function nntp-open-plain-stream))))
  (gnus-gcc-mark-as-read t)
  (gnus-list-groups-with-ticked-articles nil)
  (nnmail-expiry-wait 'immediate)
  (nnmail-expiry-target "nsacchetti.com/trash"))

(use-package gnus-art
  :if (eq system-type 'gnu/linux)
  :ensure nil
  :hook (gnus-article-display . gnus-article-highlight-citation)
  :custom
  (gnus-article-date-headers 'lapsed)
  :custom-face
  (gnus-header-name ((t (:foreground "SlateBlue" :weight bold))))
  (gnus-header-content ((t (:inherit default))))
  (gnus-header-subject ((t (:foreground "navy" :weight bold)))))

(use-package gnus-demon
  :if (eq system-type 'gnu/linux)
  :ensure nil
  :preface
  (defun nemacs-gnus-demon-setup ()
    (message "Initializing NEMACS GNUs Demon")
    (gnus-demon-init)
    (setq gnus-demon-timestep 60)
    (gnus-demon-add-handler #'(lambda ()
                                (gnus-demon-scan-news)
                                (message "NEMACS GNUs: Checking e-mail...")
                                (gnus-group-save-newsrc))
                            15 nil)
    (gnus-demon-add-scanmail)

    (defadvice gnus-demon-scan-news (around gnus-demon-timeout activate)
      (with-timeout (120 (message "GNUs timed out.")) ad-do-it)))
  :hook (gnus-startup . nemacs-gnus-demon-setup))

(use-package gnus-group
  :if (eq system-type 'gnu/linux)
  :ensure nil
  :preface
  :hook ((gnus-group-mode . hl-line-mode)
         (gnus-group-mode . gnus-topic-mode))
  :custom
  (gnus-group-goto-unread nil)
  (gnus-permanently-visible-groups "INBOX")
  (gnus-group-line-format "%S%p%P%M%5y: %(%B%G%B%)\n")
  :custom-face
  (gnus-group-mail-3 ((t (:foreground "navy" :inherit default :weight bold))))
  (gnus-group-mail-3-empty ((t (:foreground "navy" :inherit default :weight normal)))))

(use-package gnus-sum
  :if (eq system-type 'gnu/linux)
  :ensure nil
  :preface
  (defun nemacs-gnus-summary-mode-setup ()
    (hl-line-mode 1)
    (setq cursor-type t))
  :hook (gnus-summary-mode . nemacs-gnus-summary-mode-setup)
  :custom
  (gnus-summary-line-format "%«%3t %U%R %uS [%4i] %»%(%*%-15,15f   %1«%B%s%»%)\n")
  (gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)
  (gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date))
  (gnus-summary-mark-below -100)
  (gnus-thread-expunge-below -1000)
  (gnus-sum-thread-tree-false-root      "")
  (gnus-sum-thread-tree-single-indent   "")
  (gnus-sum-thread-tree-root            "")
  (gnus-sum-thread-tree-vertical        "|")
  (gnus-sum-thread-tree-leaf-with-other "+-> ")
  (gnus-sum-thread-tree-single-leaf     "\\-> ")
  (gnus-sum-thread-tree-indent          " ")
  :custom-face
  (gnus-summary-high-ancient ((t (:foreground "gray20"))))
  (gnus-summary-normal-ancient ((t (:foreground "gray40" :inherit default))))
  (gnus-summary-normal-ticked ((t (:foreground "DarkSlateBlue" :inherit default :italic t))))
  (gnus-summary-normal-unread ((t (:inherit default :weight bold))))
  (gnus-summary-selected ((t (:foreground "DarkSlateBlue" :weight bold))))
  :config
  (defface gnus-summary-expirable-face
    '((((class color) (background dark))
       (:foreground "red" :strike-through t))
      (((class color) (background light))
       (:foreground "red" :strike-through t)))
    "Face used to highlight articles marked as expirable."
    :group 'gnus-summary-visual)

  (push '((eq mark gnus-expirable-mark) . gnus-summary-expirable-face)
        gnus-summary-highlight))

(use-package gnus-alias
  :if (eq system-type 'gnu/linux)
  :after gnus
  :commands (gnus-alias-determine-identity
             gnus-alias-message-x-completion
             gnus-alias-select-identity
             gnus-alias-use-identity)
  :bind (:map message-mode-map
              ("C-c C-f C-i" . gnus-alias-select-identity))
  :preface
  (defsubst match-in-strings (re strs)
    (cl-some (apply-partially #'string-match re) strs))

  (defun nemacs-gnus-alias-determine-identity ()
    (let ((addrs
           (ignore-errors
             (with-current-buffer (gnus-copy-article-buffer)
               (apply #'nconc
                      (mapcar
                       #'(lambda (x)
                           (split-string (or (gnus-fetch-field x) "") ","))
                       '("To" "Cc" "From" "Reply-To")))))))
      (cond
       ((or (match-in-strings "me@nsacchetti\\.com" addrs)
            (match-in-strings "emacs-.*@gnu" addrs)
            (string-match "\\(gnu\\|emacs\\)" gnus-newsgroup-name))
        (gnus-alias-use-identity "Me"))
       ((or (match-in-strings "nsacchetti@itx\\.com" addrs)
            (match-in-strings "@itx\\.com" addrs)
            (match-in-strings "@paychex\\.com" addrs))
        (gnus-alias-use-identity "ITX"))
       (t
        (gnus-alias-determine-identity)))))
  :custom
  (gnus-alias-default-identity "ITX")
  (gnus-alias-identity-alist '((#("Me" 0 1
                                  (idx 4))
                                "" "\"Nahuel Jesús Sacchetti\" <me@nsacchetti.com>" "" nil "" nemacs-my-signature)
                               (#("ITX" 0 1
                                  (idx 3))
                                "" "\"Nahuel Jesús Sacchetti\" <nsacchetti@itx.com>" "" nil "" nemacs-my-signature)))
  :init
  (when (featurep 'message-x)
    (add-hook 'message-x-after-completion-functions 'gnus-alias-message-x-completion))
  (add-hook 'message-setup-hook #'nemacs-gnus-alias-determine-identity))
