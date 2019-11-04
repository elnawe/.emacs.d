(use-package notmuch
  :bind
  (("C-x m" . notmuch))
  :custom
  (notmuch-multipart/alternative-discouraged '("text/plain"))
  (notmuch-search-oldest-first nil)
  (notmuch-search-result-format `(("date" . "%12s ")
                                  ("count" . "%-6s ")
                                  ("authors" . "%-30s ")
                                  ("subject" . "%s ")
                                  ("tags" . "(%s)"))))
