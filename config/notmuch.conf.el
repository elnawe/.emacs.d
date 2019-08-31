(use-package notmuch
  :bind
  (("C-x m" . notmuch))
  :custom
  (notmuch-search-oldest-first nil)
  (notmuch-search-result-format `(("date" . "%12s ")
                                  ("count" . "%-5s ")
                                  ("authors" . "%-20s ")
                                  ("subject" . "%s ")
                                  ("tags" . "(%s)"))))
