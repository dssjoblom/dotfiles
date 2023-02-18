;;;; Miscellanious coding utilities

;; To use the functions in this file, put the file in your load-path
;; and put (require 'dsj-obscure-lang) in your .emacs file.

(defun pydoc (page)
  "Fetches pydoc documentation for page. This is borrowed from some
place (usenet?), but I forget where it is from."
  (interactive "sPydoc:")
  (require 'man)
  (let ((manual-program "pydoc"))
    (man page)))

(provide 'dsj-obscure-lang)
