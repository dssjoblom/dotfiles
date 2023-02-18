;; Creation date: Sat Oct 29 01:56:56 2005
;; Time-stamp: <2017-11-04 02:18:19 dsjoblom>

;;;; Miscellanious utility functions

;; To use the functions in this file, put the file in your load-path
;; and put (require 'dsj-util) in your .emacs file.

;;; Variables defining behaviour

(require 'cl)

(defvar default-license-path "~/files/BSD-LICENSE"
  "Path to read standard C license from")

(defvar shell-license-path "~/files/PD-LICENSE"
  "Path to read standard shell script license from")

(defvar junk-buffers "\\*Diff\\*\\|\\*cvs-diff\\*\\|\\*Shell Command Output\\*\\|\\*Customize.*\\|\\*info\\*\\|\\*Apropos\\*\\|\\*Man.*\\|\\*Backtrace\\*"
  "A regex that is compared against buffer names by kill-junk-buffers.
If a buffer matches regex it is killed. See kill-junk-buffers.")

(defvar window-resize-a-bit-magic 3
  "Amount of lines to enlarge/shrink window with in
enlarge/shrink-window-a-bit")

(defvar makefile-list '("Makefile.am" "Makefile" "makefile")
  "List of possible Makefile names. They are searched in the
order they appear in this list by visit-this-makefile")

(defvar dsj-objdump-flags '("-d")
  "Default flags to pass to objdump with dsj-objdump")

;;; Macros

(defmacro dsj-swap-order (i j)
  "Swaps i and j if j is lesser than i. Note:
i and j should both be variables"
  (let ((tmp (make-symbol "dsj-swap-order-tmp-sym")))
  `(when (< ,j ,i)
     (let ((,tmp ,i))
       (setq ,i ,j)
       (setq ,j ,tmp)))))

(defmacro with-buffers-matching (spec &rest body)
  `(dolist (,(car spec) (buffer-list))
     (when (funcall ,(cadr spec) ,(car spec))
       (progn
         ,@body))))

;;; Functions
(defun buffer-props(buff)
  "Returns alist of buffer props"
  (let ((file (if (bufferp buff)
                  (buffer-file-name buff)
                (buffer-file-name (get-buffer buff)))))
    (if (not file)
        (list (list "file" "")
              (list "dir" "")
              (list "ext" ""))
      (let ((dir (file-name-directory file))
            (ext (file-name-extension file)))
        (list (list "file" file)
              (list "dir" dir)
              (list "ext" ext))))))

(defun dsj-buf-name(buff)
  (cadr (assoc "file" (buffer-props buff))))
(defun dsj-buf-ext(buff)
  (cadr (assoc "ext" (buffer-props buff))))
(defun dsj-buf-dir(buff)
  (cadr (assoc "dir" (buffer-props buff))))

(defun dsj-require-interactively (feature)
  "Calls `require' with argument feature"
  (interactive "SFeature:")
  (require feature))

(defun dsj-buffer-file-name (buffer)
  "Shows buffer file name in minibuffer"
  (interactive "bBuffer:")
  (princ (buffer-file-name (if (bufferp buffer) buffer (get-buffer buffer)))))

(defun split-vertically-goto-other-buffer (&optional ARG)
  "Splits window vertically, selects the new window and shows
other-buffer in the new window"
  (interactive)
  (split-window-vertically ARG)
  (other-window 1)
  (switch-to-buffer (other-buffer (current-buffer) nil nil)))

(defun split-horizontally-goto-other-buffer (&optional ARG)
  "Splits window horizontally, selects the new window and shows
other-buffer in the new window"
  (interactive)
  (split-window-horizontally ARG)
  (other-window 1)
  (switch-to-buffer (other-buffer (current-buffer) nil nil)))

(defun other-window-kill-current ()
  "Selects other window and deletes this window (plus any
additional windows).  This is meant to be used when you have two
split windows, and want to move to the other and close the
current window."
  (interactive)
  (other-window 1)
  (delete-other-windows))

(defun dsj-do-in-other-window (key)
  "Executes command bound to key in other window"
  (interactive "kKeysequence:")
  (other-window 1)
  (call-interactively (global-key-binding key))
  (other-window 1))

(defun dsj-rotate-windows ()
  "Rotates buffers visible in current windows"
  (interactive)
  (let ((buffers (mapcar #'window-buffer (window-list))))
    (when buffers
      (setq buffers (nconc (cdr buffers) (list (car buffers))))
      (mapcar* #'set-window-buffer (window-list) buffers))))

(defun dsj-forward-whitespace ()
  "Moves forward until a non-whitespace character is seen"
  (interactive)
  (skip-chars-forward "[ \t\n]"))

(defun kill-line-backwards ()
  "Kills region from point to beginning of line. As a special feature,
if the line is empty, the line is deleted and *not* added to
kill-ring."
  (interactive)
  (let ((line-start (point-at-bol))
	(end (point)))
    (if (eq line-start end)
	(delete-char 1)
      (kill-region line-start end))))

(defun kill-line-at-point ()
  "Kills line that contains point. As a special feature, if the
line is empty, the line is deleted and *not* added to
kill-ring. Respects kill-whole-line setting."
  (interactive)
  (let ((start (point-at-bol))
	(end (point-at-eol)))
    (if (eq start end)
	(delete-char 1)
      (progn
        (beginning-of-line)
        (kill-line)))))

(defun yank-and-indent ()
  "Yanks current-kill and indents it. Does not modify kill-ring
structure. Point is preserved."
  (interactive)
  (save-excursion
    (let ((str (current-kill 0 t))
	  (p (point)))
      (insert str)
      (indent-region p (point) nil))))

(defun concat-string-list (l sep)
  "Concatenates a list of strings to a single string by joining
them with separator"
  (if (null l)
      ""
    (let ((str (car l)))
      (dolist (curr (cdr l) str)
	(setq str (concat str sep curr))))))

(defun insert-date ()
  "Inserts current date at point"
  (interactive)
  (insert (current-time-string)))

(defun insert-default-license ()
  "Inserts license at point. The path to license file is read
from default-license-path."
  (interactive)
  (insert-file-contents default-license-path))

(defun get-beginning-of-line ()
  "Like beginning-of-line, but also returns the point at beginning of line"
  (beginning-of-line)
  (point))

(defun get-end-of-line ()
  "Like end-of-line, but also returns the point at end of line"
  (end-of-line)
  (point))

(defun dsj-underline-region-lines (start end &optional char)
  "Underlines each line in region with char. If char is not
provided underline with =. Usage tip: Type C-<space> to set the
mark, type your text, and when you are done, run this function to
underline the typed text."
  (interactive "r")
  (dsj-swap-order start end)
  (unless char
    (setq char ?=))
  (goto-char start)
  (while (< (point) end)
    (let ((lbp (get-beginning-of-line))
          (lep (get-end-of-line))
          (len nil))
      (setq len (- lep lbp))
      (setq end (+ end len))
      (if (eobp)
          (insert "\n")
        (forward-char))
      (insert-char char len)
      (insert "\n"))))

(defun apply-to-lines-matching (regexp fn)
  "Applies function to each line containing match with regexp. fn
must be a function taking a range as args , ie. it must take a
start and end point as arguments"
  (interactive "sRegexp:\naFunction:")
  (save-excursion
    (let ((line-start)
	  (line-end))
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
	(setq line-start (get-beginning-of-line))
	(setq line-end (get-end-of-line))
	(funcall fn line-start line-end)))))

(defun comment-lines-matching (regexp)
  "Similar to flush-lines. Comments all lines that contain a
match with regexp. Useful when printf debugging ;-)"
  (interactive "sRegexp:")
  (apply-to-lines-matching regexp #'comment-region))

(defun uncomment-lines-matching (regexp)
  "Uncomments all lines that contain a match with regexp. See
also comment-lines-matching. Useful when printf debugging ;-)"
  (interactive "sRegexp:")
  (apply-to-lines-matching regexp #'uncomment-region))

(defun hide-lines-matching (regexp)
  "Hide lines containg a match with regexp. The lines can be set
visible again with facemenu-remove-special or unhide-all."
  (interactive "sRegexp:")
  (apply-to-lines-matching regexp #'facemenu-set-invisible))

(defun unhide-all ()
  "Unhides all invisible lines"
  (interactive)
  (facemenu-remove-special (point-min) (point-max)))

(defun dsj-kill-buffers-by-dir (dir)
  "Kills all buffers visiting files located in dir."
  (interactive "D")
  (setq dir (file-truename dir))
  (with-buffers-matching
      (buff
       #'(lambda (buff)
           (let ((file (buffer-file-name buff)))
             (and file
                  (string= (file-name-directory (file-truename file)) dir)))))
    (kill-buffer buff)))

(defun kill-junk-buffers ()
  "Kills all buffers whose name match regex in the junk-buffers
variable. See junk-buffers variable."
  (interactive)
  (with-buffers-matching
      (buff
       #'(lambda (buff)
           (let ((buff-name (buffer-name buff)))
             (and buff-name
                  (let ((match (string-match junk-buffers buff-name)))
                    (and (numberp match)
                         (zerop match)
                         (= (match-end 0) (length buff-name))))))))
    (kill-buffer buff)))

(defun my-some (predicate list)
  "A minimal version of some"
  (let ((curr list)
        (ret nil))
    (while (not (null curr))
      (if (funcall predicate (car curr))
          (setq ret (car curr) curr nil)
        (setq curr (cdr curr))))
    ret))

(defun visit-this-makefile (buffer)
  "Heuristically tries to visit a buffer that could be a Makefile for buffer"
  (interactive "bBuffer:")
  (let ((dirname (file-name-directory (if (bufferp buffer)
                                          (buffer-file-name buffer)
                                        (buffer-file-name (get-buffer buffer))))))
    (let ((makefile (my-some #'(lambda (s)
                                 (and (file-exists-p s)
                                      (file-readable-p s)))
                             (mapcar #'(lambda (f)
                                         (concat dirname f))
                                     makefile-list))))
      (if makefile
          (find-file makefile)
        (princ "No makefile found")))))

(defun insert-header-guard (name)
  "Inserts header guard at point"
  (interactive "sGuard name:")
  (let ((header-id (upcase name)))
    (insert "#ifndef " header-id "\n"
            "#define " header-id "\n"
            "\n#endif\n")
    (forward-line -2)))

(defun init-c-source ()
  "Initalizes a new c language source file with some info"
  (interactive)
  (insert "/* Creation date: ")
  (insert-date)
  (insert " */\n/* Time-stamp: <> */\n")
  (insert-default-license)
  (goto-char (point-max))
  (insert "\n"))

(defun init-c-header ()
  "Initalizes a new c language header file with some info"
  (interactive)
  (init-c-source)
  (insert-header-guard
   (concat-string-list (split-string (buffer-name) "[^A-Za-z0-9]") "_")))

(defun init-java-source ()
  "Initializes a new java language source file with some info"
  (interactive)
  (init-c-source)
  (insert "\n/**\n" " *\n" " * @author " user-full-name "\n*/\n"))

(defun init-shell-source ()
  "Initializes a new shell script with some info.
License is read from shell-license-path."
  (interactive)
  (save-excursion
    (insert "#\n#\n")
    (insert "# Creation date: ")
    (insert-date)
    (insert "\n# Time-stamp: <>\n")
    (insert "# Author: " user-full-name "\n")
    (insert-file-contents shell-license-path))
  (goto-char (+ (point) 1)))

(defun enlarge-window-a-bit ()
  "Enlarges current window a bit"
  (interactive)
  (enlarge-window window-resize-a-bit-magic))

(defun shrink-window-a-bit ()
  "Shrinks current window a bit"
  (interactive)
  (shrink-window window-resize-a-bit-magic))

(defun dsj-switch-to-scratch ()
  "Switches to *scratch* buffer, creating it if necessary"
  (interactive)
  (let ((scratch (get-buffer-create "*scratch*")))
    (switch-to-buffer scratch)
    (setq buffer-offer-save nil)
    (lisp-interaction-mode)))

(defun dsj-objdump (file &optional extra-flags)
  "Disassembles file using objdump. Optional argument extra-flags
is a list of flags to pass to objdump, otherwise default flags
are taken from dsj-objdump-flags"
  (interactive "fFile:")
  (let ((buff (concat "*objdump: " (file-name-nondirectory file) "*"))
        (same-window-regexps (cons "\\*objdump:.*\\*" same-window-regexps)))
    (shell-command (concat "objdump "
                           (concat-string-list (or
                                                extra-flags
                                                dsj-objdump-flags) " ")
                           " " file)
                   buff)))

(provide 'dsj-util)
