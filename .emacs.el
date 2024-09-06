;;; .emacs --- by Daniel Sjöblom - placed in the public domain

;; Time-stamp: <2024-09-06 20:02:10 daniel>

;;; Commentary:

;; This .emacs requires a fairly recent Emacs, tested on Ubuntu,
;; versions 25.1 to 29.1.

;;; Code:

;;; Local stuff, specify where to find-packages, and setup load-path

;; Extra packages...
(when (>= emacs-major-version 24)
  (when (require 'package nil t)
    (package-initialize)
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)))

;; load path
(setq load-path (append (list "~/elisp") load-path))

;; Auto-compile .emacs.el
(defun autocompile ()
  (let ((dotemacs (expand-file-name "~/.emacs.el")))
    (when (string= (buffer-file-name) (file-chase-links dotemacs))
      (byte-compile-file dotemacs))))

(add-hook 'after-save-hook 'autocompile)

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

;; Install packages we use
(use-package company :ensure t)
(use-package dockerfile-mode :ensure t)
(use-package dockerfile-mode :ensure t)
(use-package find-file-in-project :ensure t)
(use-package flycheck :ensure t)
(use-package fold-this :ensure t)
(use-package forth-mode :ensure t)
(use-package idle-highlight-in-visible-buffers-mode :ensure t)
(use-package impatient-mode :ensure t)
(use-package markdown-mode :ensure t)
(use-package rjsx-mode :ensure t)
(use-package robe :ensure t)
(use-package rvm :ensure t)
(use-package slim-mode :ensure t)
(use-package smartparens :ensure t)
(use-package smex :ensure t)
(use-package vue-mode :ensure t)
(use-package web-mode :ensure t)
(use-package yafolding :ensure t)
(use-package yaml-mode :ensure t)
(use-package rainbow-delimiters :ensure t)
(use-package dumb-jump :ensure t)
(use-package systemd :ensure t)
(use-package slime :ensure t)
(use-package cider :ensure t)
(use-package uniquify-files :ensure t)
(use-package ido-completing-read+ :ensure t)
(use-package ido-vertical-mode :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (Hopefully) portable stuff ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Don't garbage collect if less than 50 mb is consed since last
;; garbage collection

(setq gc-cons-threshold 50000000)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(apropos-do-all t)
 '(auto-compression-mode t nil (jka-compr))
 '(current-language-environment "UTF-8")
 '(default-input-method "latin-1-prefix")
 '(ibuffer-saved-filter-groups
   '(("ibuffer-filter-groups"
      ("Config"
       (predicate if
                  (numberp
                   (string-match "^conf.*"
                                 (symbol-name major-mode)))
                  t nil))
      ("Scheme"
       (mode . scheme-mode))
      ("Java"
       (mode . java-mode))
      ("Javascript"
       (mode . js-mode))
      ("Clojure"
       (mode . clojure-mode))
      ("Web"
       (mode . web-mode))
      ("Yaml"
       (mode . yaml-mode))
      ("SCSS"
       (mode . scss-mode))
      ("Systemd"
       (mode . systemd-mode))
      ("Markdown"
       (mode . markdown-mode))
      ("Dockerfile"
       (mode . dockerfile-mode))
      ("C"
       (mode . c-mode))
      ("C++"
       (mode . c++-mode))
      ("Text"
       (mode . text-mode))
      ("Dired"
       (mode . dired-mode))
      ("Automake"
       (mode . makefile-automake-mode))
      ("Lisp"
       (mode . lisp-mode))
      ("Makefile"
       (predicate or
                  (eq major-mode 'makefile-mode)
                  (eq major-mode 'makefile-gmake-mode)))
      ("Html"
       (mode . html-mode))
      ("XML"
       (mode . sgml-mode))
      ("Emacs-Lisp"
       (mode . emacs-lisp-mode))
      ("Shell"
       (mode . sh-mode))
      ("Haskell"
       (mode . haskell-mode))
      ("Python"
       (mode . python-mode))
      ("Perl"
       (mode . perl-mode))
      ("Ruby"
       (mode . ruby-mode)))))
 '(ibuffer-show-empty-filter-groups nil)
 '(package-selected-packages
   '(graphviz-dot-mode ido-vertical-mode ido-completing-read+ uniquify-files yaml-mode yafolding web-mode vue-mode use-package systemd solaire-mode smex smartparens slime slim-mode rvm robe rjsx-mode rainbow-delimiters markdown-mode magit ivy impatient-mode idle-highlight-in-visible-buffers-mode forth-mode fold-this flycheck find-file-in-project dumb-jump dockerfile-mode company coffee-mode cider))
 '(safe-local-variable-values
   '((eval font-lock-add-keywords nil
           `((,(concat "("
                       (regexp-opt
                        '("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl")
                        t)
                       "\\_>")
              1 'font-lock-variable-name-face)))))
 '(table-time-before-update 0.1))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "gray97" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 110 :width normal :foundry "DAMA" :family "Ubuntu Mono"))))
 '(cursor ((t (:background "black"))))
 '(font-lock-builtin-face ((t (:foreground "dark green"))))
 '(font-lock-comment-face ((t (:foreground "grey35"))))
 '(font-lock-constant-face ((t (:foreground "forest green"))))
 '(font-lock-doc-face ((t (:foreground "green4"))))
 '(font-lock-function-name-face ((t (:foreground "dark violet"))))
 '(font-lock-keyword-face ((t (:foreground "medium blue" :weight bold))))
 '(font-lock-string-face ((t (:foreground "blue"))))
 '(font-lock-type-face ((t (:foreground "SteelBlue4"))))
 '(font-lock-variable-name-face ((t (:foreground "orange red"))))
 '(font-lock-warning-face ((t (:foreground "red" :weight bold))))
 '(fringe ((t (:background "light gray"))))
 '(match ((((class color) (min-colors 88) (background light)) (:background "lemonchiffon"))))
 '(menu ((((type x-toolkit)) (:background "wheat" :foreground "black"))))
 '(mmm-default-submode-face ((t (:background "gray97"))))
 '(mode-line ((t (:box (:line-width (1 . -1) :style released-button) :foreground "black" :background "light gray"))))
 '(outline-7 ((t (:inherit font-lock-string-face))))
 '(region ((t (:background "khaki2"))))
 '(scroll-bar ((t (:background "wheat" :foreground "black"))))
 '(tabbar-default ((t (:inherit variable-pitch :foreground "gray50" :height 0.8))))
 '(table-cell ((t (:background "black" :foreground "gray90" :inverse-video nil))))
 '(web-mode-html-attr-name-face ((t (:foreground "black"))))
 '(web-mode-html-tag-face ((t (:foreground "dark magenta"))))
 '(web-mode-symbol-face ((t (:foreground "forest green")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General Emacs behaviour modification ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show user@host + file in frame title

; define as function, because current-project does not auto-refresh, makes the title refreshable elsewhere
(defun refresh-frame-title()
  (setq frame-title-format (list (if (and (boundp 'current-project) (> (length current-project) 0))
                                     (concat current-project " ")
                                   (if (boundp 'ap-current-project)
                                       (concat ap-current-project " ")))
                                 "| "
                                 user-login-name
                                 "@"
                                 (system-name)
                                 " || %f" )))

(refresh-frame-title)

;; Global font lock mode
(global-font-lock-mode 1)

;; No splash screen or startup message

(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)

;; Don't blink cursor
(blink-cursor-mode 0)

;; Echo complex keystrokes immediately
(setq echo-keystrokes 0.01)

;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties
      (quote (read-only t point-entered minibuffer-avoid-prompt
                        face minibuffer-prompt)))

;; Show matching parens
(show-paren-mode t)

;; Hide miscellanious bars

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Use mousewheel
(mouse-wheel-mode 1)

;; No backup files
(setq make-backup-files nil)

;; Flex completion (version >= 27.1)
(when (>= emacs-major-version 27)
  (setq completion-styles (append '(flex) completion-styles)))

;; Open help buffers in current window, except if buffer is a
;; completion buffer, because it is very annoying to have help windows
;; pop up wherever they feel like it.

(setq pop-up-windows t)
(setq temp-buffer-show-function
      #'(lambda (buffer)
          (if (string= "*Completions*" (buffer-name buffer))
              (display-buffer buffer t nil)
            (switch-to-buffer buffer))))

;; Man doesn't use temp-buffer-show-function, but uses
;; Man-notify-method instead. Set to pushy to get same behaviour as
;; for other help buffers.

(setq Man-notify-method 'pushy)

;; Goto line centers line on screen. I don't use goto-line often, but
;; since this is an advice it affects many other functions like imenu,
;; compile, grep etc.

(defadvice goto-line (after center-on-line last)
  "Advice that centers window on line after goto-line"
  (recenter))

(ad-activate 'goto-line)

;; Single key yes-or-no

(fset 'yes-or-no-p 'y-or-n-p)

;; Lines

(setq require-final-newline t)
(setq next-line-add-newlines nil)

;; Delete whitespace on backspace (very) hungrily in all modes
(setq-default backward-delete-char-untabify-method 'all)

;; Don't indent with tabs
(setq-default indent-tabs-mode nil)

;; Lots of undos
(setq undo-limit 50000)

;; A bit more of a message log
(setq message-log-max 100)

;; Delete dups in history
(setq history-delete-duplicates t)

;; Case insensitive search
(setq-default case-fold-search t)

;; auto revert buffers
(global-auto-revert-mode 1)

;; Update time stamps on write, and remove trailing whitespace
(add-hook 'write-file-functions
	  #'(lambda ()
              (time-stamp)
              (delete-trailing-whitespace)))

;; chmod +x on scripts after save
(add-hook 'after-save-hook
	  #'executable-make-buffer-file-executable-if-script-p)

;; European calendar and time

(setq calendar-week-start-day 1)
(setq display-time-24hr-format t)

;; Enable some commands that are disabled by default

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; disable overwrite mode
(define-key global-map [remap overwrite-mode] #'(lambda () (interactive)))

;; Default major mode should be text
(setq-default major-mode 'text-mode)

;; Fix clipboard interaction
(setq select-enable-clipboard t)

;; Some minor stuff

(when (fboundp 'pc-selection-mode)
  (pc-selection-mode)) ;; Enables transient-mark-mode also

;; Fix C-x C-x when using transient-mark-mode
(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))
(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

;; Something breaks backward delete char in some modes, define a new function...
(defun my-electric-backspace (arg)
  (interactive "*P")
  (let ((here (point)))
    (skip-chars-backward " \t\n")
    (if (/= (point) here)
        (delete-region (point) here)
      (backward-delete-char-untabify 1))))

(column-number-mode 1)
(line-number-mode 1)
(setq ring-bell-function #'(lambda () nil))

;; Save place in buffers
(setq-default save-place t)

;; Better separation of files with same name
(when (require 'uniquify nil t)
  (setq uniquify-buffer-name-style 'forward))

;;;;;;;;;;;;;;;;;;;
;;; Keybindings ;;;
;;;;;;;;;;;;;;;;;;;

;; This turns on ido mode, which provides a more convenient way to
;; select buffers and find files. It replaces default keybindings
;; with similar but more powerful functions. Need the ido package.

(when (require 'ido nil t)
  (ido-mode t)
  (ido-everywhere t)
  (when (require 'ido-vertical-mode nil t)
    (ido-vertical-mode 1))
  (when (require 'ido-completing-read+ nil t) ;; Similar functionality in other contexts
    (ido-ubiquitous-mode 1))
  (defun bind-ido-keys ()
    "Keybindings for ido mode."
    (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
    (define-key ido-completion-map (kbd "<up>") 'ido-prev-match))
  (add-hook 'ido-setup-hook #'bind-ido-keys)
  (when (require 'smex nil t) ;; IDO for M-x
    (smex-initialize)
    (global-set-key "\M-x" #'smex)))

;; Load my personal lib. Available online at my homepage. There are
;; some other functions in the package that are not currently bound to
;; any key.

(when (require 'dsj-util nil t)
  ;; Replace usual split binds
  (global-set-key "\C-x2" #'split-vertically-goto-other-buffer)
  (global-set-key "\C-x3" #'split-horizontally-goto-other-buffer)
  (global-set-key "\C-cp" #'kill-line-backwards)
  (global-set-key "\C-ck" #'kill-line-at-point)
  (global-set-key "\C-cy" #'yank-and-indent)
  (global-set-key "\C-co" #'other-window-kill-current)
  (global-set-key "\C-cm" #'visit-this-makefile)
  (global-set-key "\C-c+" #'enlarge-window-a-bit)
  (global-set-key "\C-c-" #'shrink-window-a-bit)
  (global-set-key "\C-cf" #'dsj-buffer-file-name)
  (global-set-key "\C-cw" #'dsj-do-in-other-window)
  (global-set-key "\C-cr" #'dsj-rotate-windows)
  (global-set-key "\C-ce" #'dsj-forward-whitespace)
  (global-set-key "\C-cs" #'dsj-switch-to-scratch))

(global-set-key "\C-cb" #'bury-buffer)
(global-set-key "\M-i" #'imenu)
(global-set-key "\C-cc" #'comment-region)
(global-set-key "\C-cu" #'uncomment-region)
(global-set-key [f5] #'compile)
(global-set-key [f6] #'grep)
(global-set-key [f7] #'occur)
(global-set-key [f8] #'man-follow) ; A very useful command, fetches
                                   ; man page for thing under point

; Random coding utilities, also on website
(require 'dsj-obscure-lang nil t)

;; Unset bg and exit keys. To avoid confusion, now show a message
;; instead of unsetting.

(defun not-bound-key-message ()
  (interactive)
  (princ "Key is no longer bound. Use M-x name-of-command instead."))

(global-set-key "\C-z" #'not-bound-key-message)
(global-set-key "\C-x\C-c" #'not-bound-key-message)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode specific modifications ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Flyspell, but just for checking code comments
(when (require 'flyspell nil t)
  (setq-default flyspell-prog-text-faces '(font-lock-comment-face)))

;; Global flycheck
(when (require 'flycheck nil t)
  (setq-default flycheck-rubocop-lint-only t) ; Only check for warnings with Ruby rubocop
  (global-flycheck-mode))

;; Quick finding of project files
;; Note: Railsy/Anyproj is set up in a local file, not part of the main .emacs
(when (require 'find-file-in-project nil t)
  (setq-default ffip-limit 2048) ; limit on files to open, default is 512
  (global-set-key (kbd "C-x p") 'find-file-in-project))

;; Text-mode hook

(add-hook 'text-mode-hook
          #'(lambda ()
              (turn-on-auto-fill)
              (text-mode-hook-identify)))

;; Minor mode for auto-highlighting thing-at-point
(when (require 'idle-highlight-mode nil t)
  (setq-default idle-highlight-idle-time 1))

;; Find stuff with dumb-jump. Works very well in many languages.
;; Use with e.g. M-.
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(global-set-key (kbd "C-c j") 'dumb-jump-go-prompt)

;; hook for all programming modes
(defun default-coding-hook ()
  ; Autofill in comments
  (auto-fill-mode 1)
  ; Set fill-column to 120, we are living in modern days
  (set-fill-column 120)
  ; A better newline
  (local-set-key "\C-m" #'reindent-then-newline-and-indent)
  (condition-case nil (imenu-add-menubar-index) (error nil))
  ; Auto-highlight thing-at-point
  (and (fboundp 'idle-highlight-mode)
       (idle-highlight-mode 1))
  ; Check comment spelling
  (and (fboundp 'flyspell-prog-mode)
       (flyspell-prog-mode))
  (and (fboundp 'company-complete)
       (global-set-key (kbd "C-c q") #'company-complete)))

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; C-mode stuff

(setq-default c-basic-offset 3)
(setq-default c-default-style "bsd")
(setq-default c-hanging-braces-alist 'set-from-style)
(setq-default c-hanging-colons-alist 'set-from-style)

(add-hook 'c-mode-common-hook 'default-coding-hook)

;; Perl mode hooks

(add-hook 'perl-mode-hook 'default-coding-hook)

;; Javascript mode hooks
(add-hook 'javascript-mode-hook 'default-coding-hook)

;; Ruby mode hooks

(add-hook 'ruby-mode-hook 'default-coding-hook)

;; Use default Ruby checker for Flycheck
(add-hook 'ruby-mode-hook
          #'(lambda ()
              (setq flycheck-checker :ruby)))

;; Robe if available
(when (and (require 'robe nil t)
           (require 'company nil t))
  (global-company-mode)
  (push 'company-robe company-backends)
  (add-hook 'ruby-mode-hook 'robe-mode))

;; RVM support
(when (require 'rvm nil t)
  (rvm-use-default)
  ;; Below advice is for robe...
  (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
    (rvm-activate-corresponding-ruby)))

;; Automatic insertion of various balanced structures
(when (and (require 'smartparens-ruby nil t)
           (require 'smartparens nil t))
  (setq sp-highlight-pair-overlay nil)
  ;; Don't balance brace types...
  (sp-pair "\{" nil :actions :rem)
  (sp-pair "\(" nil :actions :rem)
  (sp-pair "\[" nil :actions :rem)
  (sp-pair "\"" nil :actions :rem)
  (sp-pair "\'" nil :actions :rem)
  (sp-local-pair 'ruby-mode "\|" nil :actions :rem)
  (add-hook 'ruby-mode-hook #'(lambda () (smartparens-mode))))

;; ruby-tools, some small utilites for ruby-mode
(require 'ruby-tools nil t)

(setq-default js-indent-level 2)

;; use web-mode for .erb files
(when (require 'web-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
  (add-hook 'web-mode-hook 'default-coding-hook))

(add-hook 'web-mode-hook
          #'(lambda()
              (define-key web-mode-map [backspace] 'my-electric-backspace)))

(when (require 'vue-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.vue$" . vue-mode)))

(add-hook 'web-mode-hook
          #'(lambda ()
              (setq-default web-mode-markup-indent-offset 2)
              (setq-default web-mode-css-indent-offset 2)
              (setq-default web-mode-code-indent-offset 2)
              (setq-default web-mode-indent-style 2)))

;; yaml mode for .yml files
(when (require 'yaml-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-hook 'yaml-mode-hook 'default-coding-hook))

;; (s)css mode for scss files. scss-mode is available in packages, but not installed by default
(if (fboundp 'scss-mode)
    (add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
  (add-to-list 'auto-mode-alist '("\\.scss$" . css-mode)))

(when (fboundp 'ssass-mode)
  (add-to-list 'auto-mode-alist '("\\.sass$" . ssass-mode)))

;; set ruby-mode for more files automatically
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.prawn$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.xml.builder$" . ruby-mode))

;; Map extra key for tags, robe uses M-.
(global-set-key (kbd "C-c .") #'xref-find-definitions)

(add-hook 'ruby-mode-hook
          #'(lambda()
              (define-key ruby-mode-map [backspace] 'my-electric-backspace)))

;; CSS-mode hooks

(setq-default css-indent-offset 2)
(add-hook 'css-mode-hook 'default-coding-hook)

;; Python mode hooks

(add-hook 'python-mode-hook 'default-coding-hook)

;; Shellscript mode customizations

; Set default shell to bash (otherwise this defaults to zsh which I
; use, but usually don't program)

(setq sh-shell-file "/bin/bash")
(setq shell-file-name "/bin/zsh")

(add-hook 'sh-mode-hook 'default-coding-hook)

;; Systemd mode
(add-to-list 'auto-mode-alist '("\\.service$" . systemd-mode))
(add-to-list 'auto-mode-alist '("\\.service.j2$" . systemd-mode))

;; Coffee mode
(when (require 'coffee-mode nil t)
  (setq-default coffee-tab-width 2)
  (setq-default coffee-indent-tabs-mode nil))

;; Lisp mode hooks

(defun lisp-mode-hook-common ()
  (default-coding-hook)
  (local-set-key (kbd "<backspace>") #'backward-delete-char-untabify)
  ;; Turn on outline minor mode, a sort of folding mode
  (outline-minor-mode 1))

(add-hook 'emacs-lisp-mode-hook
          #'lisp-mode-hook-common)
(add-hook 'lisp-mode-hook
          #'lisp-mode-hook-common)
(add-hook 'scheme-mode-hook
          #'lisp-mode-hook-common)
(add-hook 'clojure-mode-hook
          #'lisp-mode-hook-common)

;; Add macros to elisp mode indent
(and (fboundp 'lisp-indent-function)
     (fboundp 'with-buffers-matching)
     (put 'with-buffers-matching 'lisp-indent-function 1))

(setq inferior-lisp-program "/usr/bin/sbcl")

;; Compilation mode hook
;; Since the lines with compiler flags are not very interesting
;; and often very long, turn on truncate-lines.

(add-hook 'compilation-mode-hook
          #'(lambda () (setq truncate-lines 1)))

;; Reveal invisible/hidden text when moving over it. Useful in
;; conjunction with outline-mode

(when (require 'reveal nil t)
  (global-reveal-mode 1))

;; Groovy mode
(when (require 'groovy-mode nil t)
  (add-hook 'groovy-mode-hook 'default-coding-hook)
  ;;; Copied from groovy codehaus page
  ;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy
  (autoload 'groovy-mode "groovy-mode" "Groovy editing mode." t)
  (add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
  (add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Last things to do before emacs is up and running ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Reload session (you must run desktop-save once before it will work)

(when (require 'desktop nil t)
  ;; Don't recall info buffers
  (unless (member 'Info-mode desktop-modes-not-to-save)
    (push 'Info-mode desktop-modes-not-to-save))
  (setq desktop-buffers-not-to-save "\\(^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|\\.gz\\)$")
  (desktop-save-mode 1))

;; Remember mini-buffer history (on up-to-date CVS emacs)

(when (fboundp 'savehist-mode)
  (savehist-mode 1))

;; Create a buffer list if it doesn't exist.  I prefer ibuffer
;; nowadays, but use default buffer menu if ibuffer is not available.

(if (require 'ibuffer nil t)
    (progn
      (setq-default ibuffer-movement-cycle nil)
      (unless (get-buffer "*Ibuffer*")
        (ibuffer t)
        ;; Older versions of ibuffer don't seem to support this
        ;; function, and behave a bit differently otherwise also
	(when (fboundp 'ibuffer-switch-to-saved-filter-groups)
          (ibuffer-switch-to-saved-filter-groups "ibuffer-filter-groups"))
        (delete-window))
      (let ((f #'(lambda () (interactive)
                   (switch-to-buffer "*Ibuffer*")
                   (ibuffer-do-sort-by-alphabetic)
                   (ibuffer-update 1))))
        (global-set-key "\C-cl" f)
        (global-set-key "\C-x\C-b" f)))
  (progn
    (unless (get-buffer "*Buffer List*")
      (list-buffers)
      (delete-window))
    (global-set-key "\C-cl" #'(lambda () (interactive) (switch-to-buffer "*Buffer List*")))))

(require 'dsj-local nil t)

;; Workaround for terminal mode. The cursor and some other things
;; become invisible with the default colors, so change them.

(unless window-system
  (message "No window system, changing colors")
  (condition-case err
      (progn
        (set-face-background (check-face 'default) "black")
        (set-face-foreground (check-face 'default) "white smoke")
        (set-face-foreground (check-face 'font-lock-comment-face) "grey")
        (set-face-foreground (check-face
                              'font-lock-variable-name-face) "white smoke"))
    (error (message "Problem changing colors: %S" err))))

;; Start server (for shell EDITOR=emacsclient support etc.)
(server-start)

;; WE ARE DONE !
