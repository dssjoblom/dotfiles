;;;; Rails project utilities

;; Note: does not cover mode setup etc., put that in your .emacs.el file

;; To use the functions in this file, put the file in your load-path
;; and put (require 'railsy) in your .emacs file.

;; You must switch to an active project (with anyproj) before calling the
;; the functions in this file.

;; To bind keys, you can do (for example):

;;   (global-set-key (kbd "C-x p") 'find-file-in-project)
;;   (global-set-key (kbd "C-x v") 'find-rails-view)
;;   (global-set-key (kbd "C-x m") 'find-rails-model)
;;   (global-set-key (kbd "C-x c") 'find-rails-controller))

;; Depends on utility functions
(require 'dsj-util)

;; Depends on generic project management in anyproj
(require 'anyproj)

;; Generates TAGS file for project
(defun rails-tags ()
  (interactive)
  (shell-command (format "/usr/bin/ctags --languages=Ruby -f %s/TAGS -e -R %s" (ap-get-project-dir) (ap-get-project-dir))))

;; Set up some custom grepping code for Rails projects.

;; Defines a function called name. It will run the specified command
;; and open the result in a buffer in compilation mode. This makes it
;; possible to click on the matches and open the corresponding file(s).
(defmacro make-rails-grep-command (name cmd)
  (let ((fname (intern name)))
    `(defun ,fname (regex)
       (interactive "sRegex:")
       (let ((buffer (concat "*" ,name "*"))
             (same-window-regexps (cons (concat "\\*" ,name "\\*") same-window-regexps)))
         (shell-command (concat "cd / && " ,cmd " " regex " " (ap-get-project-dir)) buffer)
         (switch-to-buffer buffer)
         (compilation-mode 1)
         nil))))

;; Make grep commands

(make-rails-grep-command "rbgrep" "grep -n -s -R --include='*.rb' --exclude-dir=node_modules")
(make-rails-grep-command "erbgrep" "grep -n -s -R --include='*.erb' --include='*.slim' --include='*.prawn' --include='*.jbuilder'")
(make-rails-grep-command "cssgrep" "grep -n -s -R --include='*.scss' --include='*.sass'")
(make-rails-grep-command "jsgrep" "ack --nocolor --noheading -s --type-set=coffee:ext:coffee --type-set=vue:ext:vue --type=js --type=vue --type=coffee --ignore-dir='public' --ignore-dir='docs' --ignore-dir=tmp --ignore-dir=vendor --ignore-dir=coverage  --ignore-dir=app/assets/builds")

;; Show rails routes
(defun rails-routes()
  (interactive)
  (let ((buffer "*Routes*")
        (same-window-regexps (cons "*Routes*" same-window-regexps)))
    (shell-command (concat "cd " (ap-get-project-dir)  " && bundle exec rails routes") buffer)
    (switch-to-buffer buffer)
    (text-mode 1)))

;; Finder functions
(defun find-rails-view(name)
  (interactive "P")
  (let ((ffip-project-root (concat (ap-get-project-dir) "/app/views")))
    (find-file-in-project)))

(defun find-rails-model(name)
  (interactive "P")
  (let ((ffip-project-root (concat (ap-get-project-dir) "/app/models")))
    (find-file-in-project)))

(defun find-rails-controller(name)
  (interactive "P")
  (let ((ffip-project-root (concat (ap-get-project-dir) "/app/controllers")))
    (find-file-in-project)))

(defun find-rails-script(name)
  (interactive "P")
  (let ((ffip-project-root (concat (ap-get-project-dir) "/app/assets/javascripts")))
    (find-file-in-project)))

(defun find-rails-script2(name)
  (interactive "P")
  (let ((ffip-project-root (concat (ap-get-project-dir) "/app/javascript")))
    (find-file-in-project)))

(defun find-rails-spec(name)
  (interactive "P")
  (let ((ffip-project-root (concat (ap-get-project-dir) "/spec")))
    (find-file-in-project)))

(provide 'railsy)
