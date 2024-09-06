;;;; Rails project management with ffip + other tools

;; Note: does not cover mode setup etc., put that in your .emacs.el file

;; To use the functions in this file, put the file in your load-path
;; and put (require 'railsy) in your .emacs file.

;; You should (require 'find-file-in-project nil t) somewhere to get most
;; out of railsy. E.g.:

;; (when (require 'find-file-in-project nil t)
;;   (setq-default ffip-limit 2048) ; limit on files to open, default is 512
;;   ; (setq-default ffip-project-file ".hg") ; Use this if you have several projects
;;   (setq-default ffip-prune-patterns (append project-prune ffip-prune-patterns)) ; Also ignore some Rails dirs
;;   (setq-default ffip-patterns (append (mapcar #'(lambda (f) (concat "*" f)) project-file-types) ffip-patterns))
;;   (ivy-mode t) ; Allow completions like: co bo => config/boot.rb (space matches .* regexp)
;;   (global-set-key (kbd "C-x p") 'find-file-in-project)
;;   (global-set-key (kbd "C-x v") 'find-rails-view)
;;   (global-set-key (kbd "C-x m") 'find-rails-model)
;;   (global-set-key (kbd "C-x c") 'find-rails-controller))

;; Depends on utility functions
(require 'dsj-util)

;; A list of alists of projects, name + base dir, e.g.
;; (("my-project" . "/path/to/my-project"))
(defvar rails-projects ())

;; The current project name
(defvar current-project "")

(defvar project-file-types (list ".rb"
                                 ".js"
                                 ".yml"
                                 ".rake"
                                 "Gemfile"
                                 "Dockerfile"
                                 ".coffee"
                                 ".erb"
                                 ".slim"
                                 ".prawn"
                                 ".builder"
                                 ".jbuilder"
                                 ".css"
                                 ".scss"
                                 ".sass"
                                 ".vue"
                                 ".rdoc"))

(defvar project-prune (list "*/db/migrate/*"
                            "*/test/*"
                            "*/docs/*"
                            "*/public/*"
                            "*/vendor/*"
                            "*/tmp/*"
                            "*/.cache/*"
                            "*/node_modules/*"))

;; Set up some custom grepping code for Rails projects.
;; The results are opened in compilation mode so they are clickable.

;; Returns current project directory
(defun get-project-dir()
  (cdr (assoc current-project rails-projects)))

;; Generates TAGS file for project
(defun rails-tags ()
  (interactive)
  (shell-command (format "/usr/bin/ctags --languages=Ruby -f %s/TAGS -e -R %s" (get-project-dir) (get-project-dir))))

;; Creates a function called name. It will run the specified command
;; and open the result in a buffer in compilation mode. This makes it
;; possible to click on the matches and open the corresponding file(s).
(defun make-rails-grep-command (name cmd)
  (eval
   (let ((fname (intern name)))
     `(defun ,fname (regex)
        (interactive "sRegex:")
        (let ((buffer (concat "*" ,name "*"))
              (same-window-regexps (cons (concat "\\*" ,name "\\*") same-window-regexps)))
          (shell-command (concat "cd / && " ,cmd " " regex " " (get-project-dir)) buffer)
          (switch-to-buffer buffer)
          (compilation-mode 1))))))

;; Switch projects, performing several actions:

;; - killing any open old project buffers (rb/erb/etc. files)
;; - automatically opening all project files with file-cache
;; - setting ffip root to project directory
;; - setting up grep commands (can be run interactively):
;;   rbgrep, erbgrep, cssgrep, jsgrep
(defun switch-project(p)
  (interactive
   (list
    (completing-read "Choose project: " (mapcar 'car rails-projects))))
  (let ((project-dir (cdr (assoc p rails-projects))))
    (if (not project-dir)
        (message "Project does not exist")
      (setq-default ffip-project-root project-dir) ; use ffip
      (setq-default current-project p)
      ;; Add to file-cache
      ;(file-cache-add-directory-recursively project-dir (regexp-opt project-file-types))
      ;; Add grep commands
      (make-rails-grep-command "rbgrep" "grep -n -s -R --include='*.rb' --exclude-dir=node_modules")
      (make-rails-grep-command "erbgrep" "grep -n -s -R --include='*.erb' --include='*.slim' --include='*.prawn'")
      (make-rails-grep-command "cssgrep" "grep -n -s -R --include='*.scss' --include='*.sass'")
      (make-rails-grep-command "jsgrep" "ack --nocolor --noheading -s --type-set=coffee:ext:coffee --type=js --type=coffee --ignore-dir='public' --ignore-dir='docs' --ignore-dir=tmp --ignore-dir=vendor --ignore-dir=coverage")

      (when (fboundp #'refresh-frame-title)
        (refresh-frame-title)) ; put project name in frame title
      (with-buffers-matching
          (buff
           #'(lambda (buff)
               (let ((ext (dsj-buf-ext buff))
                     (dir (dsj-buf-dir buff)))
                 (and (not (string-prefix-p project-dir dir))
                      (member ext (mapcar #'(lambda (s) (substring s 1)) project-file-types))))))
        (kill-buffer buff)))))

(defun rails-routes()
  (interactive)
  (let ((buffer "*Routes*")
        (same-window-regexps (cons "*Routes*" same-window-regexps)))
    (shell-command (concat "cd " (get-project-dir)  " && bundle exec rails routes") buffer)
    (switch-to-buffer buffer)
    (text-mode 1)))

(defun find-rails-view(name)
  (interactive "P")
  (let ((ffip-project-root (concat (get-project-dir) "/app/views")))
    (find-file-in-project)))

(defun find-rails-model(name)
  (interactive "P")
  (let ((ffip-project-root (concat (get-project-dir) "/app/models")))
    (find-file-in-project)))

(defun find-rails-controller(name)
  (interactive "P")
  (let ((ffip-project-root (concat (get-project-dir) "/app/controllers")))
    (find-file-in-project)))

(defun find-rails-script(name)
  (interactive "P")
  (let ((ffip-project-root (concat (get-project-dir) "/app/assets/javascripts")))
    (find-file-in-project)))

(defun find-rails-script2(name)
  (interactive "P")
  (let ((ffip-project-root (concat (get-project-dir) "/app/javascript")))
    (find-file-in-project)))

(defun find-rails-spec(name)
  (interactive "P")
  (let ((ffip-project-root (concat (get-project-dir) "/spec")))
    (find-file-in-project)))

(provide 'railsy)
