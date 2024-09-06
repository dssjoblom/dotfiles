(require 'dsj-util)
(require 'find-file-in-project)

;; generic simple project management

;; A list of alists of projects, name + type + base dir, e.g.
;; (("my-project" . ("project-type" "/path/to/my-project")))
(defvar ap-projects ())

;; The current project name
(defvar ap-current-project "")

;; A list of alists of project file types
(defvar ap-project-file-types
  '(("clojure" . (".clj"
                  ".js"
                  ".yml"
                  "Dockerfile"
                  ".css"
                  ".sql"
                  ".vue"))))

;; A list of alists of project file prune patterns
(defvar ap-project-prune
  '(("clojure" . ("*/tmp/*"
                  "*/.cache/*"
                  "*/node_modules/*"))))

(defun ap-switch-project(p)
  (interactive (list (completing-read "Choose project: " (mapcar 'car ap-projects))))
  (let* ((project-dir  (caddr (assoc p ap-projects)))
         (project-type (cadr (assoc p ap-projects)))
         (file-types (cdr (assoc project-type ap-project-file-types)))
         (prune-files (cdr (assoc project-type ap-project-prune))))
    (setq-default ffip-project-root project-dir) ; use ffip
    (setq-default ap-current-project p)
    (setq-default ffip-prune-patterns (append prune-files ffip-prune-patterns)) ; Also ignore some Rails dirs
    (setq-default ffip-patterns (append (mapcar #'(lambda (f) (concat "*" f)) file-types) ffip-patterns))
    (ivy-mode t)
    (with-buffers-matching
          (buff
           #'(lambda (buff)
               (let ((ext (dsj-buf-ext buff))
                     (dir (dsj-buf-dir buff)))
                 (and (not (string-prefix-p project-dir dir))
                      (member ext (mapcar #'(lambda (s) (substring s 1)) file-types))))))
        (kill-buffer buff))))

(provide 'anyproj)
