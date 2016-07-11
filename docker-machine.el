;;; docker-machine.el --- Emacs interface to the Docker machine

;; Author: Ben Weintraub <benweintraub34@gmail.com>
;; URL: https://github.com/iowaguy/docker-machine.el

;; This file is NOT part of GNU Emacs.

(require 's)
(require 'tabulated-list-ext)
(require 'eieio)

(defclass machine ()
  ((name         :initarg :name         :initform nil)
   (active       :initarg :active       :initform nil)
   (driver       :initarg :driver       :initform nil)
   (state        :initarg :state        :initform nil)
   (url          :initarg :url          :initform nil)
   (swarm        :initarg :swarm        :initform nil)))

(defmethod docker-machine-to-tabulated-list ((this machine))
  "Convert `docker-machine' to tabulated list."
  (list (oref this :name)
        `[,(oref this :name)
          ,(oref this :active)
          ,(oref this :driver)
          ,(oref this :state)
          ,(oref this :url)
          ,(oref this :swarm)]))

(defun make-docker-machine (name &optional active driver state url swarm)
  "Helper to create a `eieio` docker machine object."
  (machine name :name name :active active :driver driver :state state :url url :swarm swarm))

(defun docker-machine-parse (line)
  "Convert LINE from 'docker machines' to `machine'."
  ;; (apply #'make-docker-machine (s-split " " line)))
  (apply #'make-docker-machine (s-split " \\{1,10\\}" line)))

(defun docker-machine-ls ()
  "Execute `docker-machine ls' command"
  (interactive)
  (docker-machine "ls" nil))

(defun docker-machine-env (machine)
  "Execute `docker-machine env <machine>' command"
  (interactive (list (docker-machine-read "Machine name: ")))
  (docker-machine-raw-out-to-env-vars (docker-machine "env" machine)))

(defun docker-machine-read (prompt)
  "Read machine name from minibuffer."
  (read-from-minibuffer prompt))

(defun docker-machine (action &rest args)
  "Execute docker-machine ACTION passing arguments ARGS."
  (let ((command (format "docker-machine %s %s" action (s-join " " (-non-nil args)))))
    (message command)
    (shell-command-to-string command)))

(defun docker-machine-raw-out-to-env-vars (env-command-output)
  "Takes the output from `docker-machine env <machine>' and sets up environment accordingly."
  (mapc
   'docker-machine-set-env-from-kv-pair
   (docker-machine-env-raw-out-to-pairs env-command-output)))

(defun docker-machine-set-env-from-kv-pair (pair)
  "Takes a pair, and sets it as an environment variable."
  (setenv (car pair)
	  (s-with (nth 1 pair) (s-chop-suffix "\"") (s-chop-prefix "\""))))

(defun docker-machine-env-raw-out-to-pairs (raw)
  "Takes bash output, and returns a list of environment variable key-value pairs."
  (mapcar
    #'(lambda (x)
	(split-string (nth 1 (split-string x " ")) "="))
    (remove-if
     #'(lambda (y)
	 (not (string-prefix-p "export" y)))
     (s-lines raw))))

(defun docker-machines-refresh ()
  (setq tabulated-list-entries (mapcar 'docker-machine-to-tabulated-list (docker-get-machines))))

(defun docker-get-machines ()
  "Get machines as eieio objects."
  (let* ((data (docker-machine-ls))
         (lines (s-split "\n" data t))
         (lines (cdr lines)))
    (mapcar 'docker-machine-parse lines)))


(defun docker-machines ()
  "List docker machines."
  (interactive)
  (pop-to-buffer "*docker-machines*")
  (docker-machines-mode)
  (docker-machines-refresh)
  (tabulated-list-revert))

(define-derived-mode docker-machines-mode tabulated-list-ext-mode "Machines Menu"
  "Major mode for handling a list of docker machines."
  (setq tabulated-list-format [("Name" 25 t)
                               ("Active" 9 t)
                               ("Driver" 13 t)
                               ("State" 10 t)
                               ("URL" 28 t)
                               ("Swarm" 10 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Name" nil))
  (add-hook 'tabulated-list-revert-hook 'docker-machines-refresh nil t)
  (tabulated-list-init-header))

(provide 'docker-machine)

;;; docker-machine.el ends here
