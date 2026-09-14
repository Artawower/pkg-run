;;; pkg-run.el --- Run project tasks from runner and project files -*- lexical-binding: t; -*-

;; Copyright (C) 2025 darkawower

;; Author: darkawower
;; URL: https://github.com/darkawower/pkg-run
;; Package-Requires: ((emacs "27.1") (transient "0.3.0"))
;; Version: 0.4

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:
;; Universal project task runner for Emacs.
;;
;; When the external `runner' executable is available, pkg-run uses
;; `runner list --json' as the primary source for declared project tasks.
;; Language-level defaults (for example `go build ./...' or `cargo test')
;; are still added by pkg-run itself.
;;
;; If `runner' is not installed, pkg-run falls back to its native parsers
;; for package.json, justfile, Makefile, and Cargo.toml.

;;; Code:

(require 'json)
(require 'compile)
(require 'subr-x)
(require 'transient)

(defgroup pkg-run nil
  "Run project tasks from multiple sources."
  :group 'tools
  :prefix "pkg-run-")

(defcustom pkg-run-use-runner t
  "Use the external `runner' executable when it is available.

When non-nil and `runner' can be found in `exec-path', declared project
commands are collected through `runner list --json'.  Language-specific
commands maintained by pkg-run are merged into that list afterwards.

When nil, always use pkg-run's native parsers."
  :type 'boolean
  :group 'pkg-run)

(defcustom pkg-run-default-package-manager nil
  "Default package manager to use (pnpm, bun, or npm).
If nil, auto-detect based on lock files."
  :type '(choice (const :tag "Auto-detect" nil)
                 (const :tag "pnpm" pnpm)
                 (const :tag "bun" bun)
                 (const :tag "npm" npm))
  :group 'pkg-run)

(defcustom pkg-run-package-manager nil
  "Current package manager override.
If non-nil, use this instead of auto-detection or default."
  :type '(choice (const :tag "Use default" nil)
                 (const :tag "pnpm" pnpm)
                 (const :tag "bun" bun)
                 (const :tag "npm" npm))
  :group 'pkg-run)

(defcustom pkg-run-runners
  '(("package.json" "npm"   pkg-run--parse-package-json (root filepath))
    ("justfile"     "just"  pkg-run--parse-justfile     (root))
    ("Justfile"     "just"  pkg-run--parse-justfile     (root))
    ("Makefile"     "make"  pkg-run--parse-makefile     (filepath)))
  "Native fallback runners.

Each item has the form (FILENAME DISPLAY-NAME PARSER-FUNCTION ARGS-SPEC).
ARGS-SPEC defines which arguments are passed to the parser: `root',
`filepath', or neither.

These parsers are used when `runner' is unavailable or disabled."
  :type '(repeat (list string string function sexp))
  :group 'pkg-run)

(defconst pkg-run--run-commands
  '((pnpm . "pnpm run")
    (bun  . "bun run")
    (npm  . "npm run"))
  "Alist mapping package manager to run command.")

(defconst pkg-run--install-commands
  '((pnpm . "pnpm install")
    (bun  . "bun install")
    (npm  . "npm install"))
  "Alist mapping package manager to install command.")

(defconst pkg-run--frozen-install-commands
  '((pnpm . "pnpm install --frozen-lockfile")
    (bun  . "bun install --frozen-lockfile")
    (npm  . "npm ci"))
  "Alist mapping package manager to frozen install command.")

(defconst pkg-run--cargo-commands
  '("build" "run" "test" "check" "clean" "doc" "clippy" "fmt" "bench" "fetch")
  "Standard Cargo commands added even when they are not declared as tasks.")

(defconst pkg-run--go-commands
  '(("build" . "go build ./...")
    ("test"  . "go test ./...")
    ("vet"   . "go vet ./...")
    ("fmt"   . "go fmt ./...")
    ("tidy"  . "go mod tidy"))
  "Standard Go commands added when a go.mod is present.")

(defun pkg-run--detect-package-manager (project-root)
  "Detect package manager in PROJECT-ROOT based on lock files."
  (or pkg-run-package-manager
      pkg-run-default-package-manager
      (cond
       ((file-exists-p (expand-file-name "pnpm-lock.yaml" project-root)) 'pnpm)
       ((or (file-exists-p (expand-file-name "bun.lock" project-root))
            (file-exists-p (expand-file-name "bun.lockb" project-root)))
        'bun)
       (t 'npm))))

(defun pkg-run--package-manager-command (manager)
  "Return run command string for MANAGER."
  (alist-get manager pkg-run--run-commands "npm run"))

(defun pkg-run--package-manager-install-command (manager &optional flags)
  "Return install command string for MANAGER with optional FLAGS."
  (let ((base-cmd (alist-get manager pkg-run--install-commands "npm install")))
    (concat base-cmd (when flags (concat " " flags)))))

(defun pkg-run--parse-package-json (root filepath)
  "Parse scripts from FILEPATH in ROOT.
Return a list of (NAME COMMAND PROVIDER) triples."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (json (json-read-file filepath))
         (scripts (gethash "scripts" json))
         (pm (pkg-run--detect-package-manager root))
         (pm-cmd (pkg-run--package-manager-command pm))
         (provider (symbol-name pm))
         result)
    (when scripts
      (maphash
       (lambda (key _value)
         (push (list key (format "%s %s" pm-cmd key) provider) result))
       scripts))
    (nreverse result)))

(defun pkg-run--parse-justfile (root)
  "Parse recipes from justfile in ROOT.
Return a list of (NAME COMMAND PROVIDER) triples."
  (let ((default-directory root)
        (lines (ignore-errors
                 (process-lines "just" "--list" "--unsorted"))))
    (when lines
      (mapcar
       (lambda (line)
         (let ((recipe (car (split-string (string-trim line)))))
           (list recipe (format "just %s" recipe) "just")))
       (cdr lines)))))

(defun pkg-run--parse-makefile (filepath)
  "Parse targets from FILEPATH.
Return a list of (NAME COMMAND PROVIDER) triples."
  (let (targets)
    (with-temp-buffer
      (insert-file-contents filepath)
      (goto-char (point-min))
      (while (re-search-forward "^\\([a-zA-Z0-9_.-]+\\):" nil t)
        (let ((target (match-string 1)))
          (unless (string-prefix-p "." target)
            (push (list target (format "make %s" target) "make") targets)))))
    (nreverse targets)))

(defun pkg-run--build-parser-args (args-spec root filepath)
  "Build argument list from ARGS-SPEC using ROOT and FILEPATH."
  (mapcar
   (lambda (arg)
     (pcase arg
       ('root root)
       ('filepath filepath)))
   args-spec))


(defconst pkg-run--runner-root-markers
  '("runner.toml"
    "justfile" "Justfile"
    "Makefile" "makefile" "GNUmakefile"
    "Taskfile.yml" "Taskfile.yaml"
    "mise.toml" ".mise.toml"
    "package.json" "package.json5" "package.yaml"
    "pnpm-workspace.yaml" "turbo.json" "nx.json"
    "deno.json" "deno.jsonc"
    "Cargo.toml"
    "go.mod"
    "pyproject.toml" "uv.lock" "poetry.lock" "Pipfile"
    "Gemfile" "composer.json")
  "Files that can identify a useful directory for runner discovery.")

(defun pkg-run--nearest-marker-root (&optional directory)
  "Return nearest ancestor of DIRECTORY containing a runner project marker.

When several markers are found at different ancestors, prefer the closest
one to DIRECTORY.  Return DIRECTORY itself when no marker can be found."
  (let* ((start (file-name-as-directory
                 (expand-file-name (or directory default-directory))))
         (roots
          (delq nil
                (mapcar (lambda (marker)
                          (locate-dominating-file start marker))
                        pkg-run--runner-root-markers))))
    (or (car (sort (delete-dups roots)
                   (lambda (a b) (> (length a) (length b)))))
        start)))

(defun pkg-run--runner-executable ()
  "Return the absolute path to the `runner' binary, or nil.

Prefer `runner' from `exec-path'.  When only the `run' facade is visible,
look for its sibling `runner' binary as well.  runner-run normally installs
both executables next to each other."
  (or (executable-find "runner")
      (when-let* ((run (executable-find "run"))
                  (sibling (expand-file-name "runner"
                                             (file-name-directory run)))
                  ((file-executable-p sibling)))
        sibling)))

(defun pkg-run--runner-enabled-p ()
  "Return non-nil when the runner backend should be used."
  (and pkg-run-use-runner
       (pkg-run--runner-executable)))

(defun pkg-run--runner-list-json (&optional directory)
  "Return parsed output of `runner list --json' for DIRECTORY.

Discovery is anchored at the nearest ancestor containing a known runner
project marker.  This matters for buffers opened below a root-level Justfile,
Makefile, mise.toml, package.json, and similar files.

Signal an error when runner exits unsuccessfully or emits invalid JSON."
  (let* ((runner (or (pkg-run--runner-executable)
                     (error "runner executable not found")))
         (root (pkg-run--nearest-marker-root directory)))
    (with-temp-buffer
      (let ((status
             (process-file
              runner nil t nil
              "--dir" root
              "--no-warnings"
              "--schema-version" "1"
              "list"
              "--json")))
        (unless (and (integerp status) (zerop status))
          (error "runner list failed in %s (exit %s): %s"
                 root status (string-trim (buffer-string))))
        (goto-char (point-min))
        (json-parse-buffer
         :object-type 'hash-table
         :array-type 'list
         :null-object nil
         :false-object nil)))))

(defun pkg-run--runner-task-description (task)
  "Return the best human-readable description for runner TASK."
  (or (gethash "description" task)
      (when-let ((alias (gethash "alias_of" task)))
        (format "→ %s" alias))
      (when-let ((target (gethash "passthrough_to" task)))
        (format "→ %s" target))))

(defun pkg-run--collect-runner-commands ()
  "Collect declared project commands from runner.
Return commands in pkg-run's internal candidate format."
  (let* ((payload (pkg-run--runner-list-json))
         (root (gethash "root" payload))
         (tasks (gethash "tasks" payload))
         result)
    (dolist (task tasks)
      (let* ((name (gethash "name" task))
             (source (gethash "source" task))
             (description (pkg-run--runner-task-description task))
             (target (format "%s:%s" source name))
             (display
              (concat
               (format "[%s] %s" source name)
               (if (and description (not (string-empty-p description)))
                   (format " — %s" description)
                 "")))
             (runner (pkg-run--runner-executable))
             (command
              (format "%s run %s"
                      (shell-quote-argument runner)
                      (shell-quote-argument target))))
        (push (cons display (list command root source)) result)))
    (nreverse result)))

(defun pkg-run--collect-native-commands ()
  "Collect declared commands using pkg-run's native fallback parsers."
  (let (result)
    (pcase-dolist (`(,file ,provider ,parser ,args-spec) pkg-run-runners)
      (when-let* ((parser (and (functionp parser) parser))
                  (root (locate-dominating-file default-directory file))
                  (filepath (expand-file-name file root)))
        (let* ((args (pkg-run--build-parser-args args-spec root filepath))
               (commands (apply parser args)))
          (dolist (cmd commands)
            (pcase-let ((`(,name ,command ,parsed-provider) cmd))
              (let* ((effective-provider (or parsed-provider provider))
                     (display-key
                      (format "[%s] %s" effective-provider name)))
                (push
                 (cons display-key
                       (list command root effective-provider))
                 result)))))))
    (nreverse result)))

(defun pkg-run--find-package-json ()
  "Find the nearest package.json in current or parent directories."
  (locate-dominating-file default-directory "package.json"))

(defun pkg-run--cargo-root ()
  "Find the nearest Cargo.toml root."
  (locate-dominating-file default-directory "Cargo.toml"))

(defun pkg-run--go-root ()
  "Find the nearest go.mod root."
  (locate-dominating-file default-directory "go.mod"))

(defun pkg-run--collect-builtin-commands ()
  "Collect pkg-run-owned commands that runner does not need to declare.

This includes dependency installation commands and language-specific
standard commands."
  (let (result)
    ;; Package manager helpers.
    (when-let ((root (pkg-run--find-package-json)))
      (let ((pm (pkg-run--detect-package-manager root)))
        (push
         (cons (format "[%s] install" pm)
               (list (pkg-run--package-manager-install-command pm)
                     root
                     (symbol-name pm)))
         result)
        (push
         (cons (format "[%s] install (frozen)" pm)
               (list (alist-get pm pkg-run--frozen-install-commands "npm ci")
                     root
                     (symbol-name pm)))
         result)))

    ;; Cargo defaults are intentionally kept even when runner is available.
    (when-let ((root (pkg-run--cargo-root)))
      (dolist (cmd pkg-run--cargo-commands)
        (push
         (cons (format "[cargo] %s" cmd)
               (list (format "cargo %s" cmd) root "cargo"))
         result)))

    ;; Go defaults are inferred from go.mod and do not need to be declared.
    (when-let ((root (pkg-run--go-root)))
      (dolist (entry pkg-run--go-commands)
        (pcase-let ((`(,name . ,command) entry))
          (push
           (cons (format "[go] %s" name)
                 (list command root "go"))
           result))))

    (nreverse result)))

(defun pkg-run--merge-command-groups (&rest groups)
  "Merge command GROUPS while preserving the first candidate for each label."
  (let (result)
    (dolist (group groups)
      (dolist (candidate group)
        (unless (assoc (car candidate) result)
          (push candidate result))))
    (nreverse result)))

(defun pkg-run--collect-discovered-commands ()
  "Collect declared project commands from runner or native parsers."
  (if (pkg-run--runner-enabled-p)
      (condition-case err
          (pkg-run--collect-runner-commands)
        (error
         (message "pkg-run: runner failed, using native fallback: %s"
                  (error-message-string err))
         (pkg-run--collect-native-commands)))
    (pkg-run--collect-native-commands)))

(defun pkg-run--collect-all-commands ()
  "Collect declared tasks and merge pkg-run language defaults into them."
  ;; Declared tasks win on duplicate labels.  Built-ins only fill gaps.
  (pkg-run--merge-command-groups
   (pkg-run--collect-discovered-commands)
   (pkg-run--collect-builtin-commands)))


(defun pkg-run-debug-info ()
  "Show discovery information useful for diagnosing pkg-run."
  (interactive)
  (let ((runner (pkg-run--runner-executable))
        (run (executable-find "run"))
        (just (executable-find "just")))
    (with-help-window "*pkg-run debug*"
      (princ (format "default-directory: %s\n\n" default-directory))
      (princ (format "runner: %s\n" (or runner "not found")))
      (princ (format "run:    %s\n" (or run "not found")))
      (princ (format "just:   %s\n" (or just "not found")))
      (princ (format "runner dir: %s\n\n"
                     (pkg-run--nearest-marker-root)))
      (dolist (file '("package.json" "justfile" "Justfile" "Makefile"
                      "Cargo.toml" "go.mod"))
        (princ (format "%-13s %s\n"
                       file
                       (or (locate-dominating-file default-directory file)
                           "not found"))))
      (princ "\nrunner backend:\n")
      (if runner
          (condition-case err
              (let* ((payload (pkg-run--runner-list-json))
                     (tasks (gethash "tasks" payload)))
                (princ (format "root: %s\n" (gethash "root" payload)))
                (princ (format "tasks: %d\n" (length tasks))))
            (error
             (princ (format "ERROR: %s\n" (error-message-string err)))))
        (princ "disabled: runner executable not found\n"))
      (princ (format "\nnative commands: %d\n"
                     (length (pkg-run--collect-native-commands))))
      (princ (format "builtin commands: %d\n"
                     (length (pkg-run--collect-builtin-commands)))))))

;;;###autoload
(defun pkg-run-script ()
  "Select and run a project command."
  (interactive)
  (let ((candidates (pkg-run--collect-all-commands)))
    (unless candidates
      (user-error "No project commands found"))
    (let* ((selected (completing-read "Run: " candidates nil t))
           (meta (cdr (assoc selected candidates)))
           (command (nth 0 meta))
           (root (nth 1 meta))
           (default-directory root))
      (compile command))))

(defun pkg-run--has-file-p (filename)
  "Return non-nil when FILENAME exists in this project or a parent."
  (locate-dominating-file default-directory filename))

(defun pkg-run--menu-children (_transient--layout)
  "Generate transient children based on available project files."
  (transient-parse-suffixes
   'pkg-run-menu
   `(,@(list ["Run" ("r" "Run project command" pkg-run-script)])
     ,@(when (pkg-run--runner-enabled-p)
         (list ["runner" ("l" "Choose task" pkg-run-script)]))
     ,@(when (pkg-run--has-file-p "package.json")
         (list ["npm/pnpm/bun"
                ("i" "Install" pkg-run-npm-install)
                ("I" "Install (frozen)" pkg-run-npm-install-frozen)]))
     ,@(when (pkg-run--has-file-p "Cargo.toml")
         (list ["cargo"
                ("b" "Build" (lambda () (interactive) (pkg-run-cargo "build")))
                ("r" "Run" (lambda () (interactive) (pkg-run-cargo "run")))
                ("t" "Test" (lambda () (interactive) (pkg-run-cargo "test")))
                ("k" "Check" (lambda () (interactive) (pkg-run-cargo "check")))
                ("c" "Clean" (lambda () (interactive) (pkg-run-cargo "clean")))
                ("l" "Clippy" (lambda () (interactive) (pkg-run-cargo "clippy")))
                ("f" "Fmt" (lambda () (interactive) (pkg-run-cargo "fmt")))]))
     ,@(when (pkg-run--has-file-p "go.mod")
         (list ["go"
                ("b" "Build" (lambda () (interactive) (pkg-run-go "build")))
                ("t" "Test" (lambda () (interactive) (pkg-run-go "test")))
                ("v" "Vet" (lambda () (interactive) (pkg-run-go "vet")))
                ("f" "Fmt" (lambda () (interactive) (pkg-run-go "fmt")))
                ("m" "Mod tidy" (lambda () (interactive) (pkg-run-go "tidy")))])))))

;;;###autoload
(transient-define-prefix pkg-run-menu ()
  "Universal project task runner."
  [:class transient-columns
          :setup-children pkg-run--menu-children])

(defun pkg-run-npm-install ()
  "Install npm dependencies."
  (interactive)
  (when-let ((root (pkg-run--find-package-json)))
    (let ((default-directory root))
      (compile
       (pkg-run--package-manager-install-command
        (pkg-run--detect-package-manager root))))))

(defun pkg-run-npm-install-frozen ()
  "Install npm dependencies with frozen lockfile."
  (interactive)
  (when-let ((root (pkg-run--find-package-json)))
    (let ((default-directory root)
          (cmd (alist-get (pkg-run--detect-package-manager root)
                          pkg-run--frozen-install-commands
                          "npm ci")))
      (compile cmd))))

(defun pkg-run-cargo (command)
  "Run Cargo COMMAND in the nearest Cargo project root."
  (interactive
   (list (completing-read "Cargo command: " pkg-run--cargo-commands nil t)))
  (when-let ((root (pkg-run--cargo-root)))
    (let ((default-directory root))
      (compile (format "cargo %s" command)))))

(defun pkg-run-go (command)
  "Run Go COMMAND in the nearest Go project root."
  (interactive
   (list
    (completing-read
     "Go command: "
     (mapcar #'car pkg-run--go-commands)
     nil t)))
  (when-let* ((root (pkg-run--go-root))
              (entry (assoc command pkg-run--go-commands)))
    (let ((default-directory root))
      (compile (cdr entry)))))

;;;###autoload
(defalias 'pkg-run 'pkg-run-menu)

(provide 'pkg-run)
;;; pkg-run.el ends here
