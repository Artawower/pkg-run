;;; pkg-run.el --- Run scripts from package.json, justfile, Makefile -*- lexical-binding: t; -*-

;; Copyright (C) 2025 darkawower

;; Author: darkawower
;; URL: https://github.com/darkawower/pkg-run
;; Package-Requires: ((emacs "27.1") (transient "0.3.0"))
;; Version: 0.2.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Universal script runner that collects commands from multiple sources:
;; package.json, justfile, Makefile and runs them via compile.

;;; Code:

(require 'json)
(require 'compile)
(require 'transient)

(defgroup pkg-run nil
  "Run scripts from various project files."
  :group 'tools
  :prefix "pkg-run-")

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
    ("Makefile"     "make"  pkg-run--parse-makefile     (filepath))
    ("Cargo.toml"   "cargo" pkg-run--parse-cargo        ()))
  "List of (filename display-name parser-function args-spec).
Args-spec defines which args to pass: root, filepath, or none."
  :type '(repeat (list string string function sexp))
  :group 'pkg-run)

(defun pkg-run--detect-package-manager (project-root)
  "Detect package manager in PROJECT-ROOT based on lock files."
  (or pkg-run-package-manager
      pkg-run-default-package-manager
      (cond
       ((file-exists-p (expand-file-name "pnpm-lock.yaml" project-root)) 'pnpm)
       ((file-exists-p (expand-file-name "bun.lockb" project-root)) 'bun)
       (t 'npm))))

(defconst pkg-run--run-commands
  '((pnpm . "pnpm run")
    (bun  . "bun run")
    (npm  . "npm run"))
  "Alist mapping package manager to run command.")

(defun pkg-run--package-manager-command (manager)
  "Return command string for MANAGER."
  (alist-get manager pkg-run--run-commands "npm run"))

(defun pkg-run--parse-package-json (root filepath)
  "Parse scripts from FILEPATH in ROOT, return alist ((name . command) ...)."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (json (json-read-file filepath))
         (scripts (gethash "scripts" json))
         (pm (pkg-run--detect-package-manager root))
         (pm-cmd (pkg-run--package-manager-command pm))
         result)
    (when scripts
      (maphash (lambda (key _value)
                 (push (cons key (format "%s %s" pm-cmd key)) result))
               scripts))
    (nreverse result)))

(defun pkg-run--parse-justfile (root)
  "Parse recipes from justfile in ROOT."
  (let ((default-directory root)
        (lines (ignore-errors
                 (process-lines "just" "--list" "--unsorted"))))
    (when lines
      (mapcar
       (lambda (line)
         (let ((recipe (car (split-string (string-trim line)))))
           (cons recipe (format "just %s" recipe))))
       (cdr lines)))))

(defun pkg-run--parse-makefile (filepath)
  "Parse targets from FILEPATH."
  (let (targets)
    (with-temp-buffer
      (insert-file-contents filepath)
      (goto-char (point-min))
      (while (re-search-forward "^\\([a-zA-Z0-9_-]+\\):" nil t)
        (let ((target (match-string 1)))
          (unless (string-prefix-p "." target)
            (push (cons target (format "make %s" target)) targets)))))
    (nreverse targets)))

(defun pkg-run--parse-cargo ()
  "Return standard Cargo commands."
  '(("build"        . "cargo build")
    ("build release" . "cargo build --release")
    ("run"          . "cargo run")
    ("test"         . "cargo test")
    ("check"        . "cargo check")
    ("clean"        . "cargo clean")
    ("doc"          . "cargo doc")
    ("clippy"       . "cargo clippy")
    ("fmt"          . "cargo fmt")
    ("bench"        . "cargo bench")))

(defun pkg-run--build-parser-args (args-spec root filepath)
  "Build argument list from ARGS-SPEC using ROOT and FILEPATH."
  (mapcar (lambda (arg)
            (pcase arg
              ('root root)
              ('filepath filepath)))
          args-spec))

(defun pkg-run--collect-all-commands ()
  "Collect commands from all found runners.
Returns alist ((display-key . (command root provider)) ...)."
  (let (result)
    (pcase-dolist (`(,file ,provider ,parser ,args-spec) pkg-run-runners)
      (when-let* ((parser (and (functionp parser) parser))
                  (root (locate-dominating-file default-directory file))
                  (filepath (expand-file-name file root)))
        (let* ((args (pkg-run--build-parser-args args-spec root filepath))
               (commands (apply parser args)))
          (dolist (cmd commands)
            (let ((display-key (format "[%s] %s" provider (car cmd))))
              (unless (assoc display-key result)
                (push (cons display-key (list (cdr cmd) root provider)) result)))))))
    (nreverse result)))

;;;###autoload
(defun pkg-run-script ()
  "Select and run a script from available project files."
  (interactive)
  (let ((candidates (pkg-run--collect-all-commands)))
    (unless candidates
      (user-error "No scripts found (checked: %s)"
                  (mapconcat #'car pkg-run-runners ", ")))
    (let* ((selected (completing-read "Run: " candidates nil t))
           (meta (cdr (assoc selected candidates)))
           (command (nth 0 meta))
           (root (nth 1 meta))
           (default-directory root))
      (compile command))))

(defconst pkg-run--install-commands
  '((pnpm . "pnpm install")
    (bun  . "bun install")
    (npm  . "npm install"))
  "Alist mapping package manager to install command.")

(defun pkg-run--package-manager-install-command (manager &optional flags)
  "Return install command string for MANAGER with optional FLAGS."
  (let ((base-cmd (alist-get manager pkg-run--install-commands "npm install")))
    (concat base-cmd (when flags (concat " " flags)))))

(defun pkg-run--find-package-json ()
  "Find the nearest package.json file in current or parent directories."
  (locate-dominating-file default-directory "package.json"))

(defun pkg-run--has-file-p (filename)
  "Check if FILENAME exists in project."
  (locate-dominating-file default-directory filename))

(defun pkg-run--menu-children (transient--layout)
  "Generate menu children based on available project files.
TRANSIENT--LAYOUT is ignored but required by transient API."
  (transient-parse-suffixes
   'pkg-run-menu
   `(,@(list ["Run" ("r" "Run script" pkg-run-script)])
     ,@(when (pkg-run--has-file-p "package.json")
         (list ["npm/pnpm/bun"
                ("i" "Install" pkg-run-npm-install)
                ("I" "Install (frozen)" pkg-run-npm-install-frozen)]))
     ,@(when (pkg-run--has-file-p "Cargo.toml")
         (list ["cargo"
                ("cb" "Build" pkg-run-cargo-build)
                ("cr" "Run" pkg-run-cargo-run)
                ("ct" "Test" pkg-run-cargo-test)
                ("cf" "Fetch deps" pkg-run-cargo-fetch)])))))

;;;###autoload
(transient-define-prefix pkg-run-menu ()
  "Universal project script runner."
  [:class transient-columns
   :setup-children pkg-run--menu-children])

(defun pkg-run-npm-install ()
  "Install npm dependencies."
  (interactive)
  (when-let ((root (pkg-run--find-package-json)))
    (let ((default-directory root))
      (compile (pkg-run--package-manager-install-command
                (pkg-run--detect-package-manager root))))))

(defconst pkg-run--frozen-install-commands
  '((pnpm . "pnpm install --frozen-lockfile")
    (bun  . "bun install --frozen-lockfile")
    (npm  . "npm ci"))
  "Alist mapping package manager to frozen install command.")

(defun pkg-run-npm-install-frozen ()
  "Install npm dependencies with frozen lockfile."
  (interactive)
  (when-let ((root (pkg-run--find-package-json)))
    (let ((default-directory root)
          (cmd (alist-get (pkg-run--detect-package-manager root)
                          pkg-run--frozen-install-commands
                          "npm ci")))
      (compile cmd))))

(defun pkg-run--cargo-root ()
  "Find Cargo.toml root."
  (locate-dominating-file default-directory "Cargo.toml"))

(defun pkg-run-cargo-build ()
  "Run cargo build."
  (interactive)
  (when-let ((root (pkg-run--cargo-root)))
    (let ((default-directory root))
      (compile "cargo build"))))

(defun pkg-run-cargo-run ()
  "Run cargo run."
  (interactive)
  (when-let ((root (pkg-run--cargo-root)))
    (let ((default-directory root))
      (compile "cargo run"))))

(defun pkg-run-cargo-test ()
  "Run cargo test."
  (interactive)
  (when-let ((root (pkg-run--cargo-root)))
    (let ((default-directory root))
      (compile "cargo test"))))

(defun pkg-run-cargo-fetch ()
  "Run cargo fetch."
  (interactive)
  (when-let ((root (pkg-run--cargo-root)))
    (let ((default-directory root))
      (compile "cargo fetch"))))

;;;###autoload
(defalias 'pkg-run 'pkg-run-menu)

(provide 'pkg-run)
;;; pkg-run.el ends here
