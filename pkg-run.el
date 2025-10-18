;;; pkg-run.el --- Run package.json scripts with completion -*- lexical-binding: t; -*-

;; Copyright (C) 2025 darkawower

;; Author: darkawower
;; URL: https://github.com/darkawower/pkg-run
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1.0

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
;; This package reads the scripts section from package.json,
;; provides completion for script selection, and runs the selected
;; script using compile with auto-detected package manager (pnpm/bun/npm).

;;; Code:

(require 'json)
(require 'compile)

(defgroup pkg-run nil
  "Run package.json scripts with completion."
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

(defun pkg-run--find-package-json ()
  "Find the nearest package.json file in current or parent directories."
  (locate-dominating-file default-directory "package.json"))

(defun pkg-run--read-scripts (package-json-path)
  "Read scripts section from PACKAGE-JSON-PATH and return list of script names."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (json (json-read-file package-json-path))
         (scripts (gethash "scripts" json)))
    (when scripts
      (let (result)
        (maphash (lambda (key _value)
                   (push key result))
                 scripts)
        (nreverse result)))))

(defun pkg-run--detect-package-manager (project-root)
  "Detect package manager in PROJECT-ROOT based on lock files."
  (or pkg-run-default-package-manager
      (cond
       ((file-exists-p (expand-file-name "pnpm-lock.yaml" project-root)) 'pnpm)
       ((file-exists-p (expand-file-name "bun.lockb" project-root)) 'bun)
       (t 'npm))))

(defun pkg-run--package-manager-command (manager)
  "Return command string for MANAGER (pnpm, bun, or npm)."
  (pcase manager
    ('pnpm "pnpm")
    ('bun "bun")
    ('npm "npm run")
    (_ "npm run")))

;;;###autoload
(defun pkg-run-script ()
  "Select and run a script from package.json using completion."
  (interactive)
  (let* ((project-root (pkg-run--find-package-json)))
    (unless project-root
      (user-error "No package.json found in current directory or parent directories"))
    (let* ((package-json-path (expand-file-name "package.json" project-root))
           (scripts (pkg-run--read-scripts package-json-path)))
      (unless scripts
        (user-error "No scripts found in package.json"))
      (let* ((selected-script (completing-read "Select script: " scripts nil t))
             (package-manager (pkg-run--detect-package-manager project-root))
             (pm-cmd (pkg-run--package-manager-command package-manager))
             (default-directory project-root)
             (compile-command (format "%s %s" pm-cmd selected-script)))
        (compile compile-command)))))

(provide 'pkg-run)
;;; pkg-run.el ends here
