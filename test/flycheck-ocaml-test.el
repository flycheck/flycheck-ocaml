;;; flycheck-ocaml-test.el --- Flycheck OCaml: Test cases  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Sebastian Wiesner <swiesner@lunaryorn.com>

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; URL: https://github.com/flycheck/flycheck-ocaml

;; This file is not part of GNU Emacs.

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

;; Test cases Flycheck OCaml.

;;; Code:

(require 'flycheck-ocaml)
(require 'flycheck-ert)
(require 'tuareg)

(message "Running tests on Emacs %s" emacs-version)

(defconst flycheck-ocaml-test-directory
  (let ((filename (if load-in-progress load-file-name (buffer-file-name))))
    (expand-file-name "test/" (locate-dominating-file filename "Cask")))
  "Test suite directory, for resource loading.")

(defun flycheck-ocaml-test-get-merlin-errors ()
  "Get Merlin errors in the current buffer."
  (merlin-mode)
  (merlin-sync-to-point (point-max) 'skip-marker)
  (merlin-send-command 'errors))

(ert-deftest flycheck-ocaml-merlin-parse-error/error ()
  :tags '(parsing)
  (flycheck-ert-with-resource-buffer "ocaml-error.ml"
    (tuareg-mode)
    (let ((errors (mapcar (lambda (alist)
                            (flycheck-ocaml-merlin-parse-error
                             alist 'ocaml-merlin (current-buffer)))
                          (flycheck-ocaml-test-get-merlin-errors))))
      (should (equal errors
                     (list (flycheck-error-new-at 1 23 'error "This expression has type unit but an expression was expected of type string"
                                                  :checker 'ocaml-merlin)))))))

(ert-deftest flycheck-ocaml-merlin-parse-error/warning ()
  :tags '(parsing)
  (flycheck-ert-with-resource-buffer "ocaml-warning.ml"
    (tuareg-mode)
    (let ((errors (mapcar (lambda (alist)
                            (flycheck-ocaml-merlin-parse-error
                             alist 'ocaml-merlin (current-buffer)))
                          (flycheck-ocaml-test-get-merlin-errors))))
      (should (equal errors
                     (list (flycheck-error-new-at 4 9 'warning "this pattern-matching is not exhaustive. Here is an example of a value that is not matched: Bar"
                                                  :checker 'ocaml-merlin)))))))

(flycheck-ert-initialize flycheck-ocaml-test-directory)

(provide 'flycheck-ocaml-test)

;;; flycheck-ocaml-test.el ends here
