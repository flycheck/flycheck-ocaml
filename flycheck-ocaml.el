;;; flycheck-ocaml.el --- Flycheck: OCaml support    -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Sebastian Wiesner <swiesner@lunaryorn.com>

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; URL: https://github.com/flycheck/flycheck-ocaml
;; Keywords: convenience, tools, languages
;; Version: 0.1-cvs
;; Package-Requires: ((emacs "24.1") (flycheck "0.22-cvs1") (merlin "2.0"))

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

;; OCaml support for Flycheck.

;; Provide the `ocaml-merlin' syntax checker which uses Merlin Mode (see URL
;; `https://github.com/the-lambda-church/merlin') to check OCaml buffers for
;; errors.

;;; Code:

(require 'merlin)
(require 'flycheck)

(defun flycheck-ocaml-merlin-start (checker callback)
  "")

(flycheck-define-generic-checker 'ocaml-merlin
  "A syntax checker for OCaml using Merlin Mode.

See URL `https://github.com/the-lambda-church/merlin'."
  :start #'flycheck-ocaml-merlin-check
  :modes '(caml-mode tuareg-mode)
  :predicate (lambda () merlin-mode))

(provide 'flycheck-ocaml)

;;; flycheck-ocaml.el ends here
