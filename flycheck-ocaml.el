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

(defconst flycheck-ocaml-merlin-message-re
  (rx string-start
      ;; Skip over leading spaces and punctuation
      (zero-or-more (any punct space control))
      ;; Take the level...
      (group-n 1 (or "Warning" "Error"))
      ;; ...and skip over trailing digits (e.g. Warning 8:)
      (zero-or-more (any space digit)) ": "
      ;; The rest is the real error message
      (group-n 2 (one-or-more anything)) string-end)
  "Regular expression to parse a Merlin error message.")

(defun flycheck-ocaml-merlin-parse-message (message)
  "Parse an error MESSAGE from a Merlin error.

Return `(LEVEL . PARSED-MESSAGE)', where LEVEL is the Flycheck
error level, and PARSED-MESSAGE is the real error message with
irrelevant parts removed."
  (when (string-match flycheck-ocaml-merlin-message-re message)
    (let ((level (pcase (match-string 1 message)
                   (`"Warning" 'warning)
                   (`"Error" 'error)
                   (level (lwarn 'flycheck-ocaml :error
                                 "Unknown error level %S" level)))))
      (cons level
            ;; Collapse whitespace in error messages
            (replace-regexp-in-string (rx (one-or-more (any space "\n" "\r")))
                                      " " (string-trim (match-string 2 message))
                                      'fixed-case 'literal)))))

(defun flycheck-ocaml-merlin-parse-error (alist checker buffer)
  "Parse a Merlin error ALIST from CHECKER in BUFFER into a `flycheck-error'.

Return the corresponding `flycheck-error'."
  (let* ((orig-message (cdr (assq 'message alist)))
         (start (cdr (assq 'start alist)))
         (line (or (cdr (assq 'line start)) 1))
         (column (cdr (assq 'col start))))
    (when orig-message
      (pcase-let* ((`(,level . ,message)
                    (flycheck-ocaml-merlin-parse-message orig-message)))
        (if level
            (flycheck-error-new-at line column level message
                                   :checker checker
                                   :buffer buffer
                                   :filename (buffer-file-name))
          (lwarn 'flycheck-ocaml :error
                 "Failed to parse Merlin error message %S from %S"
                 orig-message alist))))))

(defun flycheck-ocaml-merlin-start (checker callback)
  "Start a Merlin syntax check with CHECKER.

CALLBACK is the status callback passed by Flycheck."
  ;; Sync the buffer contents with Merlin.
  (merlin-sync-to-point (point-max) t)
  ;; Put the current buffer into the closure environment so that we have access
  ;; to it later.
  (let ((buffer (current-buffer)))
    (merlin-send-command-async
     'errors
     (lambda (data)
       (let ((errors (mapcar
                      (lambda (alist)
                        (flycheck-ocaml-merlin-parse-error alist checker buffer))
                      data)))
         (funcall callback 'finished (delq nil errors))))
     ;; The error callback
     (lambda (msg) (funcall callback 'errored msg)))))

(flycheck-define-generic-checker 'ocaml-merlin
  "A syntax checker for OCaml using Merlin Mode.

See URL `https://github.com/the-lambda-church/merlin'."
  :start #'flycheck-ocaml-merlin-start
  :modes '(caml-mode tuareg-mode)
  :predicate (lambda () (and merlin-mode
                             ;; Don't check if Merlin's own checking is
                             ;; enabled, to avoid duplicate overlays
                             (not merlin-error-after-save)
                             ;; Only check saved buffers, like Merlin does,
                             ;; because Merlin freezes if the buffer contents
                             ;; are synced too frequently.
                             (flycheck-buffer-saved-p))))

;;;###autoload
(defun flycheck-ocaml-setup ()
  "Setup Flycheck OCaml.

Add `ocaml-merlin' to `flycheck-checkers'."
  (add-to-list 'flycheck-checkers 'ocaml-merlin))

(provide 'flycheck-ocaml)

;;; flycheck-ocaml.el ends here
