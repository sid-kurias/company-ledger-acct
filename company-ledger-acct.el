;;; company-ledger-acct.el --- Transaction Auto-Completion for account names

;; Copyright (C) 2018 Sidart Kurias

;; Author: Sidart Kurias
;; Description: Account name auto-completion for ledger
;; Version: 0.1.0
;; Package-Requires: ((company "0.8.0"))
;; URL: https://github.com/sid-kurias/company-ledger-acct

;; This file is NOT part of GNU Emacs.

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Provides auto completion for account names, based on string entered.
;; The string entered triggers a completion as long as it matches any
;; part of the account name. Once a candidate is selected the account name
;; is inserted and a currency symbol (if provided) is appended.
;; The currency symbol is defined by company-ledger-acct-currency-symbol.
;; This requires that a set of accounts are pre configured using the
;; /accounts/ keyword, and the file where these definitions are
;; available is defined in the variable company-ledger-acct-master-file.
;; A possible configuration using use-package :
;; (use-package ledger-mode
;;    :mode "\\.ledger"
;;    :hook  ((ledger-mode . flycheck-mode)
;;            (ledger-mode . (lambda ()
;;                             (set (make-local-variable 'company-backends)
;;                                   (cons '(company-ledger-acct company-yankpad)
;;                                          company-backends)))))
;;    :custom (company-ledger-acct-master-file "~/accounts.dat")
;;            (company-ledger-acct-currency-symbol "₹"))
;;
;;; Acknowledgments
;; URL: https://github.com/debanjum/company-ledger.
;; This code is a modified version of company-ledger.
;;; Code:

(require 'cl-lib)
(require 'company)

(defcustom company-ledger-acct-currency-symbol ""
  "Currency symbol to be appended to the end of the account name."
  :type 'string
  :group 'company-ledger-acct)

(defcustom company-ledger-acct-master-file "~/.ledgerrc"
  "Location of the file containing account definitions."
  :type 'file
  :group 'company-ledger-acct)

(defun company-ledger-acct--regexp-filter (regexp list)
  "Use REGEXP to filter LIST of strings."
  (let (new)
    (dolist (string list)
      (when (string-match regexp string)
        (setq new (cons (match-string 1  string) new))))
    new))


(defun company-ledger-acct--get-all-accts ()
  "Read the master file that has the account descriptions."
  (company-ledger-acct--regexp-filter
   "^account \\(.*\\)"
   (mapcar #'(lambda (s) (identity s))
           (with-temp-buffer
             (insert-file-contents company-ledger-acct-master-file)
             (split-string (buffer-string) "\n" t)))))

(defun company-ledger-acct--fuzzy-word-match (prefix candidate)
  "Return non-nil if each (partial) word in PREFIX is also in CANDIDATE."
  (eq nil
      (memq nil
            (mapcar
             #'(lambda (pre) (string-match-p (regexp-quote pre) candidate))
             (split-string prefix)))))

;;;###autoload
(defun company-ledger-acct (command &optional arg &rest ignored)
  "Company backend to prompt for account names."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-ledger-acct))
    (prefix (company-grab-symbol))
    (candidates
     (cl-remove-if-not
      (lambda (c) (company-ledger-acct--fuzzy-word-match arg c))
      (company-ledger-acct--get-all-accts)))
    (post-completion
     (progn  (delete-region(+ (line-beginning-position) (current-indentation)) (point))
             (insert(concat arg "      " company-ledger-acct-currency-symbol))))
    (sorted t)))

(provide 'company-ledger-acct)
;;; company-ledger-acct.el ends here
