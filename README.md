# company-ledger-acct
Account name completions for ledger-mode  

Provides auto completion for account names, based on string entered.
The string entered triggers a completion as long as it matches any
part of an account name. Once a candidate is selected the account name
is inserted and a currency symbol (if provided) is appended.  

The currency symbol is defined by company-ledger-acct-currency-symbol.  

This package requires that a set of accounts are pre configured using the
accounts keyword, and the file where these definitions are
available is defined in the variable company-ledger-acct-master-file.

## Setup
A possible configuration using use-package :

``` emacs-lisp
(use-package company-ledger-acct
      :straight (company-ledger-acct :type git :host github :repo "sid-kurias/company-ledger-acct")
      :after (ledger-mode)
      :hook  ((ledger-mode . (lambda ()
                               (set (make-local-variable 'company-backends)
                                    (list '(company-ledger-acct company-yasnippet))))))
      :custom (company-ledger-acct-master-file "~/accounts.dat")
      (company-ledger-acct-currency-symbol "₹"))
```

## Acknowledgments
[Company-ledger](https://github.com/debanjum/company-ledger)
This package is a modified version of company-ledger.
