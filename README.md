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
(use-package ledger-mode
    :mode "\\.ledger"
    :hook  ((ledger-mode . flycheck-mode)
            (ledger-mode . (lambda ()
                             (set (make-local-variable 'company-backends)
                                   (cons '(company-ledger-acct company-yankpad)
                                          company-backends)))))
    :custom (company-ledger-acct-master-file "~/accounts.dat")
            (company-ledger-acct-currency-symbol "₹"))
```

## Acknowledgments
[Company-ledger](https://github.com/debanjum/company-ledger)
This package is a modified version of company-ledger.
