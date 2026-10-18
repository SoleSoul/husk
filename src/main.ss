;;; main.ss -- command-line entry point
(import (chezscheme) (husk compiler))
(husk-main (command-line-arguments))
