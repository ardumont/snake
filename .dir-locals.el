((haskell-mode . ((haskell-process-wrapper-function . (lambda (argv) (append (list "nix-shell" "shell.nix" "--command")
                                                                        (list (mapconcat 'identity argv " ")))))
                  (haskell-process-type . cabal-repl))))
