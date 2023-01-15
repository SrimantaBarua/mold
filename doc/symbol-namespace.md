## Namespace/environment for evaluating a symbol

```text
;; in module "mod"
(set! foo 'bar)
(other-func foo)

;; in module "other"
(define func (lambda (sym)
   (let ((bar 1))
      (eval sym))))     ;; sym = 'bar

;; (func 'bar)
```

Wherever you call `eval`, that defines your "lexical" environment - all symbols are looked up at
that location. However, symbols can't refer to "local" variables (i.e. via `let`). They can only
refer to symbols that have been mapped with `define`.
