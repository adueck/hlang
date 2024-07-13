# hlang

A little exercise in creating a language in Haskell

```
(let
  ((x (+ 1 1))
   (y (+ x 2))
   (myF (lambda m (+ 1 (* m 2)))))
  (myF y))
```

Evaluates to `12`

To run the REPL:

```
$ runghc Main.hs
```

To evaluate a file:

```
$ runghc Main.hs example.hp
```