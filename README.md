# Elisp memoization functions

See the header in the source file for details. It's very easy to use:

```cl
(require 'memoize)

(memoize 'my-function)
```

The macro `memoize-defmemo` is also provided to directly create
memoized functions:

```cl
(memoize-defmemo my-expensive-function (x)
  (if (zerop n)
      1
    (* n (my-expensive-function (1- n)))))
```

When used frequently, it may be convenient to alias `memoize-defmemo`
to `defmemo`:

```cl
(defalias 'defmemo 'memoize-defmemo)
```
