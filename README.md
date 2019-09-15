# Elisp memoization functions

See the header in the source file for details. It's very easy to use:

```cl
(require 'memoize)

(memoize 'my-function)
```

The macro `defmemoize` is also provided to directly create memoized
functions:

```cl
(defmemoize my-expensive-function (n)
  (if (zerop n)
      1
    (* n (my-expensive-function (1- n)))))
```

Some functions are run over buffer contents, and need to be cached
only so long as the buffer contents do not change. For these
use-cases, we have the function `memoize-by-buffer-contents` as well
as the `defmemoize-by-buffer-contents` macro.

To restore the original definition of a memoized function symbol (not
a lambda or closure), use `memoize-restore`:

```cl
(memoize 'my-function)
(memoize-restore 'my-function)
```
