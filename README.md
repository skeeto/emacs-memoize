# Elisp memoization functions

See the header in the source file for details. It's very easy to use:

```cl
(require 'memoize)

(memoize 'my-function)
```

You can also use ```memoize-defmemo``` to create a new memoized
function:

```cl
(require 'memoize)

(memoize-defmemo my-expensive-function (x)
   (if (zerop n)
       1
     (* n (my-expensive-function (1- n)))))
```

You may want to ```defalias``` ```memoize-defmemo``` to ```defmemo```:

```cl
(require 'memoize)

(defalias 'defmemo 'memoize-defmemo)
```
