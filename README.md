ob-sml
====

Put `ob-sml.el` where it can be loaded by Emacs. For example I place
it in a directory `~/.emacs.d/vendor` which is automatically put on
the load path with the following:

```
(let ((default-directory  "~/.emacs.d/vendor/"))
  (normal-top-level-add-subdirs-to-load-path))
```

Then require it and make sure `org-babel` knows about it like so:

```elisp
(require 'ob-sml nil 'noerror)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((clojure . t)
   (ruby . t)
   (scheme . t)
   (js . t)
   (haskell . t)
   (prolog . t)
   (sml . t) ;; added!
   ))
```

To test, create a new `org-mode` file such as `sml.org`. Then add the
following to this file:

```
ML for the Working Programmer

* Chapter 1
#+BEGIN_SRC sml :session *sml*
  fn n => n + 1
#+END_SRC
```

Start a `sml` REPL with `M-x run-sml`. You should now be able to place
your cursor in the code block and evaluate with `C-c C-c` and the
contents of your `sml.org` file should now look like:

```
* Chapter 1
#+BEGIN_SRC sml :session *sml*
  fn n => n + 1
#+END_SRC

#+RESULTS:
: val it = fn : int -> int
```

