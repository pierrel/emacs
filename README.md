Just add

```
(defvar emacs-root "~/path/to/this/src/dir/")
(load-file (concat emacs-root "config.el"))
```

to your ~/.emacs file

Or if you're using [spacemacs](https://www.spacemacs.org) just create a symbolic link to the local .spacemacs file in your home dir.
