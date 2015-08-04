auto-project.el
===============

Emacs hook to load modes based on git projects

Prerequisites
-------------

* dash (https://github.com/magnars/dash.el)
* project-root (http://solovyov.net/project-root/)

Usage
-----

1. Download the source and put it somewhere, like ~/emacs/auto-project.el/auto-project.el
2. Load it `(load-file "~/emacs/auto-project.el/auto-project.el")`

Now, whenever you open a file that has a `.emacs-config.el` in the top level of the project it will be run once.
