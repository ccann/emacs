#!/bin/bash

# Profile .emacs.d/init.el
emacs -Q -l ~/.emacs.d/profile-dotemacs.el \
      --eval "(setq profile-dotemacs-file \
              (setq load-file-name \"~/.emacs.d/init.el\"))" \
      -f profile-dotemacs
