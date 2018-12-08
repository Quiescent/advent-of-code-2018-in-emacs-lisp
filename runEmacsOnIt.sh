#!/bin/bash

if [ "$(uname)" == "Darwin" ]; then
    echo "emacs --batch --no-init -Q --eval \"(byte-compile-file \"$1\")\""
    /usr/local/Cellar/emacs-plus/HEAD-2cf9d9f/bin/emacs --batch --no-init -Q --eval "(byte-compile-file \"$1\")"

    echo "emacs --batch --no-init -Q --eval \"(setq run-from-batch t)\" -l \"$1c\""
    /usr/local/Cellar/emacs-plus/HEAD-2cf9d9f/bin/emacs --batch --no-init -Q --eval "(setq run-from-batch t)" -l "$1c"
else
    echo "emacs --batch --no-init -Q --eval \"(byte-compile-file \"$1\")\""
    emacs --batch --no-init -Q --eval "(byte-compile-file \"$1\")"

    echo "emacs --batch --no-init -Q --eval \"(setq run-from-batch t)\" -l \"$1c\""
    emacs --batch --no-init -Q --eval "(setq run-from-batch t)" -l "$1c"
fi

