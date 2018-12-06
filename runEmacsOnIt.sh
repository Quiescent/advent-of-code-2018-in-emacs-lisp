#!/bin/bash

if [ "$(uname)" == "Darwin" ]; then
    echo "emacs --batch --eval \"(byte-compile-file \"$1\")\""
    /usr/local/Cellar/emacs-plus/HEAD-2cf9d9f/bin/emacs --batch --eval "(byte-compile-file \"$1\")"

    echo "emacs --batch -l \"$1c\""
    /usr/local/Cellar/emacs-plus/HEAD-2cf9d9f/bin/emacs --batch -l "$1c"
else
    echo "emacs --batch --eval \"(byte-compile-file \"$1\")\""
    emacs --batch --eval "(byte-compile-file \"$1\")"

    echo "emacs --batch -l \"$1c\""
    emacs --batch -l "$1c"
fi
