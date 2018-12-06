#!/bin/bash

if [ -z "$1" ]; then
    echo "Please supply the day to continuously run..."
    exit 1
fi

while [ 1 == 1 ]; do
    echo "======================================"
    echo "Restarting watch loop due to errors..."
    echo "======================================"
    $(pwd)/runEmacsOnIt.sh $1
    echo "fswatch -0 \"$(pwd)/$1\" | xargs -0 -n1 -I '{}' $(pwd)/runEmacsOnIt.sh \"{}\""
    fswatch -0 "$(pwd)/$1" | xargs -0 -n1 -I '{}' $(pwd)/runEmacsOnIt.sh "{}"
done
