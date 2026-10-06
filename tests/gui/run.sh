#!/bin/sh
# Show a page buffer in a real frame, send it real events, and compare
# what each gesture sent to the page with `expected'.  Run it under
# xvfb-run, as `make gui' does: GUI_LOAD holds the -L options of the
# Makefile, with whole paths.  A run that fails leaves the file `result'
# to read.
cd "$(dirname "$0")" || exit 2
rm -f ready done result
"${EMACS:-emacs}" -Q $GUI_LOAD -l gui-browser.el &
python3 drive.py
wait
if diff -u expected result; then
    rm -f ready done result
    echo "gui: the gestures reach the page"
else
    exit 1
fi
