# canvas-browser -- a web browser in an Emacs buffer, drawn on a canvas.

EMACS   ?= emacs
DIAGRAM ?= ../canvas-diagram
KEYS    ?= ../canvas-keys
ELPACA  ?= $(HOME)/.emacs.d/elpaca/builds
MATCH   ?=

# The cairo and pango module lives with canvas-diagram.
LOAD    = -L . -L $(DIAGRAM) -L $(KEYS) -L tests -L $(ELPACA)/websocket \
          -L $(ELPACA)/transient -L $(ELPACA)/cond-let $(ROAM) $(UNDO)
# org-roam and what it needs.  The test of its receiver is skipped
# where org-roam is not installed.
ROAM    = -L $(ELPACA)/org-roam -L $(ELPACA)/emacsql -L $(ELPACA)/magit-section \
          -L $(ELPACA)/dash -L $(ELPACA)/llama
# undo-tree and what it needs.  The test that it leaves a page buffer
# alone is skipped where undo-tree is not installed.
UNDO    = -L $(ELPACA)/undo-tree -L $(ELPACA)/queue
SUITES  = tests/canvas-browser-cdp-tests.el tests/canvas-browser-translate-tests.el \
          tests/canvas-browser-merge-tests.el tests/canvas-browser-tests.el

# The directories of the Lisp with whole paths, for a check that runs elsewhere.
GUI_LOAD = $(foreach d,. $(DIAGRAM) $(KEYS),-L $(abspath $(d))) \
           -L $(ELPACA)/websocket -L $(ELPACA)/transient -L $(ELPACA)/cond-let

.PHONY: test live gui address-acceptance draw-acceptance fit-acceptance clean

test:
	ERT_MATCH='$(MATCH)' $(EMACS) -Q --batch --eval "(setq load-prefer-newer t)" $(LOAD) \
	  $(addprefix -l ,$(SUITES)) \
	  --eval '(ert-run-tests-batch-and-exit (getenv "ERT_MATCH"))'

# Needs chromium, and opens a page from tests/fixtures.
live:
	$(EMACS) -Q --batch --eval "(setq load-prefer-newer t)" $(LOAD) -l tests/canvas-browser-live-tests.el \
	  --eval '(ert-run-tests-batch-and-exit)'

# Real turns of the wheel on a page buffer, beside a link and over one,
# on a virtual display.  No chromium runs: the check notes what a
# gesture sends to the page.  It is not part of test.
# -noreset: an X server starts anew when its last client leaves, and
# Emacs opens the display more than once as it starts.  Without it the
# server now and then refuses the Emacs of this check.
gui:
	EMACS='$(EMACS)' GUI_LOAD='$(GUI_LOAD)' \
	  xvfb-run -a -s "-screen 0 1400x900x24 -noreset" tests/gui/run.sh

# A click on the address in the header line, and the address in the
# prompt of o, with real events in a frame on a virtual display.  No
# chromium runs.  It is not part of test.  -noreset as for gui.
# The tests of the list below the prompt need vertico and orderless,
# and are skipped where they are not installed.  Both ask for compat,
# which Emacs 30 and later bring with them.
address-acceptance:
	timeout 300 xvfb-run -a -s "-screen 0 1400x900x24 -noreset" $(EMACS) -Q \
	  --eval '(setq load-prefer-newer t)' $(GUI_LOAD) \
	  -L $(ELPACA)/vertico -L $(ELPACA)/orderless \
	  -l tests/canvas-browser-address-acceptance.el \
	  --eval '(canvas-browser-address-acceptance-run)'

# Real moves of the mouse with a button held, on a PDF in the Draw mode
# of chromium and on a page of text, in a frame on a virtual display.
# A real chromium runs, on its own display.  It is not part of test.
# -noreset as for gui.
draw-acceptance:
	timeout 450 xvfb-run -a -s "-screen 0 1400x900x24 -noreset" $(EMACS) -Q \
	  --eval '(setq load-prefer-newer t)' $(GUI_LOAD) -L $(abspath tests) \
	  -l tests/canvas-browser-draw-acceptance.el \
	  --eval '(canvas-browser-draw-acceptance-run)'

# Keys that fit a whole page of a PDF in the window, and go a page at a
# time, in a frame on a virtual display.  A real chromium runs, on its
# own display.  It is not part of test.  -noreset as for gui.
fit-acceptance:
	timeout 450 xvfb-run -a -s "-screen 0 1400x900x24 -noreset" $(EMACS) -Q \
	  --eval '(setq load-prefer-newer t)' $(GUI_LOAD) -L $(abspath tests) \
	  -l tests/canvas-browser-fit-acceptance.el \
	  --eval '(canvas-browser-fit-acceptance-run)'

clean:
	rm -f *.elc tests/*.elc
