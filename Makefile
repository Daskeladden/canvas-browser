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

.PHONY: test live clean

test:
	ERT_MATCH='$(MATCH)' $(EMACS) -Q --batch --eval "(setq load-prefer-newer t)" $(LOAD) \
	  $(addprefix -l ,$(SUITES)) \
	  --eval '(ert-run-tests-batch-and-exit (getenv "ERT_MATCH"))'

# Needs chromium, and opens a page from tests/fixtures.
live:
	$(EMACS) -Q --batch --eval "(setq load-prefer-newer t)" $(LOAD) -l tests/canvas-browser-live-tests.el \
	  --eval '(ert-run-tests-batch-and-exit)'

clean:
	rm -f *.elc tests/*.elc
