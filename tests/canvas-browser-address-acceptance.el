;;; canvas-browser-address-acceptance.el --- the address in the header line -*- lexical-binding: t -*-

;; `make address-acceptance' runs these in an Emacs on a virtual
;; display.  Each test shows a page buffer of the page Coffee at
;; https://example.org/coffee in the selected window, and uses it as a
;; user does: a click on a pixel of the header line, keys pressed in
;; the page, keys typed into the prompt.  The command loop of Emacs
;; reads each of these and finds its command.  No chromium runs: the
;; functions that talk to it are replaced, and each address that the
;; page is sent to is kept.
(require 'ert)
(require 'cl-lib)
(require 'canvas-browser)

;; The pages that a test opens are no visits of the reader.  The history
;; stays in the session of the test, and the file of the reader is left
;; alone.
(setq canvas-browser-history-file nil)

(defconst canvas-browser-address-acceptance--title "Coffee"
  "The title of the page of each test.")

(defconst canvas-browser-address-acceptance--address "https://example.org/coffee"
  "The address of the page of each test.")

(defvar canvas-browser-address-acceptance-patience 2
  "The most seconds that the events of one step may take.
A prompt that is still open then is left.")

(defvar canvas-browser-address-acceptance--visits nil
  "Each address that the page was sent to, newest first.")

(defvar canvas-browser-address-acceptance--seen nil
  "What the prompt showed, newest first, as (PROMPT . CONTENTS): when it
opened, and after each command in it.")

;;;; Chromium

(defun canvas-browser-address-acceptance--send (method params &optional answer &rest _)
  "Take METHOD with PARAMS in the place of chromium, and answer as a
chromium does that opened a page.  The address of each `Page.navigate'
is kept."
  (when (equal method "Page.navigate")
    (push (plist-get params :url) canvas-browser-address-acceptance--visits))
  (when answer
    (funcall answer '(:targetId "T1" :sessionId "S1"))))

(advice-add 'canvas-browser-cdp-start :override #'ignore)
(advice-add 'canvas-browser-cdp-running-p :override (lambda () t))
(advice-add 'canvas-browser-cdp-listen :override #'ignore)
(advice-add 'canvas-browser-cdp-forget :override #'ignore)
(advice-add 'canvas-browser-cdp-send :override #'canvas-browser-address-acceptance--send)

;;;; The page

(defun canvas-browser-address-acceptance--show-page ()
  "Show a new page buffer of the page Coffee in the selected window, with
its title and its address in the header line; the buffer."
  (delete-other-windows)
  (switch-to-buffer (generate-new-buffer "coffee"))
  (canvas-browser-mode)
  ;; The mode gives the window a header line.  The body has its size
  ;; only once Emacs drew that line.
  (redisplay t)
  (canvas-browser--open canvas-browser-address-acceptance--address
                        (window-body-width nil t) (window-body-height nil t))
  (setq canvas-browser--title canvas-browser-address-acceptance--title
        canvas-browser--url canvas-browser-address-acceptance--address)
  (force-mode-line-update)
  (redisplay t)
  (current-buffer))

(defmacro canvas-browser-address-acceptance--with-page (&rest body)
  "Run BODY with the page Coffee shown in the selected window, and kill
its buffer after.  The addresses that the page is sent to and what the
prompt shows are kept from the start of BODY on."
  (declare (indent 0))
  (let ((page (make-symbol "page")))
    `(let ((,page (canvas-browser-address-acceptance--show-page))
           (canvas-browser-address-acceptance--visits nil)
           (canvas-browser-address-acceptance--seen nil))
       (unwind-protect (progn ,@body)
         (kill-buffer ,page)))))

(defmacro canvas-browser-address-acceptance--with-visited-pages (&rest body)
  "Run BODY with the page Coffee shown in the selected window, after
visits to Milk, then Sugar, then Coffee, and kill its buffer after, as
`canvas-browser-address-acceptance--with-page' does."
  (declare (indent 0))
  ;; No visit goes to tea: another test types https://example.org/tea
  ;; into the prompt, and `minibuffer-history' keeps it, so M-p could
  ;; bring tea without any history of visits.  Each test gets a list of
  ;; its own, so that a change the browser makes to it ends with the test.
  `(let ((canvas-browser--visited
          (copy-tree '(("https://example.org/coffee" "Coffee" 3.0)
                       ("https://example.org/sugar" "Sugar" 2.0)
                       ("https://example.org/milk" "Milk" 1.0)))))
     (canvas-browser-address-acceptance--with-page ,@body)))

(defun canvas-browser-address-acceptance--visited ()
  "The addresses that the page was sent to, oldest first."
  (reverse canvas-browser-address-acceptance--visits))

;;;; The header line

(defun canvas-browser-address-acceptance--posn (x)
  "The position of the pixel X of the header line of the selected window,
counted from the left edge of the window, in the middle of the line."
  (posn-at-x-y x (+ (window-tab-line-height) (/ (window-header-line-height) 2))
               (selected-window) t))

(defun canvas-browser-address-acceptance--inside-p (posn text)
  "Whether POSN is a place of the header line of the selected window that
shows a character of TEXT."
  (pcase (posn-string posn)
    (`(,string . ,place)
     (let ((start (string-search text string)))
       (and (eq (posn-area posn) 'header-line)
            (eq (posn-window posn) (selected-window))
            start
            (<= start place)
            (< place (+ start (length text))))))))

(defun canvas-browser-address-acceptance--place-of (text)
  "The position of the leftmost pixel of the header line that shows TEXT."
  (or (cl-loop for x from 0 below (window-pixel-width)
               for posn = (canvas-browser-address-acceptance--posn x)
               when (canvas-browser-address-acceptance--inside-p posn text)
               return posn)
      (error "No pixel of the header line %S shows %S"
             (format-mode-line header-line-format) text)))

(defun canvas-browser-address-acceptance--click-on (text)
  "The event of a click of the left button on TEXT in the header line."
  ;; Emacs gives a button its kind when the X server sends the first
  ;; click of it, and no click came here.  Without the kind the command
  ;; loop takes the event for a key: it looks up `mouse-1' alone, not
  ;; `header-line mouse-1', and so misses the keymap of the header line.
  ;; xt-mouse.el sets the kind in the same way for the clicks it makes.
  (put 'mouse-1 'event-kind 'mouse-click)
  (list 'mouse-1 (canvas-browser-address-acceptance--place-of text)))

(defun canvas-browser-address-acceptance--looks (text)
  "How the header line shows TEXT under the pointer, as a plist:
whether it lights up, and its hint."
  (pcase-let ((`(,string . ,place)
               (posn-string (canvas-browser-address-acceptance--place-of text))))
    (list :lights-up (and (get-text-property place 'mouse-face string) t)
          :hint (get-text-property place 'help-echo string))))

;;;; The command loop and the prompt

(defun canvas-browser-address-acceptance--keys (keys)
  "The events of KEYS, a key description as `kbd' reads it."
  (listify-key-sequence (kbd keys)))

(defun canvas-browser-address-acceptance--note-prompt ()
  "Keep what the prompt shows now."
  (push (cons (minibuffer-prompt) (minibuffer-contents-no-properties))
        canvas-browser-address-acceptance--seen))

(defun canvas-browser-address-acceptance--prompt-opened (typed)
  "Keep what the prompt that just opened shows, now and after each
command in it, and type TYPED into it, a key description."
  (canvas-browser-address-acceptance--note-prompt)
  (add-hook 'post-command-hook #'canvas-browser-address-acceptance--note-prompt nil t)
  (setq unread-command-events
        (append unread-command-events (canvas-browser-address-acceptance--keys typed))))

(defun canvas-browser-address-acceptance--leave-when-done ()
  "Leave the command loop of a step once it read every event, unless a
prompt is still open."
  (when (and (null unread-command-events) (zerop (minibuffer-depth)))
    (exit-recursive-edit)))

(defun canvas-browser-address-acceptance--act (events &optional typed)
  "Have the command loop of Emacs read EVENTS and run their commands, as
the user's own.  TYPED, a key description, is typed into the prompt that
opens, and only when one opens.
After `canvas-browser-address-acceptance-patience' seconds a prompt that
is still open is left.  The way out of it is a throw, which leaves no
quit behind for the test."
  (minibuffer-with-setup-hook
      (:append (lambda () (canvas-browser-address-acceptance--prompt-opened (or typed ""))))
    (with-timeout (canvas-browser-address-acceptance-patience)
      (let ((timer (run-with-timer 0.1 0.1 #'canvas-browser-address-acceptance--leave-when-done)))
        (unwind-protect
            (progn (setq unread-command-events events)
                   (recursive-edit))
          (cancel-timer timer)
          (setq unread-command-events nil))))))

(defun canvas-browser-address-acceptance--opened ()
  "What the prompt showed when it opened, as (PROMPT . CONTENTS), or nil
if no prompt opened."
  (car (last canvas-browser-address-acceptance--seen)))

(defun canvas-browser-address-acceptance--last-seen ()
  "What the prompt showed last, as (PROMPT . CONTENTS), or nil if no
prompt opened."
  (car canvas-browser-address-acceptance--seen))

(defun canvas-browser-address-acceptance--held ()
  "What the prompt held when it opened and after each command in it,
oldest first, as a list of strings."
  ;; The command loop of the prompt runs `post-command-hook' once as it
  ;; starts, before it reads a key, so the second note stands for no
  ;; command and repeats the first.
  (pcase-exhaustive (reverse (mapcar #'cdr canvas-browser-address-acceptance--seen))
    ('nil nil)
    (`(,opened ,start . ,after)
     (cl-assert (equal start opened) nil "The prompt changed before any key: %S" start)
     (cons opened after))))

;;;; The scenarios

(ert-deftest canvas-browser-address-acceptance-a-click-on-the-address-asks-for-it ()
  ;; GIVEN a page buffer that shows the page https://example.org/coffee
  (canvas-browser-address-acceptance--with-page
    ;; WHEN I click on the address in the header line
    (canvas-browser-address-acceptance--act
     (list (canvas-browser-address-acceptance--click-on "https://example.org/coffee")))
    ;; THEN the minibuffer asks URL: and holds https://example.org/coffee
    (should (equal (canvas-browser-address-acceptance--opened)
                   '("URL: " . "https://example.org/coffee")))))

(ert-deftest canvas-browser-address-acceptance-an-edited-address-is-opened ()
  (canvas-browser-address-acceptance--with-page
    ;; GIVEN the prompt that a click on the address opened
    ;; WHEN I press backspace six times, type tea and press RET
    (canvas-browser-address-acceptance--act
     (list (canvas-browser-address-acceptance--click-on "https://example.org/coffee"))
     (concat (string-join (make-list 6 "<backspace>") " ") " tea RET"))
    ;; THEN the page goes to https://example.org/tea
    (should (equal (canvas-browser-address-acceptance--visited)
                   '("https://example.org/tea")))))

(ert-deftest canvas-browser-address-acceptance-m-n-brings-the-address ()
  ;; GIVEN a page buffer
  (canvas-browser-address-acceptance--with-page
    ;; WHEN I press o, AND WHEN I then press M-n in the prompt
    (canvas-browser-address-acceptance--act (canvas-browser-address-acceptance--keys "o") "M-n")
    ;; THEN the prompt is empty when o opens it
    (should (equal (canvas-browser-address-acceptance--opened) '("URL: " . "")))
    ;; AND after M-n the prompt holds https://example.org/coffee
    (should (equal (canvas-browser-address-acceptance--last-seen)
                   '("URL: " . "https://example.org/coffee")))))

(ert-deftest canvas-browser-address-acceptance-one-word-is-searched-for ()
  ;; GIVEN a page buffer
  (canvas-browser-address-acceptance--with-page
    ;; WHEN I press o, type the one word espresso and press RET
    (canvas-browser-address-acceptance--act (canvas-browser-address-acceptance--keys "o")
                                            "espresso RET")
    ;; THEN the page goes to the search for espresso
    (should (equal (canvas-browser-address-acceptance--visited)
                   '("https://duckduckgo.com/?q=espresso")))))

(ert-deftest canvas-browser-address-acceptance-the-address-lights-up-under-the-pointer ()
  ;; GIVEN a page buffer
  (canvas-browser-address-acceptance--with-page
    ;; WHEN the pointer is over the address in the header line
    ;; THEN the address lights up there and its hint is
    ;;      "mouse-1: edit the address"
    (should (equal (canvas-browser-address-acceptance--looks "https://example.org/coffee")
                   '(:lights-up t :hint "mouse-1: edit the address")))
    ;; AND the title has neither
    (should (equal (canvas-browser-address-acceptance--looks "Coffee")
                   '(:lights-up nil :hint nil)))))

(ert-deftest canvas-browser-address-acceptance-m-p-brings-the-page-before ()
  ;; GIVEN a page buffer that shows https://example.org/coffee, after
  ;;       visits to Milk, then Sugar, then Coffee
  (canvas-browser-address-acceptance--with-visited-pages
    ;; WHEN I press o and then M-p in the prompt
    (canvas-browser-address-acceptance--act (canvas-browser-address-acceptance--keys "o") "M-p")
    ;; THEN the prompt holds https://example.org/sugar
    (should (equal (canvas-browser-address-acceptance--last-seen)
                   '("URL: " . "https://example.org/sugar")))))

(ert-deftest canvas-browser-address-acceptance-m-p-and-m-n-step-through-the-visits ()
  ;; GIVEN a page buffer that shows https://example.org/coffee, after
  ;;       visits to Milk, then Sugar, then Coffee
  (canvas-browser-address-acceptance--with-visited-pages
    ;; WHEN I press o, and then M-p, M-p and M-n in the prompt
    (canvas-browser-address-acceptance--act (canvas-browser-address-acceptance--keys "o")
                                            "M-p M-p M-n")
    ;; THEN the prompt held, in this order: nothing,
    ;;      https://example.org/sugar, https://example.org/milk,
    ;;      https://example.org/sugar
    (should (equal (canvas-browser-address-acceptance--held)
                   '("" "https://example.org/sugar" "https://example.org/milk"
                     "https://example.org/sugar")))))

(ert-deftest canvas-browser-address-acceptance-a-visit-brought-by-m-p-is-opened ()
  ;; GIVEN a page buffer that shows https://example.org/coffee, after
  ;;       visits to Milk, then Sugar, then Coffee
  (canvas-browser-address-acceptance--with-visited-pages
    ;; WHEN I press o, then M-p and RET in the prompt
    (canvas-browser-address-acceptance--act (canvas-browser-address-acceptance--keys "o")
                                            "M-p RET")
    ;; THEN the page goes to https://example.org/sugar
    (should (equal (canvas-browser-address-acceptance--visited)
                   '("https://example.org/sugar")))))

(ert-deftest canvas-browser-address-acceptance-m-p-after-a-click-brings-the-page-before ()
  ;; GIVEN a page buffer that shows https://example.org/coffee, after
  ;;       visits to Milk, then Sugar, then Coffee
  (canvas-browser-address-acceptance--with-visited-pages
    ;; WHEN I click on the address in the header line, and then press
    ;;      M-p in the prompt
    (canvas-browser-address-acceptance--act
     (list (canvas-browser-address-acceptance--click-on "https://example.org/coffee"))
     "M-p")
    ;; THEN the prompt holds https://example.org/coffee when it opens
    (should (equal (canvas-browser-address-acceptance--opened)
                   '("URL: " . "https://example.org/coffee")))
    ;; AND after M-p it holds https://example.org/sugar
    (should (equal (canvas-browser-address-acceptance--last-seen)
                   '("URL: " . "https://example.org/sugar")))))

(ert-deftest canvas-browser-address-acceptance-m-p-in-the-prompt-of-O-brings-the-page-before ()
  ;; GIVEN a page buffer that shows https://example.org/coffee, after
  ;;       visits to Milk, then Sugar, then Coffee
  (canvas-browser-address-acceptance--with-visited-pages
    ;; WHEN I press O and then M-p in the prompt
    (canvas-browser-address-acceptance--act (canvas-browser-address-acceptance--keys "O") "M-p")
    ;; THEN the prompt holds https://example.org/sugar
    (should (equal (canvas-browser-address-acceptance--last-seen)
                   '("URL: " . "https://example.org/sugar")))))

(ert-deftest canvas-browser-address-acceptance-m-n-in-the-prompt-of-O-brings-the-address ()
  ;; GIVEN a page buffer that shows https://example.org/coffee, after
  ;;       visits to Milk, then Sugar, then Coffee
  (canvas-browser-address-acceptance--with-visited-pages
    ;; WHEN I press O and then M-n in the prompt
    (canvas-browser-address-acceptance--act (canvas-browser-address-acceptance--keys "O") "M-n")
    ;; THEN the prompt is empty when O opens it
    (should (equal (canvas-browser-address-acceptance--opened) '("URL: " . "")))
    ;; AND after M-n it holds https://example.org/coffee
    (should (equal (canvas-browser-address-acceptance--last-seen)
                   '("URL: " . "https://example.org/coffee")))))

;;;; Running them

(defun canvas-browser-address-acceptance--leave (code)
  "Show what Emacs said on the standard error stream, and leave Emacs
with the exit CODE."
  (princ (with-current-buffer (messages-buffer) (buffer-string)) #'external-debugging-output)
  (kill-emacs code))

(defun canvas-browser-address-acceptance-run ()
  "Run these tests, show the result on the standard error stream and
leave Emacs."
  (let ((stats (ert-run-tests-batch "^canvas-browser-address-acceptance-")))
    (canvas-browser-address-acceptance--leave
     (if (zerop (ert-stats-completed-unexpected stats)) 0 1))))

;; Whatever goes wrong, this Emacs ends and says what it did so far: a
;; step that waits for ever would hold make until its timeout.
(run-with-timer 120 nil #'canvas-browser-address-acceptance--leave 3)
