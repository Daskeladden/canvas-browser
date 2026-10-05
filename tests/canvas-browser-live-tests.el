;;; canvas-browser-live-tests.el --- one test with a real chromium -*- lexical-binding: t -*-
(require 'ert)
(require 'cl-lib)
(require 'canvas-browser)

;; The pages that a test opens are no visits of the reader.  The history
;; stays in the session of the test, and the file of the reader is left
;; alone.
(setq canvas-browser-history-file nil)

(defun canvas-browser-live-test--wait (seconds test)
  "Wait up to SECONDS until TEST answers; whether it did."
  (let ((deadline (+ (float-time) seconds)))
    (while (and (not (funcall test)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall test)))

(ert-deftest canvas-browser-live-opens-a-page-and-reads-its-text ()
  ;; GIVEN chromium on this machine and the fixture page
  ;; WHEN the page is opened and its text is asked for
  ;; THEN the page has a session, AND its text holds the words of the page
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  ;; A profile of its own: a chromium that already uses the profile of
  ;; your Emacs would take this one's work and leave it without a port.
  (let ((buffer nil)
        (canvas-browser-profile-directory
         (let ((temporary-file-directory (expand-file-name "~/snap/chromium/common/")))
           (make-temp-file "canvas-browser-live-" t))))
    (unwind-protect
        (let ((file (expand-file-name "tests/fixtures/page.html")))
          (setq buffer (canvas-browser (concat "file://" file)))
          (with-current-buffer buffer
            (should (canvas-browser-live-test--wait 30 (lambda () canvas-browser--session)))
            (should (canvas-browser-live-test--wait 30 (lambda () (> canvas-browser--frames 0))))
            (canvas-browser-text)
            (should (canvas-browser-live-test--wait
                     30 (lambda ()
                          (let ((text (get-buffer (format "*canvas-browser-text: %s*"
                                                          (or canvas-browser--title
                                                              canvas-browser--url)))))
                            (and text (> (buffer-size text) 0))))))
            (with-current-buffer (canvas-browser--text-buffer)
              (should (string-search "the live test" (buffer-string))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (canvas-browser-cdp-stop)
      (delete-directory canvas-browser-profile-directory t))))

(defun canvas-browser-live-test--middle (id)
  "The middle of the element ID of this page, as (X . Y), or nil."
  (let ((answer 'waiting))
    (canvas-browser--evaluate
     (format "(() => { const r = document.getElementById('%s').getBoundingClientRect();
                       return [Math.round(r.x + r.width / 2), Math.round(r.y + r.height / 2)]; })()"
             id)
     (lambda (value) (setq answer value)))
    (canvas-browser-live-test--wait 10 (lambda () (not (eq answer 'waiting))))
    (and (consp answer) (cons (car answer) (cadr answer)))))

(defun canvas-browser-live-test--click-and-see (id)
  "Click the element ID, and whether the keys then go to the page."
  (let ((middle (canvas-browser-live-test--middle id)))
    (should middle)
    (canvas-browser--click (car middle) (cdr middle))
    ;; The page answers the question about its focus a moment later.
    (canvas-browser-live-test--wait 2 (lambda () nil))
    canvas-browser--insert))

(ert-deftest canvas-browser-live-a-click-in-a-field-sends-the-keys-to-the-page ()
  ;; GIVEN the typing fixture: a plain field, a field in the shadow root of
  ;;       a web component, as Reddit has it, and a button
  ;; WHEN each is clicked in turn
  ;; THEN a click in either field sends the keys to the page, AND a click
  ;;      on the button gives them back to Emacs, where `d\=' is dark mode
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (let ((buffer nil)
        (canvas-browser-profile-directory
         (let ((temporary-file-directory (expand-file-name "~/snap/chromium/common/")))
           (make-temp-file "canvas-browser-live-" t))))
    (unwind-protect
        (let ((file (expand-file-name "tests/fixtures/typing.html")))
          (setq buffer (canvas-browser (concat "file://" file)))
          (with-current-buffer buffer
            (should (canvas-browser-live-test--wait 30 (lambda () canvas-browser--session)))
            (should (canvas-browser-live-test--wait 30 (lambda () (> canvas-browser--frames 0))))
            (should (canvas-browser-live-test--click-and-see "plain"))
            (should-not (canvas-browser-live-test--click-and-see "button"))
            (should (canvas-browser-live-test--click-and-see "shadowed"))
            (should-not (canvas-browser-live-test--click-and-see "button"))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (canvas-browser-cdp-stop)
      (delete-directory canvas-browser-profile-directory t))))

(defun canvas-browser-live-test--value (id)
  "The value of the field ID of this page."
  (let ((answer 'waiting))
    (canvas-browser--evaluate (format "document.getElementById('%s').value" id)
                              (lambda (value) (setq answer value)))
    (canvas-browser-live-test--wait 10 (lambda () (not (eq answer 'waiting))))
    answer))

(ert-deftest canvas-browser-live-the-editing-keys-edit-a-field ()
  ;; GIVEN the typing fixture, with the plain field clicked
  ;; WHEN abc is typed, backspace is pressed, and then Home and x
  ;; THEN the field holds xab: backspace deleted, and Home moved
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (let ((buffer nil)
        (canvas-browser-profile-directory
         (let ((temporary-file-directory (expand-file-name "~/snap/chromium/common/")))
           (make-temp-file "canvas-browser-live-" t))))
    (unwind-protect
        (let ((file (expand-file-name "tests/fixtures/typing.html")))
          (setq buffer (canvas-browser (concat "file://" file)))
          (with-current-buffer buffer
            (should (canvas-browser-live-test--wait 30 (lambda () canvas-browser--session)))
            (should (canvas-browser-live-test--wait 30 (lambda () (> canvas-browser--frames 0))))
            (should (canvas-browser-live-test--click-and-see "plain"))
            (mapc #'canvas-browser--type-character "abc")
            (canvas-browser--key "Backspace")
            (canvas-browser--key "Home")
            (canvas-browser--type-character ?x)
            (should (canvas-browser-live-test--wait
                     5 (lambda () (equal (canvas-browser-live-test--value "plain") "xab"))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (canvas-browser-cdp-stop)
      (delete-directory canvas-browser-profile-directory t))))

(ert-deftest canvas-browser-live-the-hints-reach-into-web-components ()
  ;; GIVEN the typing fixture: a plain field, a field in the shadow root of
  ;;       a web component, and a button
  ;; WHEN the boxes that take a hint are asked for
  ;; THEN all three are there: `querySelectorAll\=' does not look into a
  ;;      shadow root, and a login field of Reddit sits in one
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (let ((buffer nil)
        (canvas-browser-profile-directory
         (let ((temporary-file-directory (expand-file-name "~/snap/chromium/common/")))
           (make-temp-file "canvas-browser-live-" t))))
    (unwind-protect
        (let ((file (expand-file-name "tests/fixtures/typing.html")))
          (setq buffer (canvas-browser (concat "file://" file)))
          (with-current-buffer buffer
            (should (canvas-browser-live-test--wait 30 (lambda () canvas-browser--session)))
            (should (canvas-browser-live-test--wait 30 (lambda () (> canvas-browser--frames 0))))
            (let ((boxes 'waiting)
                  (shadowed (canvas-browser-live-test--middle "shadowed")))
              (canvas-browser--boxes (lambda (found) (setq boxes found)))
              (should (canvas-browser-live-test--wait 10 (lambda () (listp boxes))))
              (should (= (length boxes) 4))
              (should (cl-some (lambda (box)
                                 (let ((middle (canvas-browser--box-middle box)))
                                   (< (abs (- (cdr middle) (cdr shadowed))) 10)))
                               boxes)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (canvas-browser-cdp-stop)
      (delete-directory canvas-browser-profile-directory t))))

(defun canvas-browser-live-test--box-at-p (boxes middle)
  "Whether one of BOXES holds MIDDLE, a point as (X . Y)."
  (cl-some (lambda (box)
             (and (<= (plist-get box :x) (car middle) (+ (plist-get box :x) (plist-get box :w)))
                  (<= (plist-get box :y) (cdr middle) (+ (plist-get box :y) (plist-get box :h)))))
           boxes))

(defun canvas-browser-live-test--lay-out (width height)
  "Lay this page out WIDTH by HEIGHT, and wait until the page says so.
A batch Emacs has no window worth the name, and a page of the least size
leaves most of a fixture outside it."
  (canvas-browser--window-resized width height)
  (should (canvas-browser-live-test--wait
           10 (lambda ()
                (let ((answer 'waiting))
                  (canvas-browser--evaluate "innerWidth" (lambda (value) (setq answer value)))
                  (canvas-browser-live-test--wait 5 (lambda () (not (eq answer 'waiting))))
                  (equal answer width))))))

(ert-deftest canvas-browser-live-the-hints-label-only-what-a-click-reaches ()
  ;; GIVEN the modal fixture: a link under a veil, a dialog over the veil
  ;;       with a field in a web component, a field under its label, a
  ;;       link around a button, and a button, and a link below the window
  ;; WHEN the boxes that take a hint are asked for
  ;; THEN the six things of the dialog each get one, a link whose text is
  ;;      put into a web component through a slot and a button that is a
  ;;      frame of its own among them, AND the link under the veil and the
  ;;      link below the window get none: a hint spent on those is a hint
  ;;      the dialog goes without
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (let ((buffer nil)
        (canvas-browser-profile-directory
         (let ((temporary-file-directory (expand-file-name "~/snap/chromium/common/")))
           (make-temp-file "canvas-browser-live-" t))))
    (unwind-protect
        (let ((file (expand-file-name "tests/fixtures/modal.html")))
          (setq buffer (canvas-browser (concat "file://" file)))
          (with-current-buffer buffer
            (should (canvas-browser-live-test--wait 30 (lambda () canvas-browser--session)))
            (should (canvas-browser-live-test--wait 30 (lambda () (> canvas-browser--frames 0))))
            (canvas-browser-live-test--lay-out 1024 768)
            (let ((boxes 'waiting))
              (canvas-browser--boxes (lambda (found) (setq boxes found)))
              (should (canvas-browser-live-test--wait 10 (lambda () (listp boxes))))
              (dolist (id '("shadowed" "labelled" "nested" "button" "slotted" "framed"))
                (should (canvas-browser-live-test--box-at-p
                         boxes (canvas-browser-live-test--middle id))))
              (should-not (canvas-browser-live-test--box-at-p
                           boxes (canvas-browser-live-test--middle "behind")))
              (should (= (length boxes) 6)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (canvas-browser-cdp-stop)
      (delete-directory canvas-browser-profile-directory t))))

(defun canvas-browser-live-test--focus ()
  "The id of what has the focus of this page, looked for in web components."
  (let ((answer 'waiting))
    (canvas-browser--evaluate
     "(() => { let e = document.activeElement;
               while (e && e.shadowRoot && e.shadowRoot.activeElement) e = e.shadowRoot.activeElement;
               return e ? e.id : ''; })()"
     (lambda (value) (setq answer value)))
    (canvas-browser-live-test--wait 10 (lambda () (not (eq answer 'waiting))))
    answer))

(ert-deftest canvas-browser-live-tab-goes-to-the-next-field ()
  ;; GIVEN the typing fixture, with the plain field clicked
  ;; WHEN tab is pressed, in insert state, and then in normal state again
  ;; THEN the focus goes to the field in the web component, and then on to
  ;;      the button
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (let ((buffer nil)
        (canvas-browser-profile-directory
         (let ((temporary-file-directory (expand-file-name "~/snap/chromium/common/")))
           (make-temp-file "canvas-browser-live-" t))))
    (unwind-protect
        (let ((file (expand-file-name "tests/fixtures/typing.html")))
          (setq buffer (canvas-browser (concat "file://" file)))
          (with-current-buffer buffer
            (should (canvas-browser-live-test--wait 30 (lambda () canvas-browser--session)))
            (should (canvas-browser-live-test--wait 30 (lambda () (> canvas-browser--frames 0))))
            (should (canvas-browser-live-test--click-and-see "plain"))
            (canvas-browser-next-field)
            (should (canvas-browser-live-test--wait
                     5 (lambda () (equal (canvas-browser-live-test--focus) "inner"))))
            ;; The page keeps the box of the field it moved to, in the
            ;; shadow root of its web component, to fly the eye there.
            (should (canvas-browser-live-test--wait
                     5 (lambda ()
                         (and canvas-browser--focus-box
                              (canvas-browser-live-test--box-at-p
                               (list canvas-browser--focus-box)
                               (canvas-browser-live-test--middle "shadowed"))))))
            ;; The button is not a field: the next tab passes it.
            (canvas-browser-next-field)
            (should (canvas-browser-live-test--wait
                     5 (lambda () (equal (canvas-browser-live-test--focus) "after"))))
            (canvas-browser-previous-field)
            (should (canvas-browser-live-test--wait
                     5 (lambda () (equal (canvas-browser-live-test--focus) "inner"))))
            ;; From normal state as well, and the keys then go to the field.
            (canvas-browser-normal-mode)
            (canvas-browser-next-field)
            (should (canvas-browser-live-test--wait
                     5 (lambda () (equal (canvas-browser-live-test--focus) "after"))))
            (should (canvas-browser-live-test--wait 5 (lambda () canvas-browser--insert)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (canvas-browser-cdp-stop)
      (delete-directory canvas-browser-profile-directory t))))

(ert-deftest canvas-browser-live-a-letter-goes-where-the-focus-is ()
  ;; GIVEN the typing fixture, with the plain field clicked, and the focus
  ;;       then moved to the button, which leaves the caret in the field
  ;; WHEN a letter is typed
  ;; THEN the field does not take it: a letter goes to what has the focus,
  ;;      as a key pressed in a browser does, and never to a caret left
  ;;      behind in a field the focus has moved on from
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (let ((buffer nil)
        (canvas-browser-profile-directory
         (let ((temporary-file-directory (expand-file-name "~/snap/chromium/common/")))
           (make-temp-file "canvas-browser-live-" t))))
    (unwind-protect
        (let ((file (expand-file-name "tests/fixtures/typing.html")))
          (setq buffer (canvas-browser (concat "file://" file)))
          (with-current-buffer buffer
            (should (canvas-browser-live-test--wait 30 (lambda () canvas-browser--session)))
            (should (canvas-browser-live-test--wait 30 (lambda () (> canvas-browser--frames 0))))
            (should (canvas-browser-live-test--click-and-see "plain"))
            (canvas-browser--evaluate "document.getElementById('button').focus()" #'ignore)
            (should (canvas-browser-live-test--wait
                     5 (lambda () (equal (canvas-browser-live-test--focus) "button"))))
            (canvas-browser--type-character ?q)
            (canvas-browser-live-test--wait 1 (lambda () nil))
            (should (equal (canvas-browser-live-test--value "plain") ""))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (canvas-browser-cdp-stop)
      (delete-directory canvas-browser-profile-directory t))))

(defun canvas-browser-live-test--other-page (buffer)
  "A page buffer other than BUFFER, or nil."
  (seq-find (lambda (other)
              (and (not (eq other buffer))
                   (eq (buffer-local-value 'major-mode other) 'canvas-browser-mode)))
            (buffer-list)))

(ert-deftest canvas-browser-live-a-window-a-page-opens-is-shown-and-closes ()
  ;; GIVEN the popup fixture, whose button opens a window
  ;; WHEN the button is clicked, and the window later closes itself
  ;; THEN the window is shown in a page buffer of its own, which paints,
  ;;      AND its buffer goes when the window closes
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (let ((buffer nil) (opened nil)
        (canvas-browser-profile-directory
         (let ((temporary-file-directory (expand-file-name "~/snap/chromium/common/")))
           (make-temp-file "canvas-browser-live-" t))))
    (unwind-protect
        (let ((file (expand-file-name "tests/fixtures/popup.html")))
          (setq buffer (canvas-browser (concat "file://" file)))
          (with-current-buffer buffer
            (should (canvas-browser-live-test--wait 30 (lambda () canvas-browser--session)))
            (should (canvas-browser-live-test--wait 30 (lambda () (> canvas-browser--frames 0))))
            (let ((middle (canvas-browser-live-test--middle "open")))
              (canvas-browser--click (car middle) (cdr middle))))
          (should (canvas-browser-live-test--wait
                   10 (lambda () (setq opened (canvas-browser-live-test--other-page buffer)))))
          (with-current-buffer opened
            (should (canvas-browser-live-test--wait 30 (lambda () canvas-browser--session)))
            (should (canvas-browser-live-test--wait 30 (lambda () (> canvas-browser--frames 0))))
            (canvas-browser--evaluate "window.close()" #'ignore))
          (should (canvas-browser-live-test--wait 10 (lambda () (not (buffer-live-p opened))))))
      (when (buffer-live-p opened) (kill-buffer opened))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (canvas-browser-cdp-stop)
      (delete-directory canvas-browser-profile-directory t))))

(defun canvas-browser-live-test--place-of (id script-function)
  "The place of the element ID among the things SCRIPT-FUNCTION labels."
  (let ((boxes 'waiting) (answer 'waiting))
    (funcall script-function (lambda (found) (setq boxes found)))
    (canvas-browser-live-test--wait 10 (lambda () (listp boxes)))
    (canvas-browser--evaluate
     (format "window.__canvasBrowserTargets.indexOf(document.getElementById('%s'))" id)
     (lambda (value) (setq answer value)))
    (canvas-browser-live-test--wait 10 (lambda () (not (eq answer 'waiting))))
    answer))

(defun canvas-browser-live-test--pixel (png x y)
  "The colour of PNG, a string of bytes, at X Y, as ARGB32."
  (let* ((file (make-temp-file "canvas-browser-live-" nil ".png"))
         (size (canvas-browser--picture-size png))
         (canvas (list 'image :type 'canvas :id (make-symbol "live")
                       :data-width (car size) :data-height (cdr size)))
         (context (canvas-cairo-context canvas)))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'binary)) (write-region png nil file nil 'silent))
          (canvas-cairo-image context file 0 0 (car size) (cdr size))
          (canvas-cairo-pixel context x y))
      (delete-file file)
      (canvas-cairo-destroy context))))

(ert-deftest canvas-browser-live-a-hint-copies-an-address-a-text-and-a-picture ()
  ;; GIVEN the copy fixture: a link, an article, and a red block far down,
  ;;       with the page scrolled to it
  ;; WHEN the address of the link, the text of the article, and the
  ;;      picture of the red block are copied
  ;; THEN the kill ring holds the address and then the text, AND the
  ;;      picture is the block's own size and red in the middle: the cut
  ;;      is made where the block is on the page, not in the window
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (let ((buffer nil) (kill-ring nil) (picture nil)
        (canvas-browser-profile-directory
         (let ((temporary-file-directory (expand-file-name "~/snap/chromium/common/")))
           (make-temp-file "canvas-browser-live-" t))))
    (unwind-protect
        (let ((file (expand-file-name "tests/fixtures/copy.html")))
          (setq buffer (canvas-browser (concat "file://" file)))
          (with-current-buffer buffer
            (should (canvas-browser-live-test--wait 30 (lambda () canvas-browser--session)))
            (should (canvas-browser-live-test--wait 30 (lambda () (> canvas-browser--frames 0))))
            (canvas-browser-live-test--lay-out 1024 768)
            (canvas-browser--copy-address
             (canvas-browser-live-test--place-of "link" #'canvas-browser--boxes) nil)
            (should (canvas-browser-live-test--wait
                     5 (lambda () (equal (car kill-ring) "https://example.org/target"))))
            (canvas-browser--copy-text
             (canvas-browser-live-test--place-of "story" #'canvas-browser--blocks) nil)
            (should (canvas-browser-live-test--wait
                     5 (lambda () (equal (car kill-ring) "The story text."))))
            (canvas-browser--evaluate "window.scrollTo(0, 1000)" #'ignore)
            (canvas-browser-live-test--wait 1 (lambda () nil))
            (cl-letf (((symbol-function 'canvas-keys-copy-png)
                       (lambda (bytes) (setq picture bytes))))
              (canvas-browser--copy-picture
               (canvas-browser-live-test--place-of "red" #'canvas-browser--blocks) nil)
              (should (canvas-browser-live-test--wait 10 (lambda () picture))))
            (should (equal (canvas-browser--picture-size picture) '(120 . 80)))
            (should (= (canvas-browser-live-test--pixel picture 60 40) #xFFFF0000))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (canvas-browser-cdp-stop)
      (delete-directory canvas-browser-profile-directory t))))

(ert-deftest canvas-browser-live-a-buffer-is-named-after-the-page-it-shows ()
  ;; GIVEN a page buffer opened on the fixture page
  ;; WHEN o sends it to the typing fixture
  ;; THEN the buffer takes the name of the new address and the title of
  ;;      the new page
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (let ((buffer nil)
        (canvas-browser-profile-directory
         (let ((temporary-file-directory (expand-file-name "~/snap/chromium/common/")))
           (make-temp-file "canvas-browser-live-" t))))
    (unwind-protect
        (let ((first (concat "file://" (expand-file-name "tests/fixtures/page.html")))
              (second (concat "file://" (expand-file-name "tests/fixtures/typing.html"))))
          (setq buffer (canvas-browser first))
          (with-current-buffer buffer
            (should (canvas-browser-live-test--wait 30 (lambda () canvas-browser--session)))
            (should (canvas-browser-live-test--wait 30 (lambda () (> canvas-browser--frames 0))))
            (canvas-browser-open-url second)
            (should (canvas-browser-live-test--wait
                     10 (lambda () (equal (buffer-name) (format "*canvas-browser: %s*" second)))))
            (should (canvas-browser-live-test--wait
                     10 (lambda () (equal canvas-browser--title "canvas-browser typing fixture"))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (canvas-browser-cdp-stop)
      (delete-directory canvas-browser-profile-directory t))))

(ert-deftest canvas-browser-live-the-hints-leave-what-only-looks-alone ()
  ;; GIVEN the noise fixture: a button with an icon in it, a div and a
  ;;       span that say they are a button and a tab, a small frame to sign
  ;;       in with; and a heading and a picture that say they only look, a
  ;;       tracker of two pixels, and an advertisement in a frame
  ;; WHEN the boxes that take a hint are asked for
  ;; THEN the four that do something take one each, AND the others take
  ;;      none: a label on an icon inside a button crowds the button
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (let ((buffer nil)
        (canvas-browser-profile-directory
         (let ((temporary-file-directory (expand-file-name "~/snap/chromium/common/")))
           (make-temp-file "canvas-browser-live-" t))))
    (unwind-protect
        (let ((file (expand-file-name "tests/fixtures/noise.html")))
          (setq buffer (canvas-browser (concat "file://" file)))
          (with-current-buffer buffer
            (should (canvas-browser-live-test--wait 30 (lambda () canvas-browser--session)))
            (should (canvas-browser-live-test--wait 30 (lambda () (> canvas-browser--frames 0))))
            (canvas-browser-live-test--lay-out 1024 768)
            (let ((boxes 'waiting))
              (canvas-browser--boxes (lambda (found) (setq boxes found)))
              (should (canvas-browser-live-test--wait 10 (lambda () (listp boxes))))
              (dolist (id '("button" "made" "tab" "signin"))
                (should (canvas-browser-live-test--box-at-p
                         boxes (canvas-browser-live-test--middle id))))
              (should (= (length boxes) 4)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (canvas-browser-cdp-stop)
      (delete-directory canvas-browser-profile-directory t))))

(ert-deftest canvas-browser-live-a-card-a-script-opens-takes-a-hint ()
  ;; GIVEN the cards fixture: a post whose title a link over the whole
  ;;       post covers, with a picture over that link that a script opens,
  ;;       and a card a script opens with a link of its own inside
  ;; WHEN the boxes that take a hint are asked for
  ;; THEN the link over the post takes the place of the title it covers,
  ;;      the post takes one where a click reaches its picture, the card
  ;;      takes one, and the link inside it one: a picture of Reddit, and
  ;;      its title, took none
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (let ((buffer nil)
        (canvas-browser-profile-directory
         (let ((temporary-file-directory (expand-file-name "~/snap/chromium/common/")))
           (make-temp-file "canvas-browser-live-" t))))
    (unwind-protect
        (let ((file (expand-file-name "tests/fixtures/cards.html")))
          (setq buffer (canvas-browser (concat "file://" file)))
          (with-current-buffer buffer
            (should (canvas-browser-live-test--wait 30 (lambda () canvas-browser--session)))
            (should (canvas-browser-live-test--wait 30 (lambda () (> canvas-browser--frames 0))))
            (canvas-browser-live-test--lay-out 1024 768)
            (let ((boxes 'waiting))
              (canvas-browser--boxes (lambda (found) (setq boxes found)))
              (should (canvas-browser-live-test--wait 10 (lambda () (listp boxes))))
              (dolist (id '("title" "media" "card" "inner"))
                (should (canvas-browser-live-test--box-at-p
                         boxes (canvas-browser-live-test--middle id))))
              (should (= (length boxes) 4)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (canvas-browser-cdp-stop)
      (delete-directory canvas-browser-profile-directory t))))

(defun canvas-browser-live-test--near (colour expected)
  "Whether COLOUR, an ARGB32, is EXPECTED within the rounding of a JPEG."
  (cl-every (lambda (shift)
              (< (abs (- (logand (ash colour (- shift)) #xFF)
                         (logand (ash expected (- shift)) #xFF)))
                 24))
            '(0 8 16)))

(ert-deftest canvas-browser-live-copying-a-picture-leaves-the-page-as-it-was ()
  ;; GIVEN the copy fixture, scrolled to its red block, drawn on the canvas
  ;; WHEN the picture of the block is copied, and a moment passes
  ;; THEN the canvas still shows the block where it was: chromium cuts a
  ;;      picture by laying the page out anew for a moment, and a frame
  ;;      drawn in that moment shows the page shrunk into a corner
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (let ((buffer nil) (picture nil)
        (canvas-browser-profile-directory
         (let ((temporary-file-directory (expand-file-name "~/snap/chromium/common/")))
           (make-temp-file "canvas-browser-live-" t))))
    (unwind-protect
        (let ((file (expand-file-name "tests/fixtures/copy.html")))
          (setq buffer (canvas-browser (concat "file://" file)))
          (with-current-buffer buffer
            (should (canvas-browser-live-test--wait 30 (lambda () canvas-browser--session)))
            (should (canvas-browser-live-test--wait 30 (lambda () (> canvas-browser--frames 0))))
            (canvas-browser-live-test--lay-out 1024 768)
            (canvas-browser--evaluate "window.scrollTo(0, 1000)" #'ignore)
            (canvas-browser-live-test--wait 2 (lambda () nil))
            ;; The block stands at 200,200 in the window, 120 by 80.
            (should (canvas-browser-live-test--near
                     (canvas-cairo-pixel canvas-browser--context 260 240) #xFFFF0000))
            (cl-letf (((symbol-function 'canvas-keys-copy-png)
                       (lambda (bytes) (setq picture bytes))))
              (canvas-browser--copy-picture
               (canvas-browser-live-test--place-of "red" #'canvas-browser--blocks) nil)
              (should (canvas-browser-live-test--wait 10 (lambda () picture))))
            (canvas-browser-live-test--wait 2 (lambda () nil))
            (should (canvas-browser-live-test--near
                     (canvas-cairo-pixel canvas-browser--context 260 240) #xFFFF0000))
            (should (canvas-browser-live-test--near
                     (canvas-cairo-pixel canvas-browser--context 700 600) #xFFFFFFFF))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (canvas-browser-cdp-stop)
      (delete-directory canvas-browser-profile-directory t))))

(defun canvas-browser-live-test--press (keys)
  "Run what KEYS, one key, runs in this buffer, as a key press would."
  (let ((last-command-event (aref (key-parse keys) 0)))
    (call-interactively (key-binding (kbd keys)))))

(defun canvas-browser-live-test--settle ()
  "Give chromium a moment to answer what was sent."
  (canvas-browser-live-test--wait 0.4 (lambda () nil)))

(ert-deftest canvas-browser-live-the-caret-marks-and-copies-words ()
  ;; GIVEN the fixture page, with nothing focused
  ;; WHEN v starts the caret, M-f passes the first word, C-SPC sets the
  ;;      mark, two M-f mark two words, and M-w copies
  ;; THEN the caret stood in the heading, the first text in view, AND the
  ;;      kill ring holds the two words
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (let ((buffer nil) (kill-ring nil)
        (canvas-browser-profile-directory
         (let ((temporary-file-directory (expand-file-name "~/snap/chromium/common/")))
           (make-temp-file "canvas-browser-live-" t))))
    (unwind-protect
        (let ((file (expand-file-name "tests/fixtures/page.html")))
          (setq buffer (canvas-browser (concat "file://" file)))
          (with-current-buffer buffer
            (should (canvas-browser-live-test--wait 30 (lambda () canvas-browser--session)))
            (should (canvas-browser-live-test--wait 30 (lambda () (> canvas-browser--frames 0))))
            (canvas-browser-live-test--lay-out 1024 768)
            (canvas-browser-live-test--press "v")
            (should (canvas-browser-live-test--wait 5 (lambda () canvas-browser--caret-box)))
            (dolist (keys '("M-f" "C-SPC" "M-f" "M-f"))
              (canvas-browser-live-test--press keys)
              (canvas-browser-live-test--settle))
            (canvas-browser-live-test--press "M-w")
            (should (canvas-browser-live-test--wait
                     5 (lambda () (equal (car kill-ring) " page for"))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (canvas-browser-cdp-stop)
      (delete-directory canvas-browser-profile-directory t))))

(defun canvas-browser-live-test--jump (text)
  "Press M-j, and type TEXT as the text of the jump."
  (cl-letf (((symbol-function 'canvas-browser--read-jump-text) (lambda () text)))
    (canvas-browser-live-test--press "M-j"))
  (canvas-browser-live-test--settle))

(ert-deftest canvas-browser-live-m-j-jumps-and-marks-to-text ()
  ;; GIVEN the fixture page in normal state, where "parser" and "browser"
  ;;       show once each
  ;; WHEN M-j jumps to "parser", C-SPC sets the mark, M-j jumps to
  ;;      "browser", and M-w copies
  ;; THEN the caret started at "parser", AND the kill ring holds what lies
  ;;      between the two
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (let ((buffer nil) (kill-ring nil)
        (canvas-browser-profile-directory
         (let ((temporary-file-directory (expand-file-name "~/snap/chromium/common/")))
           (make-temp-file "canvas-browser-live-" t))))
    (unwind-protect
        (let ((file (expand-file-name "tests/fixtures/page.html")))
          (setq buffer (canvas-browser (concat "file://" file)))
          (with-current-buffer buffer
            (should (canvas-browser-live-test--wait 30 (lambda () canvas-browser--session)))
            (should (canvas-browser-live-test--wait 30 (lambda () (> canvas-browser--frames 0))))
            (canvas-browser-live-test--lay-out 1024 768)
            (canvas-browser-live-test--jump "parser")
            (should (canvas-browser-live-test--wait 5 (lambda () canvas-browser--caret-box)))
            (should canvas-browser--caret)
            (canvas-browser-live-test--press "C-SPC")
            (canvas-browser-live-test--jump "browser")
            (canvas-browser-live-test--press "M-w")
            (should (canvas-browser-live-test--wait
                     5 (lambda () (equal (car kill-ring) "parser, canvas, "))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (canvas-browser-cdp-stop)
      (delete-directory canvas-browser-profile-directory t))))

(defun canvas-browser-live-test--places (text)
  "The boxes of the places in view of this page that show TEXT."
  (let ((boxes 'waiting))
    (canvas-browser--evaluate
     (canvas-browser--caret-script (format "find(%s)" (json-encode text)))
     (lambda (found) (setq boxes found)))
    (canvas-browser-live-test--wait 10 (lambda () (listp boxes)))
    boxes))

(ert-deftest canvas-browser-live-a-jump-finds-only-text-that-shows ()
  ;; GIVEN the modal fixture: a link under its veil, a link below the
  ;;       window, and a text put into a web component through a slot
  ;; WHEN the places of "link" and of "continue" are looked for
  ;; THEN neither link is found, AND the slotted text is, once: the text
  ;;      in the frame of the Google button lies in a page of its own
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (let ((buffer nil)
        (canvas-browser-profile-directory
         (let ((temporary-file-directory (expand-file-name "~/snap/chromium/common/")))
           (make-temp-file "canvas-browser-live-" t))))
    (unwind-protect
        (let ((file (expand-file-name "tests/fixtures/modal.html")))
          (setq buffer (canvas-browser (concat "file://" file)))
          (with-current-buffer buffer
            (should (canvas-browser-live-test--wait 30 (lambda () canvas-browser--session)))
            (should (canvas-browser-live-test--wait 30 (lambda () (> canvas-browser--frames 0))))
            (canvas-browser-live-test--lay-out 1024 768)
            (should-not (canvas-browser-live-test--places "link"))
            (let ((places (canvas-browser-live-test--places "continue"))
                  (middle (canvas-browser-live-test--middle "slotted")))
              (should (= (length places) 1))
              ;; The word starts the link, so it lies on the line of the
              ;; link, to the left of its middle.
              (should (<= (plist-get (car places) :y) (cdr middle)
                          (+ (plist-get (car places) :y) (plist-get (car places) :h))))
              (should (< (plist-get (car places) :x) (car middle))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (canvas-browser-cdp-stop)
      (delete-directory canvas-browser-profile-directory t))))

(defun canvas-browser-live-test--rect (id)
  "The box of the element ID of this page, as (X Y W H)."
  (let ((answer 'waiting))
    (canvas-browser--evaluate
     (format "(() => { const r = document.getElementById('%s').getBoundingClientRect();
                       return [r.x, r.y, r.width, r.height]; })()"
             id)
     (lambda (value) (setq answer value)))
    (canvas-browser-live-test--wait 10 (lambda () (not (eq answer 'waiting))))
    answer))

(defun canvas-browser-live-test--place-in-p (places rect)
  "Whether one of PLACES, boxes as plists, lies inside RECT, (X Y W H)."
  (pcase-let ((`(,x ,y ,w ,h) rect))
    (cl-some (lambda (place)
               (and (<= (1- x) (plist-get place :x))
                    (<= (1- y) (plist-get place :y))
                    (<= (+ (plist-get place :x) (plist-get place :w)) (+ x w 1))
                    (<= (+ (plist-get place :y) (plist-get place :h)) (+ y h 1))))
             places)))

(ert-deftest canvas-browser-live-a-jump-leaves-hidden-text-alone ()
  ;; GIVEN the hidden fixture: "secret" stands in text for screen readers,
  ;;       past the edge of a box too narrow for it, in text made
  ;;       invisible, in text made clear and under a banner; "shown"
  ;;       stands in plain text, in the narrow box, in a popup that leaves
  ;;       its parent's box, and in a title a clear link of its card covers
  ;; WHEN the places of "secret" and of "shown" are looked for
  ;; THEN no secret is found, AND each shown one is, the popup and the
  ;;      title included
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (let ((buffer nil)
        (canvas-browser-profile-directory
         (let ((temporary-file-directory (expand-file-name "~/snap/chromium/common/")))
           (make-temp-file "canvas-browser-live-" t))))
    (unwind-protect
        (let ((file (expand-file-name "tests/fixtures/hidden.html")))
          (setq buffer (canvas-browser (concat "file://" file)))
          (with-current-buffer buffer
            (should (canvas-browser-live-test--wait 30 (lambda () canvas-browser--session)))
            (should (canvas-browser-live-test--wait 30 (lambda () (> canvas-browser--frames 0))))
            (canvas-browser-live-test--lay-out 1024 768)
            (should-not (canvas-browser-live-test--places "secret"))
            (let ((places (canvas-browser-live-test--places "shown")))
              (should (= (length places) 4))
              (dolist (id '("popup" "title"))
                (should (canvas-browser-live-test--place-in-p
                         places (canvas-browser-live-test--rect id)))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (canvas-browser-cdp-stop)
      (delete-directory canvas-browser-profile-directory t))))

(ert-deftest canvas-browser-live-a-region-of-a-field-is-copied-and-cut ()
  ;; GIVEN the plain field of the typing fixture, holding "hello world"
  ;; WHEN C-a, C-SPC and M-f mark the first word, M-w copies it, and it is
  ;;      marked again and cut with C-w
  ;; THEN the kill ring holds "hello", AND the field holds " world"
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (let ((buffer nil) (kill-ring nil)
        (canvas-browser-profile-directory
         (let ((temporary-file-directory (expand-file-name "~/snap/chromium/common/")))
           (make-temp-file "canvas-browser-live-" t))))
    (unwind-protect
        (let ((file (expand-file-name "tests/fixtures/typing.html")))
          (setq buffer (canvas-browser (concat "file://" file)))
          (with-current-buffer buffer
            (should (canvas-browser-live-test--wait 30 (lambda () canvas-browser--session)))
            (should (canvas-browser-live-test--wait 30 (lambda () (> canvas-browser--frames 0))))
            (should (canvas-browser-live-test--click-and-see "plain"))
            (mapc #'canvas-browser--type-character "hello world")
            (dolist (keys '("C-a" "C-SPC" "M-f"))
              (canvas-browser-live-test--press keys)
              (canvas-browser-live-test--settle))
            (canvas-browser-live-test--press "M-w")
            (should (canvas-browser-live-test--wait 5 (lambda () (equal (car kill-ring) "hello"))))
            (dolist (keys '("C-a" "C-SPC" "M-f"))
              (canvas-browser-live-test--press keys)
              (canvas-browser-live-test--settle))
            (canvas-browser-live-test--press "C-w")
            (should (canvas-browser-live-test--wait
                     5 (lambda () (equal (canvas-browser-live-test--value "plain") " world"))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (canvas-browser-cdp-stop)
      (delete-directory canvas-browser-profile-directory t))))

;;;; Marking in an editor that a page built itself

(defmacro canvas-browser-live-test--in-editor (&rest body)
  "Run BODY in a page buffer of the editor fixture, typing into its editor.
The kill ring is empty, and chromium has a profile of its own."
  (declare (indent 0))
  `(let ((buffer nil) (kill-ring nil)
         (canvas-browser-profile-directory
          (let ((temporary-file-directory (expand-file-name "~/snap/chromium/common/")))
            (make-temp-file "canvas-browser-live-" t))))
     (unwind-protect
         (progn
           (setq buffer (canvas-browser
                         (concat "file://" (expand-file-name "tests/fixtures/editor.html"))))
           (with-current-buffer buffer
             (should (canvas-browser-live-test--wait 30 (lambda () canvas-browser--session)))
             (should (canvas-browser-live-test--wait 30 (lambda () (> canvas-browser--frames 0))))
             (should (canvas-browser-live-test--click-and-see "one"))
             ,@body))
       (when (buffer-live-p buffer) (kill-buffer buffer))
       (canvas-browser-cdp-stop)
       (delete-directory canvas-browser-profile-directory t))))

(defun canvas-browser-live-test--copied-both-p ()
  "Whether the newest kill holds the text of both paragraphs of the editor."
  (let ((text (car kill-ring)))
    (and (stringp text)
         (string-search "first paragraph" text)
         (string-search "second paragraph" text)
         t)))

(ert-deftest canvas-browser-live-c-x-h-marks-all-of-an-editor ()
  ;; GIVEN the editor fixture, an element that can be edited with two
  ;;       paragraphs in it, with the keys going to it
  ;; WHEN C-x h is pressed, and then M-w
  ;; THEN the kill ring holds the text of both paragraphs
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--in-editor
    (canvas-browser-live-test--press "C-x h")
    (canvas-browser-live-test--settle)
    (canvas-browser-live-test--press "M-w")
    (should (canvas-browser-live-test--wait 5 #'canvas-browser-live-test--copied-both-p))))

(ert-deftest canvas-browser-live-a-drag-marks-the-text-it-covers ()
  ;; GIVEN the editor fixture, with the keys going to its editor
  ;; WHEN the mouse is dragged from the start of the first paragraph to
  ;;      the end of the second, and then M-w is pressed
  ;; THEN the kill ring holds the text of both paragraphs
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--in-editor
    (pcase-let ((`(,x1 ,y1 ,_ ,h1) (canvas-browser-live-test--rect "one"))
                (`(,x2 ,y2 ,w2 ,h2) (canvas-browser-live-test--rect "two")))
      (canvas-browser--drag (round (+ x1 1)) (round (+ y1 (/ h1 2.0)))
                            (round (+ x2 w2 -2)) (round (+ y2 (/ h2 2.0)))))
    (canvas-browser-live-test--settle)
    (canvas-browser-live-test--press "M-w")
    (should (canvas-browser-live-test--wait 5 #'canvas-browser-live-test--copied-both-p))))

(defun canvas-browser-live-test--editor-lines ()
  "The text of each paragraph of the editor fixture, as a list."
  (let ((answer 'waiting))
    (canvas-browser--evaluate
     "Array.from(document.querySelectorAll('#editor p')).map(p => p.textContent)"
     (lambda (value) (setq answer value)))
    (canvas-browser-live-test--wait 10 (lambda () (not (eq answer 'waiting))))
    answer))

(ert-deftest canvas-browser-live-c-k-deletes-to-the-end-of-a-line ()
  ;; GIVEN the editor fixture, with the cursor at the start of its first
  ;;       paragraph
  ;; WHEN C-k is pressed, and then C-k again
  ;; THEN the first deletes the text of the paragraph and leaves the
  ;;      empty line, AND the second deletes the line break, so that the
  ;;      second paragraph is the first line, as with C-k in a buffer,
  ;;      AND the two make one kill, the text and a newline
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--in-editor
    (canvas-browser-live-test--press "C-a")
    (canvas-browser-live-test--settle)
    (canvas-browser-live-test--press "C-k")
    (should (canvas-browser-live-test--wait
             5 (lambda () (equal (canvas-browser-live-test--editor-lines) '("" "second paragraph")))))
    (should (equal kill-ring '("first paragraph")))
    (let ((last-command 'kill-region))
      (canvas-browser-live-test--press "C-k"))
    (should (canvas-browser-live-test--wait
             5 (lambda () (equal (canvas-browser-live-test--editor-lines) '("second paragraph")))))
    (should (canvas-browser-live-test--wait
             5 (lambda () (equal kill-ring '("first paragraph\n")))))))

(ert-deftest canvas-browser-live-c-k-deletes-the-rest-of-a-plain-field ()
  ;; GIVEN the plain field of the typing fixture, holding "hello world",
  ;;       with the cursor after "hello"
  ;; WHEN C-k is pressed
  ;; THEN the field holds "hello", AND " world" is the newest kill
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (let ((buffer nil) (kill-ring nil)
        (canvas-browser-profile-directory
         (let ((temporary-file-directory (expand-file-name "~/snap/chromium/common/")))
           (make-temp-file "canvas-browser-live-" t))))
    (unwind-protect
        (progn
          (setq buffer (canvas-browser
                        (concat "file://" (expand-file-name "tests/fixtures/typing.html"))))
          (with-current-buffer buffer
            (should (canvas-browser-live-test--wait 30 (lambda () canvas-browser--session)))
            (should (canvas-browser-live-test--wait 30 (lambda () (> canvas-browser--frames 0))))
            (should (canvas-browser-live-test--click-and-see "plain"))
            (mapc #'canvas-browser--type-character "hello world")
            (canvas-browser-live-test--press "C-a")
            (dotimes (_ 5) (canvas-browser-live-test--press "C-f"))
            (canvas-browser-live-test--settle)
            (canvas-browser-live-test--press "C-k")
            (should (canvas-browser-live-test--wait
                     5 (lambda () (equal (canvas-browser-live-test--value "plain") "hello"))))
            (should (equal (car kill-ring) " world"))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (canvas-browser-cdp-stop)
      (delete-directory canvas-browser-profile-directory t))))

;;;; The keys of a browser, sent from the keys of Emacs

(defmacro canvas-browser-live-test--in-plain-field (text &rest body)
  "Run BODY in a page buffer of the typing fixture, with TEXT typed into
its plain field.  The kill ring is empty, and chromium has a profile of
its own."
  (declare (indent 1))
  `(let ((buffer nil) (kill-ring nil)
         (canvas-browser-profile-directory
          (let ((temporary-file-directory (expand-file-name "~/snap/chromium/common/")))
            (make-temp-file "canvas-browser-live-" t))))
     (unwind-protect
         (progn
           (setq buffer (canvas-browser
                         (concat "file://" (expand-file-name "tests/fixtures/typing.html"))))
           (with-current-buffer buffer
             (should (canvas-browser-live-test--wait 30 (lambda () canvas-browser--session)))
             (should (canvas-browser-live-test--wait 30 (lambda () (> canvas-browser--frames 0))))
             (should (canvas-browser-live-test--click-and-see "plain"))
             (mapc #'canvas-browser--type-character ,text)
             (canvas-browser-live-test--settle)
             ,@body))
       (when (buffer-live-p buffer) (kill-buffer buffer))
       (canvas-browser-cdp-stop)
       (delete-directory canvas-browser-profile-directory t))))

(ert-deftest canvas-browser-live-the-undo-key-takes-back-what-was-typed ()
  ;; GIVEN the plain field of the typing fixture, with "abc" typed into it
  ;; WHEN C-/ is pressed
  ;; THEN the field no longer holds "abc": chromium took typing back
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--in-plain-field "abc"
    (should (equal (canvas-browser-live-test--value "plain") "abc"))
    (canvas-browser-live-test--press "C-/")
    (should (canvas-browser-live-test--wait
             5 (lambda () (not (equal (canvas-browser-live-test--value "plain") "abc")))))))

(ert-deftest canvas-browser-live-shift-and-a-motion-mark-in-a-field ()
  ;; GIVEN the plain field of the typing fixture, holding "hello", with
  ;;       the cursor at its start
  ;; WHEN S-<right> is pressed twice, and then M-w
  ;; THEN the newest kill is "he"
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--in-plain-field "hello"
    (dolist (keys '("C-a" "S-<right>" "S-<right>"))
      (canvas-browser-live-test--press keys)
      (canvas-browser-live-test--settle))
    (canvas-browser-live-test--press "M-w")
    (should (canvas-browser-live-test--wait 5 (lambda () (equal (car kill-ring) "he"))))))

(ert-deftest canvas-browser-live-shift-and-return-break-the-line-in-its-paragraph ()
  ;; GIVEN the editor fixture, with the cursor at the end of its first
  ;;       paragraph
  ;; WHEN S-<return> is pressed
  ;; THEN the first paragraph has a line break in it, AND the editor has
  ;;      two paragraphs still: Shift and Enter break the line and start
  ;;      no new paragraph, as in a chat where Enter alone sends
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--in-editor
    (canvas-browser-live-test--press "C-e")
    (canvas-browser-live-test--settle)
    (canvas-browser-live-test--press "S-<return>")
    (let ((answer nil))
      (should (canvas-browser-live-test--wait
               5 (lambda ()
                   (canvas-browser--evaluate
                    "[document.querySelectorAll('#editor p').length,
                      !!document.querySelector('#one br')]"
                    (lambda (value) (setq answer value)))
                   (canvas-browser-live-test--settle)
                   (equal answer '(2 t))))))))

(ert-deftest canvas-browser-live-a-drag-over-text-is-copied-with-m-w ()
  ;; GIVEN the fixture page, with nothing focused
  ;; WHEN the mouse is dragged over its first paragraph, from its start
  ;;      to its end
  ;; THEN the caret has the keys with the mark set, AND nothing is
  ;;      copied yet
  ;; WHEN M-w is pressed
  ;; THEN the newest kill is the text of the paragraph
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (let ((buffer nil) (kill-ring nil) (mouse-drag-copy-region nil)
        (canvas-browser-profile-directory
         (let ((temporary-file-directory (expand-file-name "~/snap/chromium/common/")))
           (make-temp-file "canvas-browser-live-" t))))
    (unwind-protect
        (progn
          (setq buffer (canvas-browser
                        (concat "file://" (expand-file-name "tests/fixtures/page.html"))))
          (with-current-buffer buffer
            (should (canvas-browser-live-test--wait 30 (lambda () canvas-browser--session)))
            (should (canvas-browser-live-test--wait 30 (lambda () (> canvas-browser--frames 0))))
            (canvas-browser-live-test--lay-out 1024 768)
            (let ((box 'waiting))
              (canvas-browser--evaluate
               "(r => [r.x, r.y, r.width, r.height])(document.querySelector('p').getBoundingClientRect())"
               (lambda (value) (setq box value)))
              (should (canvas-browser-live-test--wait 10 (lambda () (consp box))))
              (pcase-let ((`(,x ,y ,w ,h) box))
                (canvas-browser--drag (round (+ x 1)) (round (+ y (/ h 2.0)))
                                      (round (+ x w -2)) (round (+ y (/ h 2.0))))))
            (should (canvas-browser-live-test--wait 5 (lambda () canvas-browser--caret-mark)))
            (should canvas-browser--caret)
            (should-not kill-ring)
            (canvas-browser-live-test--press "M-w")
            (should (canvas-browser-live-test--wait
                     5 (lambda ()
                         (equal (car kill-ring)
                                "Some words that the text test looks for: parser, canvas, browser."))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (canvas-browser-cdp-stop)
      (delete-directory canvas-browser-profile-directory t))))

;;;; A file for a page, picked in dired

(require 'dired)

(defmacro canvas-browser-live-test--uploading (directory &rest body)
  "Run BODY in a page buffer of the upload fixture.
DIRECTORY is bound to a new directory that holds one.txt and two.txt,
and is where a choice of files starts.  It is in the home of the snap,
which a snap chromium can read, so no copy of a file is left behind.
Chromium has a profile of its own, and what the test leaves of a choice
is cleared away."
  (declare (indent 1))
  `(let* ((buffer nil)
          (,directory
           (let ((temporary-file-directory (expand-file-name "~/snap/chromium/common/")))
             (file-name-as-directory (make-temp-file "canvas-browser-upload-" t))))
          (canvas-browser--attach-directory ,directory)
          (canvas-browser-profile-directory
           (let ((temporary-file-directory (expand-file-name "~/snap/chromium/common/")))
             (make-temp-file "canvas-browser-live-" t))))
     (dolist (name '("one.txt" "two.txt"))
       (write-region name nil (expand-file-name name ,directory) nil 'silent))
     (unwind-protect
         (progn
           (setq buffer (canvas-browser
                         (concat "file://" (expand-file-name "tests/fixtures/upload.html"))))
           (with-current-buffer buffer
             (should (canvas-browser-live-test--wait 30 (lambda () canvas-browser--session)))
             (should (canvas-browser-live-test--wait 30 (lambda () (> canvas-browser--frames 0))))
             ,@body))
       (canvas-browser--attach-finish)
       (when (buffer-live-p buffer) (kill-buffer buffer))
       (canvas-browser-cdp-stop)
       (delete-directory canvas-browser-profile-directory t)
       (delete-directory ,directory t))))

(defun canvas-browser-live-test--ask-for-files (id)
  "Click the file field ID of this page; the dired buffer that then opens."
  (let ((middle (canvas-browser-live-test--middle id)))
    (should middle)
    (canvas-browser--click (car middle) (cdr middle)))
  (should (canvas-browser-live-test--wait 10 (lambda () canvas-browser--chooser)))
  (seq-find (lambda (buffer) (buffer-local-value 'canvas-browser-attach-mode buffer))
            (buffer-list)))

(defun canvas-browser-live-test--page-says (script)
  "The value of SCRIPT in this page."
  (let ((answer 'waiting))
    (canvas-browser--evaluate script (lambda (value) (setq answer value)))
    (canvas-browser-live-test--wait 10 (lambda () (not (eq answer 'waiting))))
    answer))

(ert-deftest canvas-browser-live-a-file-picked-in-dired-reaches-the-page ()
  ;; GIVEN the upload fixture, and a directory with two files
  ;; WHEN the field for several files is clicked, both files are marked
  ;;      in the dired buffer that opens, and C-c C-c is pressed
  ;; THEN the page has both files, with their names and their sizes
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--uploading directory
    (let ((page (current-buffer)))
      (with-current-buffer (canvas-browser-live-test--ask-for-files "many")
        (dolist (name '("one.txt" "two.txt"))
          (dired-goto-file (expand-file-name name directory))
          (dired-mark 1))
        (canvas-browser-attach-send))
      (with-current-buffer page
        (should (canvas-browser-live-test--wait
                 10 (lambda ()
                      (equal (canvas-browser-live-test--page-says "window.picked.many || null")
                             '("one.txt:7" "two.txt:7")))))))))

(ert-deftest canvas-browser-live-a-cancelled-choice-is-told-to-the-page ()
  ;; GIVEN the upload fixture
  ;; WHEN the field for one file is clicked, and C-c C-k is pressed in
  ;;      the dired buffer that opens
  ;; THEN the page is told that the choice of that field was cancelled,
  ;;      AND the field has no file
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--uploading directory
    (let ((page (current-buffer)))
      (with-current-buffer (canvas-browser-live-test--ask-for-files "one")
        (canvas-browser-attach-cancel))
      (with-current-buffer page
        (should (canvas-browser-live-test--wait
                 10 (lambda ()
                      (equal (canvas-browser-live-test--page-says "window.cancelled")
                             '("one")))))
        (should (equal (canvas-browser-live-test--page-says
                        "document.getElementById('one').files.length")
                       0))))))

(ert-deftest canvas-browser-live-a-page-is-sent-to-a-receiver ()
  ;; GIVEN the fixture page, with nothing marked, and one receiver
  ;; WHEN the page is sent to that receiver
  ;; THEN the receiver gets the text of the page, its address and its
  ;;      title, in the same call: the command waits for chromium
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (let* ((buffer nil)
         (received nil)
         (canvas-browser-receivers
          (list (cons "test" (lambda (content) (setq received content)))))
         (canvas-browser-profile-directory
          (let ((temporary-file-directory (expand-file-name "~/snap/chromium/common/")))
            (make-temp-file "canvas-browser-live-" t))))
    (unwind-protect
        (let ((url (concat "file://" (expand-file-name "tests/fixtures/page.html"))))
          (setq buffer (canvas-browser url))
          (with-current-buffer buffer
            (should (canvas-browser-live-test--wait 30 (lambda () canvas-browser--session)))
            ;; The page has the name of its file until it has loaded.
            (should (canvas-browser-live-test--wait
                     30 (lambda () (equal canvas-browser--title "canvas-browser fixture"))))
            (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "test")))
              (canvas-browser-send-to))
            (should (eq (plist-get received :kind) 'page))
            (should (string-search "the live test" (plist-get received :text)))
            (should (equal (plist-get received :url) canvas-browser--url))
            (should (equal (plist-get received :title) "canvas-browser fixture"))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (canvas-browser-cdp-stop)
      (delete-directory canvas-browser-profile-directory t))))

(defun canvas-browser-live-test--loaded-p (address)
  "Whether the page of this buffer says that it is at ADDRESS and has loaded."
  (equal (canvas-browser-live-test--page-says "location.href + ' ' + document.readyState")
         (concat address " complete")))

(defmacro canvas-browser-live-test--at (url &rest body)
  "Run BODY in a page buffer at URL, once the page has loaded.
Chromium has a profile of its own."
  (declare (indent 1))
  (let ((address (make-symbol "address")))
    `(let ((buffer nil)
           (,address ,url)
           (canvas-browser-profile-directory
            (let ((temporary-file-directory (expand-file-name "~/snap/chromium/common/")))
              (make-temp-file "canvas-browser-live-" t))))
       (unwind-protect
           (progn
             (setq buffer (canvas-browser ,address))
             (with-current-buffer buffer
               (should (canvas-browser-live-test--wait 30 (lambda () canvas-browser--session)))
               ;; A new page stands at about:blank first, with that as
               ;; its title.  And the buffer has the address before the
               ;; page has read its own text.  So the page itself is
               ;; asked whether it has loaded.
               (should (canvas-browser-live-test--wait
                        30 (lambda () (and canvas-browser--title
                                           (equal canvas-browser--url ,address)
                                           (canvas-browser-live-test--loaded-p ,address)))))
               ,@body))
         (when (buffer-live-p buffer) (kill-buffer buffer))
         (canvas-browser-cdp-stop)
         (delete-directory canvas-browser-profile-directory t)))))

(defmacro canvas-browser-live-test--in-fixture (name &rest body)
  "Run BODY in a page buffer of the fixture NAME, once the page is there."
  (declare (indent 1))
  `(canvas-browser-live-test--at (concat "file://" (expand-file-name ,name "tests/fixtures/"))
     ,@body))

(defun canvas-browser-live-test--pick (selector)
  "Pick the part of this page that SELECTOR matches, as `S\=' and its label do."
  (cl-letf (((symbol-function 'canvas-browser--draw-hints) #'ignore)
            ((symbol-function 'canvas-browser--draw-outline) #'ignore)
            ((symbol-function 'canvas-browser--read-hint)
             (lambda (&rest _)
               ;; The whole page has the first label.
               (list (1+ (canvas-browser-live-test--page-says
                          (format "window.__canvasBrowserParts.findIndex(e => e.matches('%s'))"
                                  selector)))))))
    (setq canvas-browser--part nil)
    (canvas-browser-pick-part)
    (should (canvas-browser-live-test--wait 10 (lambda () canvas-browser--part)))))

(ert-deftest canvas-browser-live-a-part-of-a-page-is-sent-without-the-rest ()
  ;; GIVEN the thread fixture, whose main part holds the post and its
  ;;       comments, with a sidebar and a heading outside it
  ;; WHEN the main part is picked, and the page is sent to a receiver
  ;;      that asks for the HTML as well
  ;; THEN the receiver gets the text of the main part, from its first
  ;;      line to its last, AND its HTML as the page has it, with one
  ;;      comment inside the other, AND neither holds the sidebar
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--in-fixture "thread.html"
    (canvas-browser-live-test--lay-out 800 600)
    (canvas-browser-live-test--pick "main")
    (should-not (plist-get canvas-browser--part :scrolls))
    (let* ((received nil)
           (html nil)
           (canvas-browser-receivers
            (list (cons "test" (lambda (content)
                                 (setq received content
                                       html (canvas-browser-content-html content)))))))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "test"))
                ((symbol-function 'canvas-browser--draw-tint) #'ignore))
        (canvas-browser-send-to))
      (should (eq (plist-get received :kind) 'part))
      (should (string-match-p "\\`A post\n\\(.\\|\n\\)*bob 3h ago\n\\(.\\|\n\\)*under top\\'"
                              (plist-get received :text)))
      (should-not (string-search "sidebar" (plist-get received :text)))
      (should (string-prefix-p "<main>" html))
      (should (string-match-p "<x-comment id=\"c1\">\\(.\\|\n\\)*<x-comment id=\"c2\" slot=\"children\">"
                              html))
      (should-not (string-search "sidebar" html)))))

(ert-deftest canvas-browser-live-the-html-of-marked-text-is-what-the-mark-covers ()
  ;; GIVEN the thread fixture, where the selection of the page covers
  ;;       one comment
  ;; WHEN a receiver asks for the HTML of the marked text
  ;; THEN it gets the HTML inside that comment, AND nothing of the
  ;;      comment after it
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--in-fixture "thread.html"
    (canvas-browser-live-test--page-says
     "getSelection().selectAllChildren(document.getElementById('c3')); true")
    (let ((html (canvas-browser-content-html '(:kind text))))
      (should (string-search "<div slot=\"meta\"><b>alice</b>" html))
      (should (string-search "I see." html))
      (should-not (string-search "carol" html)))))

(ert-deftest canvas-browser-live-the-parts-of-a-page-are-what-scrolls-and-what-it-marks ()
  ;; GIVEN the parts fixture: a main part, a list that scrolls on its
  ;;       own, and a sidebar
  ;; WHEN the parts of the page are asked for
  ;; THEN all three are parts, AND only the list is said to scroll
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--in-fixture "parts.html"
    (canvas-browser-live-test--lay-out 800 600)
    (let ((boxes 'waiting))
      (canvas-browser--parts (lambda (answer) (setq boxes answer)))
      (should (canvas-browser-live-test--wait 10 (lambda () (not (eq boxes 'waiting)))))
      (should (equal (mapcar (lambda (box) (eq (plist-get box :scrolls) t)) boxes)
                     '(nil t nil)))
      (should (equal (canvas-browser-live-test--page-says
                      "window.__canvasBrowserParts.map(e => e.tagName)")
                     '("MAIN" "DIV" "ASIDE"))))))

(ert-deftest canvas-browser-live-a-pane-whose-content-fits-is-a-part ()
  ;; GIVEN the panes fixture, laid out as an application lays itself out
  ;;       with nothing that marks a part: a list that scrolls, beside
  ;;       it a pane that could scroll and whose short content fits, and
  ;;       in the pane a small box of code that could scroll as well
  ;; WHEN the parts of the page are asked for
  ;; THEN the list and the pane are parts, AND only the list is said to
  ;;      scroll, AND the small box is no part
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--in-fixture "panes.html"
    (canvas-browser-live-test--lay-out 800 600)
    (let ((boxes 'waiting))
      (canvas-browser--parts (lambda (answer) (setq boxes answer)))
      (should (canvas-browser-live-test--wait 10 (lambda () (not (eq boxes 'waiting)))))
      (should (equal (canvas-browser-live-test--page-says
                      "window.__canvasBrowserParts.map(e => e.id)")
                     '("side" "pane")))
      (should (equal (mapcar (lambda (box) (eq (plist-get box :scrolls) t)) boxes)
                     '(t nil))))))

(ert-deftest canvas-browser-live-the-scroll-keys-move-the-part-that-is-picked ()
  ;; GIVEN the parts fixture, where the list that scrolls is picked
  ;; WHEN a line is scrolled
  ;; THEN the list has moved, AND the page has not
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--in-fixture "parts.html"
    (canvas-browser-live-test--lay-out 800 600)
    (canvas-browser-live-test--pick "#list")
    (should (plist-get canvas-browser--part :scrolls))
    (canvas-browser-scroll-line-up)
    (should (canvas-browser-live-test--wait
             10 (lambda ()
                  (> (canvas-browser-live-test--page-says
                      "document.getElementById('list').scrollTop")
                     0))))
    (should (equal (canvas-browser-live-test--page-says "window.scrollY") 0))))

(defun canvas-browser-live-test--box-of (selector)
  "The box of the element that SELECTOR matches, as (LEFT TOP WIDTH HEIGHT)."
  (canvas-browser-live-test--page-says
   (format "(e => { if (!e) return null; const r = e.getBoundingClientRect();
                    return [r.left, r.top, r.width, r.height].map(Math.round); })(document.querySelector('%s'))"
           selector)))

(defun canvas-browser-live-test--marked-p (selector)
  "Whether the mark of the part that is picked lies on what SELECTOR matches."
  (canvas-browser-live-test--wait
   10 (lambda ()
        (let ((mark (canvas-browser-live-test--box-of "#canvas-browser-mark")))
          (and mark (equal mark (canvas-browser-live-test--box-of selector)))))))

(ert-deftest canvas-browser-live-the-part-that-is-picked-has-a-mark-over-the-page ()
  ;; GIVEN the parts fixture
  ;; WHEN the main part is picked, then the sidebar, then the whole page
  ;; THEN a mark lies on the part that is picked, over the page and not
  ;;      in the part, so that nothing inside the part covers it, AND the
  ;;      part itself is left as it was, AND the mark is gone with the
  ;;      whole page picked
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--in-fixture "parts.html"
    (canvas-browser-live-test--lay-out 800 600)
    (canvas-browser-live-test--pick "main")
    (should (canvas-browser-live-test--marked-p "main"))
    (should (equal (canvas-browser-live-test--page-says
                    "document.getElementById('canvas-browser-mark').parentNode === document.documentElement")
                   t))
    (should (equal (canvas-browser-live-test--page-says
                    "document.querySelector('main').style.outline")
                   ""))
    (canvas-browser-live-test--pick "aside")
    (should (canvas-browser-live-test--marked-p "aside"))
    (should (equal (canvas-browser-live-test--page-says
                    "document.querySelectorAll('#canvas-browser-mark').length")
                   1))
    (cl-letf (((symbol-function 'canvas-browser--draw-hints) #'ignore)
              ((symbol-function 'canvas-browser--draw-outline) #'ignore)
              ((symbol-function 'canvas-browser--read-hint) (lambda (&rest _) (list 0))))
      (canvas-browser-pick-part)
      (should (canvas-browser-live-test--wait 10 (lambda () (null canvas-browser--part)))))
    (should (canvas-browser-live-test--wait
             ;; The null of JSON comes as `:null'.
             10 (lambda () (eq :null (canvas-browser-live-test--box-of "#canvas-browser-mark")))))))

(ert-deftest canvas-browser-live-the-mark-follows-its-part-when-the-page-scrolls ()
  ;; GIVEN the thread fixture in a low window, with its main part picked
  ;; WHEN the page scrolls
  ;; THEN the part has moved, AND the mark lies on it again
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--in-fixture "thread.html"
    (canvas-browser-live-test--lay-out 800 300)
    (canvas-browser-live-test--pick "main")
    (should (canvas-browser-live-test--marked-p "main"))
    (let ((before (canvas-browser-live-test--box-of "main")))
      (canvas-browser-scroll-line-up)
      (should (canvas-browser-live-test--wait
               10 (lambda () (not (equal before (canvas-browser-live-test--box-of "main"))))))
      (should (canvas-browser-live-test--marked-p "main")))))

(ert-deftest canvas-browser-live-the-mark-sends-no-frames-while-the-page-stands-still ()
  ;; GIVEN the parts fixture, with its main part picked and the page at rest
  ;; WHEN two seconds pass
  ;; THEN chromium sent no frame: the mark watches its part without
  ;;      drawing anything while the part stays where it is
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--in-fixture "parts.html"
    (canvas-browser-live-test--lay-out 800 600)
    (canvas-browser-live-test--pick "main")
    (should (canvas-browser-live-test--marked-p "main"))
    (let ((arrived 0))
      (canvas-browser-live-test--wait 2 (lambda () nil))
      (cl-letf* ((frame (symbol-function 'canvas-browser--frame))
                 ((symbol-function 'canvas-browser--frame)
                  (lambda (params) (cl-incf arrived) (funcall frame params))))
        (canvas-browser-live-test--wait 2 (lambda () nil)))
      (should (= arrived 0)))))

(ert-deftest canvas-browser-live-the-text-of-a-page-without-components-is-as-chromium-gives-it ()
  ;; GIVEN the fixture page, which has no web component
  ;; WHEN the text of the page is read
  ;; THEN it is the text that chromium gives for the page, to the letter
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--in-fixture "page.html"
    (should (equal (canvas-browser--page-text)
                   (canvas-browser-live-test--page-says "document.body.innerText")))))

(ert-deftest canvas-browser-live-the-text-of-a-page-holds-what-its-components-draw ()
  ;; GIVEN the components fixture: a panel that is a web component, which
  ;;       draws rows and a sentence of its own, with components inside
  ;;       them, shows one child that the page gave it, and hides another
  ;; WHEN the text of the page is read
  ;; THEN it holds all the text that the page shows, in the order it
  ;;      shows it, also what the components draw themselves, where
  ;;      chromium gives only what the page gave the panel as a child,
  ;;      AND it holds nothing that the page does not show
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--in-fixture "components.html"
    (should (equal (canvas-browser-live-test--page-says "document.body.innerText")
                   "Before the panel.\n\nCommunities\n\nAfter the panel."))
    (should (equal (canvas-browser--page-text)
                   (string-join '("Before the panel."
                                  ""
                                  "Communities"
                                  "row Home"
                                  "row Popular"
                                  ""
                                  "A sentence with 7 inside."
                                  ""
                                  "After the panel.")
                                "\n")))))

(ert-deftest canvas-browser-live-the-html-of-a-page-holds-what-its-components-draw ()
  ;; GIVEN the components fixture, with its panel picked
  ;; WHEN a receiver asks for the HTML of the part
  ;; THEN it gets the panel itself, with the children that the page gave
  ;;      it, AND what the panel draws of its own, as HTML writes that: in
  ;;      a template that says it is the inside of a component
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--in-fixture "components.html"
    (canvas-browser-live-test--lay-out 800 600)
    (canvas-browser-live-test--page-says
     "window.__canvasBrowserParts = [document.getElementById('panel')]; true")
    (setq canvas-browser--part '(:place 0 :scrolls nil))
    (let ((html (canvas-browser-content-html '(:kind part))))
      (should (string-prefix-p "<x-panel id=\"panel\">" html))
      (should (string-suffix-p "</x-panel>" html))
      (should (string-search "<span slot=\"title\">Communities</span>" html))
      (should (string-search "<template shadowrootmode=\"open\">" html))
      (should (string-match-p "<x-row><template shadowrootmode=\"open\">.*</template>Home</x-row>"
                              html)))
    (setq canvas-browser--part nil)))

(ert-deftest canvas-browser-live-a-page-that-names-itself-later-gets-that-title ()
  ;; GIVEN the fixture page, which has loaded and given its title
  ;; WHEN the page gives itself another title and draws something, as
  ;;      Reddit names a post while it fills the page
  ;; THEN the buffer takes the new title once the page is quiet
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--in-fixture "page.html"
    (should (canvas-browser-live-test--wait
             10 (lambda () (equal canvas-browser--title "canvas-browser fixture"))))
    (canvas-browser-live-test--page-says
     "document.title = 'A later title'; document.body.append('more words'); true")
    (should (canvas-browser-live-test--wait
             10 (lambda () (equal canvas-browser--title "A later title"))))))

(ert-deftest canvas-browser-live-a-page-with-a-translator-is-read-as-a-thread ()
  ;; GIVEN the thread fixture, AND a translator for its address that
  ;;       says what a comment is and where its author and its text are
  ;; WHEN the text of the page is read as it is sent
  ;; THEN it is the thread: each comment with its author, a reply two
  ;;      spaces further in than what it answers, AND nothing of the
  ;;      rest of the page
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (skip-unless (libxml-available-p))
  (let ((canvas-browser-translators
         (list (cons "fixtures/thread\\.html"
                     (canvas-browser-thread-translator
                      `(x-comment
                        :author ,(lambda (comment)
                                   (car (dom-by-tag (canvas-browser-dom-with comment 'slot "meta")
                                                    'b)))
                        :body (inside slot "body")))))))
    (canvas-browser-live-test--in-fixture "thread.html"
      (should (equal (canvas-browser--text-to-read 'page #'canvas-browser--page-text)
                     (string-join '("alice" "" "What is the role?" ""
                                    "  bob" "" "  The drawing layer." "" "  It uses cairo." ""
                                    "    alice" "" "    I see." ""
                                    "carol" "" "Nice.")
                                  "\n"))))))

(ert-deftest canvas-browser-live-the-page-gives-the-bytes-of-its-pictures ()
  ;; GIVEN the pictures fixture: a picture from a file, and one whose
  ;;       address the page made itself
  ;; WHEN the page is asked for the bytes of each
  ;; THEN the first is the file to the byte, AND the second is a picture
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--in-fixture "pictures.html"
    (should (canvas-browser-live-test--wait
             10 (lambda () (string-prefix-p "blob:" (canvas-browser-live-test--page-says
                                                     "document.getElementById('blob').src")))))
    (let ((file (canvas-browser-live-test--page-says "document.getElementById('file').src"))
          (made (canvas-browser-live-test--page-says "document.getElementById('blob').src")))
      (should (equal (canvas-browser-picture-bytes file)
                     (with-temp-buffer
                       (set-buffer-multibyte nil)
                       (insert-file-contents-literally (expand-file-name "tests/fixtures/plot.png"))
                       (buffer-string))))
      (should (string-prefix-p "\x89PNG" (canvas-browser-picture-bytes made))))))


(defun canvas-browser-live-test--answer (file)
  "What a server answers to a request for FILE, as the text of http."
  (if (file-regular-p file)
      (let ((body (with-temp-buffer
                    (set-buffer-multibyte nil)
                    (insert-file-contents-literally file)
                    (buffer-string))))
        (format "HTTP/1.1 200 OK\r\nContent-Type: %s\r\nContent-Length: %d\r\n\r\n%s"
                (if (string-suffix-p ".js" file) "text/javascript" "text/html")
                (length body) body))
    "HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\n\r\n"))

(defun canvas-browser-live-test--serve (directory)
  "Serve the files of DIRECTORY over http on a port of this machine; the server.
A page from a file has no site that chromium can be told about, so a
test of what goes by site needs a page that a server gave."
  (make-network-process
   :name "canvas-browser-live-test" :server t :host "127.0.0.1" :service t :family 'ipv4
   :coding 'binary :noquery t
   :filter (lambda (connection request)
             (when (string-match "\\`GET /\\([^ ?]*\\)" request)
               (process-send-string
                connection
                (canvas-browser-live-test--answer
                 (expand-file-name (match-string 1 request) directory)))
               (delete-process connection)))))

(defmacro canvas-browser-live-test--serving (origin &rest body)
  "Run BODY while the fixtures are served over http, with ORIGIN as their site."
  (declare (indent 1))
  (let ((server (make-symbol "server")))
    `(let* ((,server (canvas-browser-live-test--serve (expand-file-name "tests/fixtures/")))
            (,origin (format "http://127.0.0.1:%d" (process-contact ,server :service))))
       (unwind-protect
           (progn ,@body)
         (delete-process ,server)))))

(defun canvas-browser-live-test--shown-line (line)
  "Press `M-.\=' on the first line of the text buffer that is LINE.
The answer is where the hit stands in the page: the id of the element
that holds its start, and whether the window shows that element."
  (canvas-browser-text)
  (let ((page (current-buffer)))
    (with-current-buffer (canvas-browser--text-buffer)
      (goto-char (point-min))
      (should (re-search-forward (concat "^" (regexp-quote line) "$") nil t))
      (canvas-browser-text-show-in-page))
    (with-current-buffer page
      (canvas-browser-live-test--wait 0.5 (lambda () nil))
      (canvas-browser-live-test--page-says
       "(() => { const hits = CSS.highlights.get('canvas-browser-find');
                 if (!hits) return null;
                 const range = [...hits][0];
                 const box = range.getBoundingClientRect();
                 return [range.startContainer.parentElement.id,
                         box.top >= 0 && box.bottom <= innerHeight,
                         range.toString()]; })()"))))

(ert-deftest canvas-browser-live-a-line-of-the-text-is-shown-in-the-page ()
  ;; GIVEN the locate fixture: the line Reply twice, the second far down
  ;;       in a part that scrolls on its own; a sentence with a link in
  ;;       it; a line that the page shows in capitals; and a line far
  ;;       down the page
  ;; WHEN M-. is pressed on each of them in the text of the page, on the
  ;;      second Reply
  ;; THEN the page paints that line where it stands, AND the window
  ;;      shows it: the second Reply in the part, the sentence across
  ;;      its link, the capitals, and the line far down
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (cl-letf (((symbol-function 'display-buffer) #'ignore)
            ((symbol-function 'pop-to-buffer) (lambda (buffer &rest _) (set-buffer buffer))))
    (canvas-browser-live-test--in-fixture "locate.html"
      (canvas-browser-live-test--lay-out 800 600)
      (canvas-browser-text)
      (let ((page (current-buffer)))
        (with-current-buffer (canvas-browser--text-buffer)
          (goto-char (point-min))
          (re-search-forward "^Reply$")
          (re-search-forward "^Reply$")
          (canvas-browser-text-show-in-page))
        (with-current-buffer page
          (canvas-browser-live-test--wait 0.5 (lambda () nil))
          (should (equal (canvas-browser-live-test--page-says
                          "[...CSS.highlights.get('canvas-browser-find')][0].startContainer.parentElement.id")
                         "second"))
          (should (> (canvas-browser-live-test--page-says
                      "document.getElementById('pane').scrollTop")
                     0))))
      (should (equal (canvas-browser-live-test--shown-line "A sentence with a link inside it.")
                     '("sentence" t "A sentence with a link inside it.")))
      (should (equal (canvas-browser-live-test--shown-line "LOUD WORDS")
                     '("shout" t "loud words")))
      (should (equal (canvas-browser-live-test--shown-line "The far end.")
                     '("far" t "The far end.")))
      ;; The paint is a blink: a moment later the page is as it was.
      (canvas-browser-live-test--wait 2.5 (lambda () nil))
      (should (equal (canvas-browser-live-test--page-says
                      "CSS.highlights.has('canvas-browser-find') ? 1 : 0")
                     0)))))

(ert-deftest canvas-browser-live-a-line-with-pictures-is-shown-in-the-page ()
  ;; GIVEN the locate fixture, where a message has a picture before its
  ;;       words and one after them, as an application draws an emoji,
  ;;       and a translator that gives the message with the first
  ;;       picture named in brackets and the second as the emoji itself
  ;; WHEN M-. is pressed on the line of the message in the text
  ;; THEN the page paints the words of the message where they stand
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (skip-unless (libxml-available-p))
  (let ((canvas-browser-translators
         (list (cons "locate\\.html"
                     (canvas-browser-thread-translator
                      '((data-kind "item")
                        :author (inside data-kind "name")
                        :body (inside data-kind "message")))))))
    (cl-letf (((symbol-function 'display-buffer) #'ignore)
              ((symbol-function 'pop-to-buffer) (lambda (buffer &rest _) (set-buffer buffer))))
      (canvas-browser-live-test--in-fixture "locate.html"
        (canvas-browser-live-test--lay-out 800 600)
        (canvas-browser-text)
        (let ((page (current-buffer)))
          (with-current-buffer (canvas-browser--text-buffer)
            (goto-char (point-min))
            (should (re-search-forward "^\\[point\\].*the door of the house.*🐻$" nil t))
            (canvas-browser-text-show-in-page))
          (with-current-buffer page
            (canvas-browser-live-test--wait 0.5 (lambda () nil))
            (should (equal (canvas-browser-live-test--page-says
                            "(() => { const range = [...CSS.highlights.get('canvas-browser-find')][0];
                                      return [range.startContainer.parentElement.id, range.toString()]; })()")
                           '("pictures" "the door of the house")))))))))

(ert-deftest canvas-browser-live-a-message-that-starts-with-a-name-is-shown-in-the-page ()
  ;; GIVEN the locate fixture, where a message starts with a name in a
  ;;       block that the page shows inside the line, as an application
  ;;       draws a mention, AND the page holds that name before it, in a
  ;;       part that is no message, AND between two messages the page
  ;;       has a time, which the translator does not give
  ;; WHEN M-. is pressed on the name, which the text has on a line of
  ;;      its own
  ;; THEN the page paints the name where the message has it: it was
  ;;      asked for the name with the words after it, without the
  ;;      message that comes next
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (skip-unless (libxml-available-p))
  (let ((canvas-browser-translators
         (list (cons "locate\\.html"
                     (canvas-browser-thread-translator
                      '((data-kind "item")
                        :author (inside data-kind "name")
                        :body (inside data-kind "message")))))))
    (cl-letf (((symbol-function 'display-buffer) #'ignore)
              ((symbol-function 'pop-to-buffer) (lambda (buffer &rest _) (set-buffer buffer))))
      (canvas-browser-live-test--in-fixture "locate.html"
        (canvas-browser-live-test--lay-out 800 600)
        (canvas-browser-text)
        (let ((page (current-buffer)))
          (with-current-buffer (canvas-browser--text-buffer)
            (goto-char (point-min))
            (should (re-search-forward "^Kim Doe$" nil t))
            (canvas-browser-text-show-in-page))
          (with-current-buffer page
            (canvas-browser-live-test--wait 0.5 (lambda () nil))
            (should (equal (canvas-browser-live-test--page-says
                            "(() => { const range = [...CSS.highlights.get('canvas-browser-find')][0];
                                      return [range.startContainer.parentElement.id, range.toString()]; })()")
                           '("mention" "Kim Doe")))))))))

(ert-deftest canvas-browser-live-two-blocks-side-by-side-are-found-as-the-text-has-them ()
  ;; GIVEN the locate fixture, where a message starts with a name in two
  ;;       blocks that the page shows side by side with no space between
  ;;       them, AND the page holds the first of the two names before
  ;;       it, in a part that is no message
  ;; WHEN M-. is pressed on the first name, which the text has on a line
  ;;      of its own, with the second name on the line after it
  ;; THEN the page paints the name where the message has it: the page
  ;;      compares without regard to white space
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (skip-unless (libxml-available-p))
  (let ((canvas-browser-translators
         (list (cons "locate\\.html"
                     (canvas-browser-thread-translator
                      '((data-kind "item")
                        :author (inside data-kind "name")
                        :body (inside data-kind "message")))))))
    (cl-letf (((symbol-function 'display-buffer) #'ignore)
              ((symbol-function 'pop-to-buffer) (lambda (buffer &rest _) (set-buffer buffer))))
      (canvas-browser-live-test--in-fixture "locate.html"
        (canvas-browser-live-test--lay-out 800 600)
        (canvas-browser-text)
        (let ((page (current-buffer)))
          (with-current-buffer (canvas-browser--text-buffer)
            (goto-char (point-min))
            (should (re-search-forward "^Dee\nFox$" nil t))
            (forward-line -1)
            (canvas-browser-text-show-in-page))
          (with-current-buffer page
            (canvas-browser-live-test--wait 0.5 (lambda () nil))
            (should (equal (canvas-browser-live-test--page-says
                            "(() => { const range = [...CSS.highlights.get('canvas-browser-find')][0];
                                      return [range.startContainer.parentElement.id, range.toString()]; })()")
                           '("given" "Dee")))))))))

(ert-deftest canvas-browser-live-the-links-of-a-page-are-offered-shown-and-followed ()
  ;; GIVEN the links fixture: a link with a text, one that shows a
  ;;       picture, one that the page does not show, one that goes
  ;;       nowhere, the first link again, a link in a web component, and
  ;;       a link far down the page
  ;; WHEN the links are made ready to choose from
  ;; THEN each link that is shown and goes somewhere is offered once, by
  ;;      its text or the name of its picture, with its address
  ;; WHEN the far link is shown, as while the reader moves to it
  ;; THEN the page scrolls to it
  ;; WHEN the link with the picture is followed
  ;; THEN the page is at the address of that link
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--in-fixture "links.html"
    (canvas-browser-live-test--lay-out 800 600)
    (let* ((links (canvas-browser--link-candidates))
           (names (mapcar (lambda (link)
                            (replace-regexp-in-string "file://[^#]*" "" (car link)))
                          links)))
      (should (equal names '("First link  #one" "The logo  #two" "In a component  #inner"
                             "The far link  #far")))
      (canvas-browser--show-link (cdr (nth 3 links)))
      (should (canvas-browser-live-test--wait
               5 (lambda () (> (canvas-browser-live-test--page-says "scrollY") 0))))
      (canvas-browser--follow-link (cdr (nth 1 links)))
      (should (canvas-browser-live-test--wait
               5 (lambda () (equal (canvas-browser-live-test--page-says "location.hash")
                                   "#two")))))))

(defun canvas-browser-live-test--scrolls ()
  "How far the locate fixture is scrolled: the page, and its part that scrolls."
  (canvas-browser-live-test--page-says
   "[Math.round(scrollY), Math.round(document.getElementById('pane').scrollTop)]"))

(defun canvas-browser-live-test--scroll-to (page pane)
  "Scroll the locate fixture to PAGE pixels, and its part that scrolls to PANE."
  (canvas-browser-live-test--page-says
   (format "(scrollTo(0, %d), document.getElementById('pane').scrollTo(0, %d), true)"
           page pane)))

(ert-deftest canvas-browser-live-a-mark-brings-the-page-and-its-parts-back ()
  ;; GIVEN the locate fixture, a long page with a part that scrolls on
  ;;       its own, with the page scrolled to 500 and the part to 100
  ;; WHEN the mark is set, both are scrolled elsewhere, and the mark is
  ;;      gone back to
  ;; THEN the page and the part are where they were
  ;; WHEN the position is kept in a register, the page is read again,
  ;;      and the register is jumped to
  ;; THEN the page is scrolled to 500 again: a page that has loaded anew
  ;;      no longer knows how far its part was scrolled
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--in-fixture "locate.html"
    (canvas-browser-live-test--lay-out 800 600)
    (let ((register-alist nil))
      (canvas-browser-live-test--scroll-to 500 100)
      (should (equal (canvas-browser-live-test--scrolls) '(500 100)))
      (canvas-browser-set-mark)
      (should (canvas-browser-live-test--wait 5 (lambda () canvas-browser--mark-ring)))
      (canvas-browser-position-to-register ?a)
      (should (canvas-browser-live-test--wait 5 (lambda () (get-register ?a))))
      (canvas-browser-live-test--scroll-to 1200 0)
      (should (equal (canvas-browser-live-test--scrolls) '(1200 0)))
      (canvas-browser-pop-mark)
      (should (canvas-browser-live-test--wait
               5 (lambda () (equal (canvas-browser-live-test--scrolls) '(500 100)))))
      (canvas-browser-refresh)
      ;; The page that has loaded anew has its part at the start again.
      ;; Chromium itself may put the page back where it was.
      (should (canvas-browser-live-test--wait
               10 (lambda () (equal (cadr (canvas-browser-live-test--scrolls)) 0))))
      (canvas-browser-live-test--scroll-to 1200 0)
      (should (equal (canvas-browser-live-test--scrolls) '(1200 0)))
      (jump-to-register ?a)
      (should (canvas-browser-live-test--wait
               5 (lambda () (equal (canvas-browser-live-test--scrolls) '(500 0))))))))

(ert-deftest canvas-browser-live-an-address-that-names-a-text-opens-the-page-at-it ()
  ;; GIVEN a page buffer at some page, AND the address of the locate
  ;;       fixture with the part that names its last line, which stands
  ;;       far down, as a link that org stored from marked text has it
  ;; WHEN the page buffer goes to that address
  ;; THEN the page is scrolled down to the line
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--in-fixture "page.html"
    (canvas-browser-live-test--lay-out 800 600)
    (canvas-browser-open-url
     (canvas-browser--with-text-fragment
      (concat "file://" (expand-file-name "locate.html" "tests/fixtures/"))
      "The far end."))
    (should (canvas-browser-live-test--wait
             10 (lambda ()
                  (and (equal (canvas-browser-live-test--page-says "document.title")
                              "canvas-browser locate fixture")
                       (> (canvas-browser-live-test--page-says "scrollY") 1000)))))))

(ert-deftest canvas-browser-live-a-line-of-another-page-is-found-and-shown ()
  ;; GIVEN two page buffers: one at the locate fixture, where the test
  ;;       stands, and one at the outline fixture, whose last heading is
  ;;       far down the page
  ;; WHEN the lines of every page are made ready to choose from
  ;; THEN the lines of both pages are there, each under the title of
  ;;      its page
  ;; WHEN the line of that last heading is shown
  ;; THEN the outline page scrolls to it, AND the locate page stays
  ;;      where it is
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (cl-letf (((symbol-function 'pop-to-buffer) (lambda (buffer &rest _) buffer)))
    (canvas-browser-live-test--in-fixture "locate.html"
      (canvas-browser-live-test--lay-out 800 600)
      (let* ((address (concat "file://" (expand-file-name "outline.html" "tests/fixtures/")))
             (other (canvas-browser address)))
        (unwind-protect
            (progn
              (with-current-buffer other
                (should (canvas-browser-live-test--wait
                         30 (lambda () (and canvas-browser--session canvas-browser--title
                                            (canvas-browser-live-test--loaded-p address))))))
              (let* ((hits (canvas-browser--pages-hits))
                     (groups (delete-dups
                              (mapcar (lambda (hit) (canvas-browser--hit-group (car hit) nil))
                                      hits)))
                     (end (seq-find (lambda (hit) (string-prefix-p "The end" (car hit))) hits)))
                (should (equal groups '("canvas-browser locate fixture"
                                        "canvas-browser outline fixture")))
                (should (eq (cadr end) other))
                (canvas-browser--show-hit (cdr end))
                (with-current-buffer other
                  (should (canvas-browser-live-test--wait
                           5 (lambda () (> (canvas-browser-live-test--page-says "scrollY") 0)))))
                (should (equal (canvas-browser-live-test--page-says "scrollY") 0))))
          (dolist (name '("*canvas-browser-text: canvas-browser locate fixture*"
                          "*canvas-browser-text: canvas-browser outline fixture*"))
            (when (get-buffer name) (kill-buffer name)))
          (kill-buffer other))))))

(defun canvas-browser-live-test--field-buffers ()
  "The buffers that hold the text of a field of a page."
  (seq-filter (lambda (buffer) (buffer-local-value 'canvas-browser-edit-field-mode buffer))
              (buffer-list)))

(defun canvas-browser-live-test--edited-field ()
  "The buffer of a field that is open to be written in, or nil."
  (seq-find (lambda (buffer)
              (eq (buffer-local-value 'canvas-browser--field-state buffer) 'edit))
            (canvas-browser-live-test--field-buffers)))

(defun canvas-browser-live-test--edit-field (element)
  "Give the focus to what the JavaScript ELEMENT names, and open it in a buffer.
Give the buffer once the page has answered, or nil when none opens."
  (canvas-browser-live-test--page-says (format "(%s).focus()" element))
  (canvas-browser-edit-field)
  (canvas-browser-live-test--wait 2 #'canvas-browser-live-test--edited-field)
  (canvas-browser-live-test--edited-field))

(defun canvas-browser-live-test--finish-field (field text)
  "End the edit in FIELD, the buffer of a field, with TEXT as its text.
With nil for TEXT, the text stays as the buffer has it."
  (with-current-buffer field
    (when text
      (erase-buffer)
      (insert text))
    (canvas-browser-edit-field-finish))
  (should (canvas-browser-live-test--wait
           10 (lambda () (not (canvas-browser-live-test--edited-field))))))

(defmacro canvas-browser-live-test--in-field-fixture (&rest body)
  "Run BODY in a page buffer of the field fixture, with no window changed."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
     (canvas-browser-live-test--in-fixture "field.html"
       (canvas-browser-live-test--lay-out 800 600)
       (unwind-protect (progn ,@body)
         (mapc #'kill-buffer (canvas-browser-live-test--field-buffers))))))

(defun canvas-browser-live-test--step-back ()
  "Stand in for a view of the undo history: take one step back in this buffer."
  (interactive)
  (let ((last-command nil))
    (undo)))

(ert-deftest canvas-browser-live-a-step-in-the-history-of-a-field-shows-in-the-page ()
  ;; GIVEN the field fixture, with the focus in its text area, which
  ;;       holds two lines, AND a package that shows the undo history of
  ;;       a buffer on C-x u, here one that takes a step back at once
  ;; WHEN the field is read, as it is when the keys pause, AND more is
  ;;      typed into the page, AND the field is read again
  ;; THEN the buffer of the field holds what the area holds now
  ;; WHEN the history of the field is asked for in the page buffer
  ;; THEN the step back is typed into the page: the area holds its two
  ;;      lines again
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (let ((before (lookup-key text-mode-map (kbd "C-x u"))))
    (unwind-protect
        (canvas-browser-live-test--in-field-fixture
          (define-key text-mode-map (kbd "C-x u") #'canvas-browser-live-test--step-back)
          (canvas-browser-live-test--page-says "document.getElementById('area').focus()")
          (canvas-browser--field-read (current-buffer))
          (should (canvas-browser-live-test--wait 5 #'canvas-browser-live-test--field-buffers))
          (let ((field (car (canvas-browser-live-test--field-buffers))))
            (should (equal (with-current-buffer field (buffer-string)) "one\ntwo"))
            (canvas-browser--tell "Input.insertText" (list :text " and more"))
            (should (canvas-browser-live-test--wait
                     5 (lambda () (string-search "and more"
                                                 (canvas-browser-live-test--value "area")))))
            (canvas-browser--field-read (current-buffer))
            (should (canvas-browser-live-test--wait
                     5 (lambda () (string-search "and more"
                                                 (with-current-buffer field (buffer-string))))))
            (canvas-browser-field-history)
            (should (canvas-browser-live-test--wait
                     5 (lambda () (equal (canvas-browser-live-test--value "area") "one\ntwo"))))
            (should (equal (canvas-browser-live-test--page-says "window.keys.length") 0))))
      (define-key text-mode-map (kbd "C-x u") (and (not (numberp before)) before)))))

(ert-deftest canvas-browser-live-a-text-area-is-edited-in-a-buffer ()
  ;; GIVEN the field fixture, where a text area holds two lines
  ;; WHEN the area has the focus, and its text is opened in a buffer
  ;; THEN the buffer holds the two lines
  ;; WHEN the text is changed in the buffer, and the edit is ended
  ;; THEN the area holds the new text with its line breaks, AND the page
  ;;      was told of no key, so a page that sends on Enter sends nothing
  ;; WHEN the undo key of the field is pressed in the page buffer
  ;; THEN the area holds its two lines again
  ;; WHEN the redo key is pressed
  ;; THEN the area holds the new text again
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--in-field-fixture
    (let ((field (canvas-browser-live-test--edit-field "document.getElementById('area')")))
      (should field)
      (should (equal (with-current-buffer field (buffer-string)) "one\ntwo"))
      (canvas-browser-live-test--finish-field field "alpha\nbeta\n\ngamma")
      (should (canvas-browser-live-test--wait
               5 (lambda () (equal (canvas-browser-live-test--value "area")
                                   "alpha\nbeta\n\ngamma"))))
      (should (equal (canvas-browser-live-test--page-says "window.keys.length") 0))
      (canvas-browser-field-undo)
      (should (canvas-browser-live-test--wait
               5 (lambda () (equal (canvas-browser-live-test--value "area") "one\ntwo"))))
      (canvas-browser-field-redo)
      (should (canvas-browser-live-test--wait
               5 (lambda () (equal (canvas-browser-live-test--value "area")
                                   "alpha\nbeta\n\ngamma")))))))

(ert-deftest canvas-browser-live-an-editor-keeps-its-lines-through-a-buffer ()
  ;; GIVEN the field fixture, where an editor that the page built holds
  ;;       three paragraphs, the second one empty
  ;; WHEN the editor has the focus, and its text is opened in a buffer
  ;; THEN the buffer holds a line for each paragraph: an empty line for
  ;;      the empty one, and no more empty lines than that
  ;; WHEN the edit is ended with one more line
  ;; THEN the editor holds a paragraph for each line of the buffer
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--in-field-fixture
    (let ((field (canvas-browser-live-test--edit-field "document.getElementById('editor')")))
      (should field)
      (should (equal (with-current-buffer field (buffer-string)) "first\n\nthird"))
      (canvas-browser-live-test--finish-field field "first\n\nthird\nfourth")
      (should (canvas-browser-live-test--wait
               5 (lambda ()
                   (equal (canvas-browser-live-test--page-says
                           "[...document.querySelectorAll('#editor > p')].map(p => p.textContent)")
                          '("first" "" "third" "fourth")))))
      (should (equal (canvas-browser-live-test--page-says "window.keys.length") 0)))))

(ert-deftest canvas-browser-live-a-field-in-a-web-component-is-edited-in-a-buffer ()
  ;; GIVEN the field fixture, where a text area stands in the shadow
  ;;       root of a web component
  ;; WHEN that area has the focus, its text is opened in a buffer, and
  ;;      the edit is ended with another text
  ;; THEN the buffer held the text of the area, AND the area holds the
  ;;      other text
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--in-field-fixture
    (let* ((inner "document.getElementById('shadowed').shadowRoot.getElementById('inner')")
           (field (canvas-browser-live-test--edit-field inner)))
      (should field)
      (should (equal (with-current-buffer field (buffer-string)) "in the shadow"))
      (canvas-browser-live-test--finish-field field "out of it")
      (should (canvas-browser-live-test--wait
               5 (lambda () (equal (canvas-browser-live-test--page-says (concat inner ".value"))
                                   "out of it")))))))

(ert-deftest canvas-browser-live-a-password-field-opens-no-buffer ()
  ;; GIVEN the field fixture, with the focus in its field for a password
  ;; WHEN the field is asked for in a buffer
  ;; THEN no buffer opens
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--in-field-fixture
    (should-not (canvas-browser-live-test--edit-field "document.getElementById('secret')"))))

(defun canvas-browser-live-test--hits ()
  "What the page paints of a search: how many hits, and where the one shown is.
It is (COUNT ID TEXT): the id of the element that holds the start of
the hit that is shown, and the text of that hit.  The page is given a
moment to answer the search first."
  (canvas-browser-live-test--wait 1 (lambda () nil))
  (canvas-browser-live-test--page-says
   "(() => { const all = CSS.highlights.get('canvas-browser-find');
             const current = CSS.highlights.get('canvas-browser-find-current');
             if (!all || !current) return [0, null, null];
             const range = [...current][0];
             return [all.size, range.startContainer.parentElement.id, range.toString()]; })()"))

(defun canvas-browser-live-test--pane-top ()
  "How far the part of the locate fixture that scrolls is scrolled."
  (canvas-browser-live-test--page-says "document.getElementById('pane').scrollTop"))

(ert-deftest canvas-browser-live-a-search-steps-through-the-hits-of-a-page ()
  ;; GIVEN the locate fixture, where Reply stands twice, the second time
  ;;       far down in a part that scrolls on its own
  ;; WHEN the page is searched for it, and the search steps on twice
  ;; THEN both hits are painted and the first is shown, then the second,
  ;;      for which the part scrolls, then the first again
  ;; WHEN the search is given up
  ;; THEN the part is back where it was, AND nothing is painted
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--in-fixture "locate.html"
    (canvas-browser-live-test--lay-out 800 600)
    (canvas-browser-find "Reply")
    (should (equal (canvas-browser-live-test--hits) '(2 "first" "Reply")))
    ;; The page says where the hit stands, for the eye to be drawn there.
    (should (> (plist-get canvas-browser--find-box :w) 0))
    (should (< (plist-get canvas-browser--find-box :y) 600))
    (canvas-browser-find-next)
    (should (equal (canvas-browser-live-test--hits) '(2 "second" "Reply")))
    (should (> (canvas-browser-live-test--pane-top) 0))
    (canvas-browser-find-next)
    (should (equal (canvas-browser-live-test--hits) '(2 "first" "Reply")))
    (canvas-browser-find-previous)
    (should (equal (canvas-browser-live-test--hits) '(2 "second" "Reply")))
    (canvas-browser--find-end "Reply" t)
    (should (equal (car (canvas-browser-live-test--hits)) 0))
    (should (= (canvas-browser-live-test--pane-top) 0))))

(ert-deftest canvas-browser-live-a-search-follows-what-is-typed ()
  ;; GIVEN the locate fixture
  ;; WHEN a search is typed letter by letter
  ;; THEN each longer text is searched for from the hit before, AND a
  ;;      text that crosses a link is found in one piece
  ;; WHEN the text has a capital letter
  ;; THEN the search is exact about case, AND it finds the text that the
  ;;      page shows in capitals
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--in-fixture "locate.html"
    (canvas-browser-live-test--lay-out 800 600)
    (canvas-browser--find-typed "fil")
    (should (equal (car (canvas-browser-live-test--hits)) 20))
    (canvas-browser--find-typed "filler 2")
    (should (equal (canvas-browser-live-test--hits) '(2 "" "filler 2")))
    (setq canvas-browser--find-anchor nil)
    (canvas-browser--find-typed "with a link inside")
    (should (equal (canvas-browser-live-test--hits) '(1 "sentence" "with a link inside")))
    (canvas-browser--find-typed "reply")
    (should (equal (car (canvas-browser-live-test--hits)) 2))
    (canvas-browser--find-typed "REPLY")
    (should (equal (car (canvas-browser-live-test--hits)) 0))
    (canvas-browser--find-typed "LOUD")
    (should (equal (canvas-browser-live-test--hits) '(1 "shout" "loud")))))

(ert-deftest canvas-browser-live-a-search-for-a-regexp ()
  ;; GIVEN the locate fixture, with twenty lines of filler
  ;; WHEN the page is searched for a regexp that names ten of them
  ;; THEN ten hits are painted
  ;; WHEN the regexp is not finished
  ;; THEN the search says what is wrong, AND the page is as it was
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--in-fixture "locate.html"
    (canvas-browser-live-test--lay-out 800 600)
    (setq canvas-browser--find-regexp t)
    (canvas-browser-find "filler 1\\d")
    (should (equal (canvas-browser-live-test--hits) '(10 "" "filler 10")))
    (let ((said nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq said (and format-string (apply #'format format-string args))))))
        (canvas-browser-find "filler (")
        (canvas-browser-live-test--wait 1 (lambda () nil)))
      (should (string-search "regexp, " said))
      (should (string-search "nterminated" said)))
    (should (equal (car (canvas-browser-live-test--hits)) 10))))

(ert-deftest canvas-browser-live-the-index-of-a-page-is-its-outline ()
  ;; GIVEN the outline fixture: a heading, under it three headings, one
  ;;       of them marked as a heading by its role, a heading under the
  ;;       first of the three, and a heading that the page does not show
  ;; WHEN imenu asks the page buffer for its index
  ;; THEN the index is the outline of what the page shows
  ;; WHEN the last heading, far down the page, is picked
  ;; THEN the page scrolls to it and paints it, AND a moment later the
  ;;      paint is gone
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--in-fixture "outline.html"
    (canvas-browser-live-test--lay-out 800 600)
    (let* ((index (funcall imenu-create-index-function))
           (guide (car index))
           (names (lambda (entries) (mapcar #'car entries))))
      (should (equal (funcall names index) '("Guide")))
      ;; A heading that is there for a screen reader only is none.
      (should (equal (funcall names (cdr guide)) '("Guide" "Install" "Use" "The end")))
      (should (equal (funcall names (cdr (nth 2 guide))) '("Install" "From source")))
      ;; The headings of two columns come column by column, as the page
      ;; has them, and not by how high each stands in the window.
      (should (equal (funcall names (cdr (nth 3 guide)))
                     '("Use" "Left" "Left and low" "Right")))
      (pcase-let ((`(,name ,position ,function . ,arguments) (car (last guide))))
        (apply function name position arguments))
      (canvas-browser-live-test--wait 0.5 (lambda () nil))
      (should (> (canvas-browser-live-test--page-says "scrollY") 0))
      (should (equal (canvas-browser-live-test--page-says
                      "[...CSS.highlights.get('canvas-browser-find')][0].startContainer.id")
                     "end"))
      (should (plist-get canvas-browser--find-box :w))
      (canvas-browser-live-test--wait 2.5 (lambda () nil))
      (should (equal (canvas-browser-live-test--page-says
                      "CSS.highlights.has('canvas-browser-find') ? 1 : 0")
                     0)))))

(ert-deftest canvas-browser-live-a-choice-from-the-index-that-is-given-up-goes-back ()
  ;; GIVEN the outline fixture, at its top, where the reader chooses
  ;;       from the index and moves to the last heading, far down
  ;; WHEN the page shows that heading
  ;; THEN it has scrolled there
  ;; WHEN the reader gives up
  ;; THEN the page is back at its top, AND nothing is painted
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--in-fixture "outline.html"
    (canvas-browser-live-test--lay-out 800 600)
    (let* ((entries (canvas-browser--flat-index (funcall imenu-create-index-function)))
           (state (canvas-browser--preview-state #'canvas-browser--pick-entry)))
      (should (equal (mapcar #'car entries)
                     '("Guide" "Guide/Install" "Guide/Install/From source" "Guide/Use"
                       "Guide/Use/Left" "Guide/Use/Left and low" "Guide/Use/Right"
                       "Guide/The end")))
      (funcall state 'setup nil)
      ;; consult shows the first entry when the index opens.
      (funcall state 'preview (car entries))
      (funcall state 'preview (car (last entries)))
      (canvas-browser-live-test--wait 0.5 (lambda () nil))
      (should (> (canvas-browser-live-test--page-says "scrollY") 0))
      (funcall state 'return nil)
      (canvas-browser-live-test--wait 0.5 (lambda () nil))
      (should (equal (canvas-browser-live-test--page-says "scrollY") 0))
      (should (equal (canvas-browser-live-test--page-says
                      "CSS.highlights.has('canvas-browser-find') ? 1 : 0")
                     0)))))

(defun canvas-browser-live-test--click-on (id)
  "Click the element ID of this page."
  (let ((middle (canvas-browser-live-test--middle id)))
    (should middle)
    (canvas-browser--click (car middle) (cdr middle))))

(ert-deftest canvas-browser-live-what-a-page-copies-goes-to-the-kill-ring ()
  ;; GIVEN the clipboard fixture, served as a site: a button that copies
  ;;       a text as pages do now, one that copies as pages did before,
  ;;       and one that copies nothing
  ;; WHEN each is clicked
  ;; THEN the text of the first is the newest kill, then the text of the
  ;;      second, AND the third leaves the kill ring as it is
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--serving origin
    (canvas-browser-live-test--at (concat origin "/clipboard.html")
      (canvas-browser-live-test--lay-out 800 600)
      (let ((kill-ring nil)
            (kill-ring-yank-pointer nil)
            (interprogram-cut-function nil)
            (canvas-browser--clipboard nil))
        (canvas-browser-live-test--click-on "modern")
        (should (canvas-browser-live-test--wait
                 5 (lambda () (equal (car kill-ring) "copied the new way"))))
        (canvas-browser-live-test--click-on "old")
        (should (canvas-browser-live-test--wait
                 5 (lambda () (equal (car kill-ring) "copied the old way"))))
        (canvas-browser-live-test--click-on "nothing")
        (canvas-browser-live-test--wait 2 (lambda () nil))
        (should (equal kill-ring '("copied the old way" "copied the new way")))))))

(defun canvas-browser-live-test--said-p (words)
  "Whether the notify fixture has heard WORDS so far, within ten seconds."
  (canvas-browser-live-test--wait
   10 (lambda () (equal (canvas-browser-live-test--page-says "said.join(' ')") words))))

(ert-deftest canvas-browser-live-a-notification-of-a-page-reaches-the-hook ()
  ;; GIVEN the notify fixture, served as a site that is kept as one that
  ;;       may show notifications
  ;; WHEN the page asks whether it may show notifications, and shows one
  ;; THEN the page hears that it may, and that the notification was
  ;;      shown, AND the hook gets the notification with its title, its
  ;;      body, its site and the page buffer
  ;; WHEN the notification is opened
  ;; THEN the page hears that it was clicked
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--serving origin
    (let* ((got nil)
           (canvas-browser-notification-sites (list (cons origin t)))
           (canvas-browser-notification-functions
            (list (lambda (notification) (setq got notification)))))
      (canvas-browser-live-test--at (concat origin "/notify.html")
        (canvas-browser-live-test--page-says "notify(); true")
        (should (canvas-browser-live-test--wait 10 (lambda () got)))
        (should (equal (plist-get got :title) "New mail"))
        (should (equal (plist-get got :body) "From alice"))
        (should (equal (plist-get got :tag) "mail-1"))
        (should (equal (plist-get got :origin) origin))
        (should (eq (plist-get got :buffer) (current-buffer)))
        (should (canvas-browser-live-test--said-p "granted shown"))
        (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
          (canvas-browser-notification-open got))
        (should (canvas-browser-live-test--said-p "granted shown clicked"))))))

(ert-deftest canvas-browser-live-a-notification-that-its-page-closed-can-still-be-opened ()
  ;; GIVEN the notify fixture, served as a site that may show
  ;;       notifications
  ;; WHEN the page shows a notification and closes it at once, twice, as
  ;;      a chat site closes one when its own toast ends
  ;; THEN the hook has the notification, AND the page hears that it was
  ;;      shown, and once that it was closed
  ;; WHEN the notification is opened afterwards
  ;; THEN the page hears that it was clicked.  Emacs keeps a notification
  ;;      for the reader after its site took it back, so the click must
  ;;      still find what the site listens on
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--serving origin
    (let* ((got nil)
           (canvas-browser-notification-sites (list (cons origin t)))
           (canvas-browser-notification-functions
            (list (lambda (notification) (setq got notification)))))
      (canvas-browser-live-test--at (concat origin "/notify.html")
        (canvas-browser-live-test--page-says "notifyBriefly(); true")
        (should (canvas-browser-live-test--wait 10 (lambda () got)))
        (should (equal (plist-get got :title) "New message"))
        (should (canvas-browser-live-test--said-p "granted shown closed"))
        (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
          (canvas-browser-notification-open got))
        (should (canvas-browser-live-test--said-p "granted shown closed clicked"))))))

(ert-deftest canvas-browser-live-a-site-is-left-alone-until-the-reader-allows-it ()
  ;; GIVEN the notify fixture, served as a site that nothing is kept about
  ;; THEN the page has the notifications of chromium itself, and no name
  ;;      to call Emacs by: nothing was put into it
  ;; WHEN the reader says in the page buffer that the site may show
  ;;      notifications, and the page then asks and shows one
  ;; THEN the page hears that it may, AND its notification reaches the
  ;;      hook, without the page being loaded again
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--serving origin
    (let* ((got nil)
           (canvas-browser-notification-sites nil)
           (canvas-browser-notification-functions
            (list (lambda (notification) (setq got notification)))))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'customize-save-variable) #'ignore))
        (canvas-browser-live-test--at (concat origin "/notify.html")
          (should (equal (canvas-browser-live-test--page-says
                          (concat "[Notification.name, typeof window.canvasBrowserNotify,"
                                  " typeof window.__canvasBrowserNotifications].join(' ')"))
                         "Notification undefined undefined"))
          (canvas-browser-notifications)
          (should (canvas-browser-live-test--wait
                   10 (lambda () (equal (canvas-browser-live-test--page-says
                                         "typeof window.__canvasBrowserNotifications")
                                        "object"))))
          (canvas-browser-live-test--page-says "notify(); true")
          (should (canvas-browser-live-test--said-p "granted shown"))
          (should (canvas-browser-live-test--wait 10 (lambda () got)))
          (should (equal (plist-get got :title) "New mail")))))))

(ert-deftest canvas-browser-live-a-page-that-is-put-away-is-looked-at-no-more ()
  ;; GIVEN the fixture page, which was told that it has the focus
  ;; THEN the page says that it has the focus and that it is visible
  ;; WHEN the page was clicked, and no window shows it any more
  ;; THEN the page says that it has no focus and is hidden, so a site
  ;;      that holds its notifications back while you look at it lets
  ;;      them out.  A page that was clicked kept the focus of chromium
  ;;      when another tab was only put in front of it
  ;; WHEN the page is looked at again, clicked again and put away again,
  ;;      now that the empty tab is there already
  ;; THEN it says the same
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (let ((canvas-browser--rest-target nil))
    (canvas-browser-live-test--in-fixture "page.html"
      (canvas-browser-live-test--lay-out 800 600)
      (let ((says-p (lambda (wanted)
                      (canvas-browser-live-test--wait
                       5 (lambda () (equal (canvas-browser-live-test--page-says
                                            "document.hasFocus() + ' ' + document.visibilityState")
                                           wanted))))))
        (dotimes (_ 2)
          (should (funcall says-p "true visible"))
          (canvas-browser--click 100 100)
          (canvas-browser--sleep)
          (should (funcall says-p "false hidden"))
          (canvas-browser--wake t))
        (should (funcall says-p "true visible"))))))

(declare-function dbus-get-name-owner "dbus" (bus service))

(defun canvas-browser-live-test--bus-free-p ()
  "Whether this Emacs can be the notification service of the session now."
  (and (featurep 'dbusbind)
       (require 'dbus nil t)
       (ignore-errors (not (dbus-get-name-owner :session "org.freedesktop.Notifications")))))

(defun canvas-browser-live-test--wait-on-bus (seconds test)
  "Wait up to SECONDS until TEST answers, and hear the session bus meanwhile.
What comes over the bus is an event that Emacs reads as it reads a key."
  (let ((deadline (+ (float-time) seconds)))
    (while (and (not (funcall test)) (< (float-time) deadline))
      (accept-process-output nil 0.05)
      (read-event nil nil 0.05))
    (funcall test)))

(ert-deftest canvas-browser-live-a-notification-of-a-service-worker-reaches-the-hook ()
  ;; GIVEN a session with no notification service, AND the option to be
  ;;       that service, AND the worker fixture, served as a site that
  ;;       may show notifications
  ;; WHEN the service worker of the page shows a notification, which no
  ;;      script in the page sees
  ;; THEN chromium hands it to Emacs, AND the hook gets it with its
  ;;      title, its text, its site and the page buffer of that site
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (skip-unless (canvas-browser-live-test--bus-free-p))
  (let ((canvas-browser-notification-service t)
        (canvas-browser--dbus-served nil)
        (got nil))
    (unwind-protect
        (canvas-browser-live-test--serving origin
          (let ((canvas-browser-notification-sites (list (cons origin t)))
                (canvas-browser-notification-functions
                 (list (lambda (notification) (push notification got)))))
            (canvas-browser-live-test--at (concat origin "/worker.html")
              (canvas-browser-live-test--page-says "fromWorker(); true")
              (should (canvas-browser-live-test--wait-on-bus 20 (lambda () got)))
              (should (equal (plist-get (car got) :title) "New mail"))
              (should (equal (plist-get (car got) :body) "From alice"))
              (should (equal (concat "http://" (plist-get (car got) :source)) origin))
              (should (eq (plist-get (car got) :buffer) (current-buffer))))))
      (dbus-unregister-service :session "org.freedesktop.Notifications"))))

