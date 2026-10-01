;;; canvas-browser-live-tests.el --- one test with a real chromium -*- lexical-binding: t -*-
(require 'ert)
(require 'cl-lib)
(require 'canvas-browser)

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
  ;;      second paragraph is the first line, as with C-k in a buffer
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (canvas-browser-live-test--in-editor
    (canvas-browser-live-test--press "C-a")
    (canvas-browser-live-test--settle)
    (canvas-browser-live-test--press "C-k")
    (should (canvas-browser-live-test--wait
             5 (lambda () (equal (canvas-browser-live-test--editor-lines) '("" "second paragraph")))))
    (canvas-browser-live-test--press "C-k")
    (should (canvas-browser-live-test--wait
             5 (lambda () (equal (canvas-browser-live-test--editor-lines) '("second paragraph")))))))

(ert-deftest canvas-browser-live-c-k-deletes-the-rest-of-a-plain-field ()
  ;; GIVEN the plain field of the typing fixture, holding "hello world",
  ;;       with the cursor after "hello"
  ;; WHEN C-k is pressed
  ;; THEN the field holds "hello"
  (skip-unless (cl-some #'executable-find canvas-browser-chromium))
  (let ((buffer nil)
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
                     5 (lambda () (equal (canvas-browser-live-test--value "plain") "hello"))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (canvas-browser-cdp-stop)
      (delete-directory canvas-browser-profile-directory t))))
