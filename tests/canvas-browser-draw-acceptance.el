;;; canvas-browser-draw-acceptance.el --- the mouse held down on a page -*- lexical-binding: t -*-

;; `make draw-acceptance' runs these in an Emacs on a virtual display,
;; with a real chromium.  Each test shows a page buffer in the only
;; window of the frame, and moves the real pointer of the display with
;; tests/gui/xtest.py, as a user moves the mouse.  The command loop of
;; Emacs reads each event and finds its command.  What the page shows
;; is read from chromium: a small picture of the page around a point,
;; or the text that the page marks.
(require 'ert)
(require 'cl-lib)
(require 'canvas-browser)
(load "canvas-browser-live-tests" nil t)

(defvar canvas-browser-draw-acceptance--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "The directory of the tests.")

(defun canvas-browser-draw-acceptance--fixture (name)
  "The file:// address of the fixture NAME."
  (concat "file://" (expand-file-name (concat "fixtures/" name) canvas-browser-draw-acceptance--dir)))

;;;; The page

(defmacro canvas-browser-draw-acceptance--in-page (name &rest body)
  "Run BODY in a page buffer of the fixture NAME, which the only window
of the frame shows, once the page has drawn itself.  Chromium has a
profile of its own."
  (declare (indent 1))
  `(let ((buffer nil)
         (canvas-browser-profile-directory
          (let ((temporary-file-directory (expand-file-name "~/snap/chromium/common/")))
            (make-temp-file "canvas-browser-draw-" t))))
     (unwind-protect
         (progn
           (delete-other-windows)
           (setq buffer (let ((display-buffer-overriding-action '(display-buffer-same-window)))
                          (canvas-browser (canvas-browser-draw-acceptance--fixture ,name))))
           (with-current-buffer buffer
             (should (canvas-browser-live-test--wait 30 (lambda () canvas-browser--session)))
             (should (canvas-browser-live-test--wait 30 (lambda () (> canvas-browser--frames 0))))
             ,@body))
       (when (buffer-live-p buffer) (kill-buffer buffer))
       (canvas-browser-cdp-stop)
       (delete-directory canvas-browser-profile-directory t))))

(defun canvas-browser-draw-acceptance--page-has (script)
  "The value of SCRIPT in this page, once it has one; an error after ten seconds."
  (let ((value nil))
    (should (canvas-browser-live-test--wait
             10 (lambda () (setq value (canvas-browser-live-test--page-says script)))))
    value))

;;;; The mouse

(defconst canvas-browser-draw-acceptance--driver
  (expand-file-name "gui/xtest.py" canvas-browser-draw-acceptance--dir)
  "The script that sends real mouse events to the display.")

(defun canvas-browser-draw-acceptance--on-screen (point)
  "The pixel of the screen that shows POINT, (X . Y) of the page, as X,Y."
  (let ((edges (window-absolute-body-pixel-edges (get-buffer-window (current-buffer)))))
    (format "%d,%d"
            (+ (nth 0 edges) (round (* canvas-browser--zoom (car point))))
            (+ (nth 1 edges) (round (* canvas-browser--zoom (cdr point)))))))

(defun canvas-browser-draw-acceptance--to (verb point)
  "The step of xtest.py that moves the pointer to POINT of the page by VERB."
  (format "%s:%s" verb (canvas-browser-draw-acceptance--on-screen point)))

(defun canvas-browser-draw-acceptance--off-the-frame (point)
  "The step of xtest.py that glides the pointer from POINT out of the
frame, to the right of it, at the height of POINT."
  (let ((y (string-to-number (cadr (split-string (canvas-browser-draw-acceptance--on-screen point) ",")))))
    (format "glide:%d,%d" (+ (nth 2 (frame-edges nil 'outer-edges)) 40) y)))

(defun canvas-browser-draw-acceptance--act (steps &optional on-word)
  "Move the real mouse by STEPS, see xtest.py, while the command loop of
Emacs reads the events and runs their commands.  ON-WORD is called in
this buffer with each word that a say: step prints, at once, while the
button may still be held."
  (let* ((buffer (current-buffer))
         (ended nil)
         (process (make-process
                   :name "xtest" :connection-type 'pipe :noquery t
                   :command (append (list "python3" canvas-browser-draw-acceptance--driver) steps)
                   :stderr (get-buffer-create "*xtest errors*")
                   :filter (lambda (_process text)
                             (dolist (word (split-string text "\n" t))
                               (when (and on-word (buffer-live-p buffer))
                                 (with-current-buffer buffer (funcall on-word word)))))
                   :sentinel (lambda (process _event)
                               (unless (process-live-p process) (setq ended (float-time))))))
         ;; The command loop leaves only once the steps have ended, the
         ;; events have come in, and no command follows the mouse.
         (timer (run-with-timer 0.1 0.1 (lambda ()
                                          (when (and ended (> (float-time) (+ ended 0.5))
                                                     (not track-mouse))
                                            (exit-recursive-edit))))))
    ;; The command loop makes the buffer of the selected window the
    ;; current one, so the test comes back to the page buffer after it.
    (unwind-protect
        (save-current-buffer
          (select-window (get-buffer-window buffer))
          (with-timeout (60 (error "canvas-browser: the steps of the mouse did not end in a minute"))
            (recursive-edit)))
      (cancel-timer timer))
    (should (zerop (process-exit-status process)))))

(defun canvas-browser-draw-acceptance--click (point)
  "Click POINT of the page with the real mouse, without a move between."
  (canvas-browser-draw-acceptance--act
   (list (canvas-browser-draw-acceptance--to "move" point) "down:1" "up:1" "wait:0.5")))

;;;; What the page shows

(defun canvas-browser-draw-acceptance--look-then (point then)
  "Ask chromium for a picture of the 12 by 12 pixels of the page around
POINT, and call THEN with it, a PNG in base64."
  (canvas-browser--tell "Page.captureScreenshot"
                        (list :format "png"
                              :clip (list :x (- (car point) 6) :y (- (cdr point) 6)
                                          :width 12 :height 12 :scale 1))
                        (lambda (answer) (funcall then (plist-get answer :data)))))

(defun canvas-browser-draw-acceptance--look (point)
  "A picture of the 12 by 12 pixels of the page around POINT, as it is now."
  (let ((picture nil))
    (canvas-browser-draw-acceptance--look-then point (lambda (data) (setq picture data)))
    (should (canvas-browser-live-test--wait 10 (lambda () picture)))
    picture))

(defconst canvas-browser-draw-acceptance--marked-js "getSelection().toString()"
  "The JavaScript that gives the text that the page marks.")

;;;; A PDF in Draw mode

(defun canvas-browser-draw-acceptance--viewer ()
  "The session of the PDF viewer of this page, once it has its document."
  (let ((viewer nil))
    (should (canvas-browser-live-test--wait
             30 (lambda ()
                  (setq viewer (or viewer (canvas-browser-live-test--viewer)))
                  (and viewer (consp (canvas-browser-live-test--page-says
                                      canvas-browser-live-test--pdf-place-js viewer))))))
    viewer))

(defun canvas-browser-draw-acceptance--press-draw ()
  "Click the Draw button of the PDF viewer with the real mouse."
  (let ((found nil))
    ;; The viewer draws its buttons one after the other.
    (should (canvas-browser-live-test--wait
             30 (lambda ()
                  (setq found (canvas-browser-live-test--hint-boxes))
                  (member "Draw" (canvas-browser-live-test--labels (cdr found))))))
    (pcase-let* ((`(,boxes . ,session) found)
                 (box (nth (seq-position (canvas-browser-live-test--labels session) "Draw") boxes)))
      (canvas-browser-draw-acceptance--click (canvas-browser--box-middle box)))))

(defconst canvas-browser-draw-acceptance--page-rect-js
  "(() => { const viewer = document.querySelector('pdf-viewer');
     const area = viewer.shadowRoot.querySelector('#main').getBoundingClientRect();
     const r = viewer.viewport_.getPageScreenRect(0);
     return [area.x + r.x, area.y + r.y, r.width, r.height].map(Math.round); })()"
  "The JavaScript that gives the box of the first page of the PDF in the
window, as [X Y W H].  The viewer keeps the box in a field of its own,
from the corner of the area of the document, which lies beside the
sidebar and below the toolbar.")

(defun canvas-browser-draw-acceptance--v (viewer)
  "The points of a V on the first page of the PDF of VIEWER: (START TURN
END CHORD).  The V goes down from START to TURN and up to END.  CHORD
is the middle of the straight line between START and END."
  (pcase-let ((`(,x ,y ,_ ,_) (canvas-browser-draw-acceptance--page-has-in
                               canvas-browser-draw-acceptance--page-rect-js viewer)))
    (list (cons (+ x 100) (+ y 260)) (cons (+ x 200) (+ y 420))
          (cons (+ x 300) (+ y 260)) (cons (+ x 200) (+ y 260)))))

(defun canvas-browser-draw-acceptance--page-has-in (script session)
  "The value of SCRIPT in the frame of SESSION, once it has one."
  (let ((value nil))
    (should (canvas-browser-live-test--wait
             10 (lambda ()
                  (setq value (canvas-browser-live-test--page-says script session))
                  (consp value))))
    value))

(defmacro canvas-browser-draw-acceptance--in-draw-mode (v &rest body)
  "Run BODY in a page buffer of a PDF, with the Draw mode of its viewer
on.  V is bound to the points of a V on its first page, see
`canvas-browser-draw-acceptance--v'."
  (declare (indent 1))
  `(canvas-browser-draw-acceptance--in-page "pages.pdf"
     (let ((viewer (canvas-browser-draw-acceptance--viewer)))
       (canvas-browser-draw-acceptance--press-draw)
       (let ((,v (canvas-browser-draw-acceptance--v viewer)))
         ,@body))))

;;;; The words of a line

(defconst canvas-browser-draw-acceptance--words-js
  "(() => { const text = document.getElementById('line').firstChild;
     const range = document.createRange();
     const box = i => { range.setStart(text, i); range.setEnd(text, i + 1);
                        return range.getBoundingClientRect(); };
     const first = box(text.data.indexOf('milk'));
     const last = box(text.data.indexOf('sugar') + 'sugar'.length - 1);
     return [first.left + 1, (first.top + first.bottom) / 2,
             last.right + 1, (last.top + last.bottom) / 2].map(Math.round); })()"
  "The JavaScript that gives where milk starts and sugar ends, in the line
of the fixture pad.html, as [X1 Y1 X2 Y2].")

(defun canvas-browser-draw-acceptance--milk-to-sugar ()
  "The point where milk starts and the point where sugar ends, as (FROM . TO)."
  (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (canvas-browser-draw-acceptance--page-has
                                   canvas-browser-draw-acceptance--words-js)))
    (cons (cons x1 y1) (cons x2 y2))))

(defconst canvas-browser-draw-acceptance--count-js
  "(() => { const r = document.getElementById('count').getBoundingClientRect();
     return [Math.round(r.left + r.width / 2), Math.round(r.top + r.height / 2)]; })()"
  "The JavaScript that gives the middle of the button Count of pad.html.")

;;;; The scenarios

(ert-deftest canvas-browser-draw-acceptance-a-stroke-follows-the-mouse ()
  ;; GIVEN a PDF in a page buffer, with the Draw mode of its viewer on
  ;; WHEN the user holds the left button, moves the mouse along a V, and
  ;;      lets go
  ;; THEN the stroke on the PDF follows the V: there is ink at the turn
  ;;      of the V, AND none on the straight line between its two ends
  (canvas-browser-draw-acceptance--in-draw-mode v
    (pcase-let ((`(,start ,turn ,end ,chord) v))
      (let ((turn-before (canvas-browser-draw-acceptance--look turn))
            (chord-before (canvas-browser-draw-acceptance--look chord)))
        (canvas-browser-draw-acceptance--act
         (list (canvas-browser-draw-acceptance--to "move" start) "down:1"
               (canvas-browser-draw-acceptance--to "glide" turn)
               (canvas-browser-draw-acceptance--to "glide" end) "up:1"))
        (should-not (equal (canvas-browser-draw-acceptance--look turn) turn-before))
        (should (equal (canvas-browser-draw-acceptance--look chord) chord-before))))))

(ert-deftest canvas-browser-draw-acceptance-ink-shows-while-the-button-is-held ()
  ;; GIVEN a PDF in a page buffer, with the Draw mode of its viewer on
  ;; WHEN the user holds the left button, moves the mouse to the turn of
  ;;      a V, and holds still
  ;; THEN the ink is at the turn already, before the button comes up
  (canvas-browser-draw-acceptance--in-draw-mode v
    (pcase-let ((`(,start ,turn . ,_) v))
      (let ((before (canvas-browser-draw-acceptance--look turn))
            (held nil))
        (canvas-browser-draw-acceptance--act
         (list (canvas-browser-draw-acceptance--to "move" start) "down:1"
               (canvas-browser-draw-acceptance--to "glide" turn)
               "say:holding" "wait:1.5" "up:1")
         (lambda (_word)
           (canvas-browser-draw-acceptance--look-then turn (lambda (data) (setq held data)))))
        (should held)
        (should-not (equal held before))))))

(ert-deftest canvas-browser-draw-acceptance-text-is-marked-while-the-button-is-held ()
  ;; GIVEN a page with the line "milk sugar coffee"
  ;; WHEN the user holds the left button at the start of "milk", moves the
  ;;      mouse to the end of "sugar", and holds still
  ;; THEN the page marks "milk sugar" before the button comes up
  ;; WHEN the user lets go
  ;; THEN "milk sugar" stays marked
  (canvas-browser-draw-acceptance--in-page "pad.html"
    (pcase-let ((`(,from . ,to) (canvas-browser-draw-acceptance--milk-to-sugar)))
      (let ((held nil))
        (canvas-browser-draw-acceptance--act
         (list (canvas-browser-draw-acceptance--to "move" from) "down:1"
               (canvas-browser-draw-acceptance--to "glide" to)
               "say:holding" "wait:1.5" "up:1")
         (lambda (_word)
           (canvas-browser--evaluate canvas-browser-draw-acceptance--marked-js
                                     (lambda (text) (setq held text)))))
        (should (equal held "milk sugar"))
        (should (equal (canvas-browser-live-test--page-says canvas-browser-draw-acceptance--marked-js)
                       "milk sugar"))))))

(ert-deftest canvas-browser-draw-acceptance-a-click-without-a-move-is-one-click ()
  ;; GIVEN a page with a button that counts its clicks
  ;; WHEN the user clicks the button, without a move between press and
  ;;      release
  ;; THEN the page counted one click
  (canvas-browser-draw-acceptance--in-page "pad.html"
    (pcase-let ((`(,x ,y) (canvas-browser-draw-acceptance--page-has
                           canvas-browser-draw-acceptance--count-js)))
      (canvas-browser-draw-acceptance--click (cons x y))
      (should (equal (canvas-browser-live-test--page-says
                      "document.getElementById('count').dataset.clicks")
                     "1")))))

(ert-deftest canvas-browser-draw-acceptance-a-stroke-ends-where-the-mouse-leaves-the-page ()
  ;; GIVEN a PDF in a page buffer, with the Draw mode of its viewer on
  ;; WHEN the user holds the left button, moves the mouse over the turn
  ;;      of a V and on out of the frame, and lets go there
  ;; THEN the stroke is on the PDF up to where the mouse left the page:
  ;;      there is ink at the turn
  ;; WHEN the user moves the mouse back over the PDF without a button
  ;; THEN no ink comes, as the page holds no button any more
  (canvas-browser-draw-acceptance--in-draw-mode v
    (pcase-let ((`(,start ,turn ,end ,chord) v))
      (let ((turn-before (canvas-browser-draw-acceptance--look turn))
            (chord-before (canvas-browser-draw-acceptance--look chord)))
        (canvas-browser-draw-acceptance--act
         (list (canvas-browser-draw-acceptance--to "move" start) "down:1"
               (canvas-browser-draw-acceptance--to "glide" turn)
               (canvas-browser-draw-acceptance--off-the-frame turn) "up:1"))
        (should-not (equal (canvas-browser-draw-acceptance--look turn) turn-before))
        (canvas-browser-draw-acceptance--act
         (list (canvas-browser-draw-acceptance--to "glide" chord)
               (canvas-browser-draw-acceptance--to "glide" end)))
        (should (equal (canvas-browser-draw-acceptance--look chord) chord-before))))))

;;;; Running

(defun canvas-browser-draw-acceptance--leave (code)
  "Show what Emacs said on the standard error stream, and leave Emacs
with the exit CODE."
  (princ (with-current-buffer (messages-buffer) (buffer-string)) #'external-debugging-output)
  (kill-emacs code))

(defun canvas-browser-draw-acceptance-run ()
  "Run these tests in a frame of 1200 by 800 pixels at the corner of the
screen, show the result on the standard error stream and leave Emacs."
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) 1200 800 t)
  (sit-for 1)
  (let ((stats (ert-run-tests-batch "^canvas-browser-draw-acceptance-")))
    (canvas-browser-draw-acceptance--leave
     (if (zerop (ert-stats-completed-unexpected stats)) 0 1))))

;; Whatever goes wrong, this Emacs ends and says what it did so far: a
;; step that waits for ever would hold make until its timeout.
(run-with-timer 400 nil #'canvas-browser-draw-acceptance--leave 3)

;;; canvas-browser-draw-acceptance.el ends here
