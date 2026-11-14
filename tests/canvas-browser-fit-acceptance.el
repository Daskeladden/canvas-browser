;;; canvas-browser-fit-acceptance.el --- a whole page of a PDF in the window -*- lexical-binding: t -*-

;; `make fit-acceptance' runs these in an Emacs on a virtual display,
;; with a real chromium.  Each test shows a page buffer in the only
;; window of the frame, and presses keys in it as a user does: the
;; command loop of Emacs reads each key and finds its command.  What the
;; PDF shows is read from the viewer of chromium: the page that shows
;; most, and where that page lies in the area of the document.
(require 'ert)
(require 'cl-lib)
(require 'canvas-browser)
(load "canvas-browser-live-tests" nil t)
;; The menu of a page has an entry for `org-store-link', and the menu
;; does not open while that command is unknown.  Emacs 32 does not
;; autoload it, so the tests load it as the Emacs of a user of org has.
(require 'ol)

(defvar canvas-browser-fit-acceptance--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "The directory of the tests.")

(defun canvas-browser-fit-acceptance--fixture (name)
  "The file:// address of the fixture NAME."
  (concat "file://" (expand-file-name (concat "fixtures/" name) canvas-browser-fit-acceptance--dir)))

;;;; The page

(defmacro canvas-browser-fit-acceptance--in-page (name &rest body)
  "Run BODY in a page buffer of the fixture NAME, which the only window
of the frame shows, once the page has drawn itself.  Chromium has a
profile of its own."
  (declare (indent 1))
  `(let ((buffer nil)
         (canvas-browser-profile-directory
          (let ((temporary-file-directory (expand-file-name "~/snap/chromium/common/")))
            (make-temp-file "canvas-browser-fit-" t))))
     (unwind-protect
         (progn
           (delete-other-windows)
           (setq buffer (let ((display-buffer-overriding-action '(display-buffer-same-window)))
                          (canvas-browser (canvas-browser-fit-acceptance--fixture ,name))))
           (with-current-buffer buffer
             (should (canvas-browser-live-test--wait 30 (lambda () canvas-browser--session)))
             (should (canvas-browser-live-test--wait 30 (lambda () (> canvas-browser--frames 0))))
             ,@body))
       (when (buffer-live-p buffer) (kill-buffer buffer))
       (canvas-browser-cdp-stop)
       (delete-directory canvas-browser-profile-directory t))))

(defvar canvas-browser-fit-acceptance--viewer nil
  "The session of the PDF viewer of the page of the test.")

(defmacro canvas-browser-fit-acceptance--in-pdf (&rest body)
  "Run BODY in a page buffer of a PDF of six A4 pages, once its viewer
shows the document at the zoom it opens with.  At that zoom a page is
taller than the window, as a page of a document from an office is."
  (declare (indent 0))
  `(canvas-browser-fit-acceptance--in-page "a4.pdf"
     (let ((canvas-browser-fit-acceptance--viewer nil))
       (should (canvas-browser-live-test--wait
                30 (lambda ()
                     (setq canvas-browser-fit-acceptance--viewer
                           (or canvas-browser-fit-acceptance--viewer
                               (canvas-browser-live-test--viewer)))
                     (canvas-browser-fit-acceptance--shown))))
       ,@body)))

;;;; The keys

(defun canvas-browser-fit-acceptance--leave-when-done ()
  "Leave the command loop of a step once it read every key."
  (unless unread-command-events
    (exit-recursive-edit)))

(defun canvas-browser-fit-acceptance--press (keys)
  "Have the command loop of Emacs read KEYS, a key description as `kbd'
reads it, and run their commands, as the user's own."
  (let ((buffer (current-buffer))
        (timer (run-with-timer 0.3 0.3 #'canvas-browser-fit-acceptance--leave-when-done)))
    ;; The command loop makes the buffer of the selected window the
    ;; current one, so the test comes back to the page buffer after it.
    (unwind-protect
        (save-current-buffer
          (select-window (get-buffer-window buffer))
          (with-timeout (10 (error "canvas-browser: the keys %s were not read in ten seconds" keys))
            (setq unread-command-events (listify-key-sequence (kbd keys)))
            (recursive-edit)))
      (cancel-timer timer)
      (setq unread-command-events nil))))

(defun canvas-browser-fit-acceptance--said-p (text)
  "Whether Emacs said TEXT in the echo area, as *Messages* keeps it."
  (with-current-buffer (messages-buffer)
    (and (string-search text (buffer-string)) t)))

;;;; What the PDF shows

(defconst canvas-browser-fit-acceptance--shown-js
  "(() => { const v = document.querySelector('pdf-viewer');
     if (!v || v.loadState_ !== 'success') return null;
     const p = v.viewport_, i = p.getMostVisiblePage(), r = p.getPageScreenRect(i);
     return {page: i + 1, pages: v.docLength_, top: Math.round(r.y),
             bottom: Math.round(r.y + r.height), width: Math.round(r.width),
             y: Math.round(p.position.y),
             areaWidth: p.size.width, areaHeight: p.size.height}; })()"
  "The JavaScript that gives what the PDF viewer shows, or null before it
has loaded the document.  PAGE is the page that shows most, and TOP,
BOTTOM and WIDTH are where that page lies in the area of the document,
which lies beside the sidebar and below the toolbar.  AREAWIDTH and
AREAHEIGHT are the size of that area, and Y is how far the document is
scrolled down.  The viewer keeps all this in fields of its own.")

(defun canvas-browser-fit-acceptance--shown ()
  "What the PDF viewer shows now, as a plist, or nil before it has loaded."
  (and canvas-browser-fit-acceptance--viewer
       (let ((answer (canvas-browser-live-test--page-says
                      canvas-browser-fit-acceptance--shown-js
                      canvas-browser-fit-acceptance--viewer)))
         ;; The null of JSON comes back as :null.
         (and (consp answer) answer))))

(defun canvas-browser-fit-acceptance--whole-page-p (page)
  "Whether PAGE shows most, with its top and its bottom in the window."
  (let ((shown (canvas-browser-fit-acceptance--shown)))
    (and (equal (plist-get shown :page) page)
         (>= (plist-get shown :top) 0)
         (<= (plist-get shown :bottom) (plist-get shown :areaHeight)))))

(defun canvas-browser-fit-acceptance--fills-the-width-p ()
  "Whether the page that shows most is about as wide as the area of the
document: nine tenths of it at least."
  (let ((shown (canvas-browser-fit-acceptance--shown)))
    (>= (plist-get shown :width) (* 0.9 (plist-get shown :areaWidth)))))

(defun canvas-browser-fit-acceptance--soon (test)
  "Whether TEST answers within ten seconds."
  (canvas-browser-live-test--wait 10 test))

(defun canvas-browser-fit-acceptance--whole-page-soon-p (page)
  "Whether PAGE shows whole within ten seconds."
  (canvas-browser-fit-acceptance--soon
   (lambda () (canvas-browser-fit-acceptance--whole-page-p page))))

(defun canvas-browser-fit-acceptance--a-whole-page ()
  "Press z, and wait until the whole first page shows."
  (canvas-browser-fit-acceptance--press "z")
  (should (canvas-browser-fit-acceptance--whole-page-soon-p 1)))

;;;; The scenarios

(ert-deftest canvas-browser-fit-acceptance-z-fits-a-whole-page ()
  ;; GIVEN a PDF of six A4 pages in a page buffer, at the zoom it opens
  ;;      with, so the first page is cut off at the foot of the window
  ;; WHEN the user presses z
  ;; THEN the whole first page shows in the window, its top and its
  ;;      bottom too
  (canvas-browser-fit-acceptance--in-pdf
    (should-not (canvas-browser-fit-acceptance--whole-page-p 1))
    (canvas-browser-fit-acceptance--press "z")
    (should (canvas-browser-fit-acceptance--whole-page-soon-p 1))))

(ert-deftest canvas-browser-fit-acceptance-z-again-fits-the-width ()
  ;; GIVEN a PDF in a page buffer, with a whole page fitted by z
  ;; WHEN the user presses z again
  ;; THEN the page is as wide as the area of the document
  ;; WHEN the user presses z a third time
  ;; THEN the whole page shows again
  (canvas-browser-fit-acceptance--in-pdf
    (canvas-browser-fit-acceptance--a-whole-page)
    (canvas-browser-fit-acceptance--press "z")
    (should (canvas-browser-fit-acceptance--soon #'canvas-browser-fit-acceptance--fills-the-width-p))
    (canvas-browser-fit-acceptance--press "z")
    (should (canvas-browser-fit-acceptance--whole-page-soon-p 1))))

(ert-deftest canvas-browser-fit-acceptance-z-in-the-menu-fits-a-whole-page ()
  ;; GIVEN a PDF in a page buffer, at the zoom it opens with
  ;; WHEN the user opens the menu with SPC, presses z in it, and closes
  ;;      the menu with SPC
  ;; THEN the whole first page shows in the window
  (canvas-browser-fit-acceptance--in-pdf
    (canvas-browser-fit-acceptance--press "SPC z SPC")
    (should (canvas-browser-fit-acceptance--whole-page-soon-p 1))))

(ert-deftest canvas-browser-fit-acceptance-the-screen-keys-go-a-whole-page ()
  ;; GIVEN a PDF in a page buffer, with the whole first page fitted by z
  ;; WHEN the user presses C-v
  ;; THEN the whole second page shows
  ;; WHEN the user presses M-v
  ;; THEN the whole first page shows again
  (canvas-browser-fit-acceptance--in-pdf
    (canvas-browser-fit-acceptance--a-whole-page)
    (canvas-browser-fit-acceptance--press "C-v")
    (should (canvas-browser-fit-acceptance--whole-page-soon-p 2))
    (canvas-browser-fit-acceptance--press "M-v")
    (should (canvas-browser-fit-acceptance--whole-page-soon-p 1))))

(ert-deftest canvas-browser-fit-acceptance-c-v-stops-at-the-last-page ()
  ;; GIVEN a PDF of six A4 pages in a page buffer, with the whole first
  ;;      page fitted by z
  ;; WHEN the user presses C-v six times
  ;; THEN the whole last page shows, AND Emacs says no error
  (canvas-browser-fit-acceptance--in-pdf
    (canvas-browser-fit-acceptance--a-whole-page)
    (let ((said-before (with-current-buffer (messages-buffer) (buffer-size))))
      (canvas-browser-fit-acceptance--press "C-v C-v C-v C-v C-v C-v")
      (should (canvas-browser-fit-acceptance--whole-page-soon-p 6))
      (sit-for 1)
      (should (canvas-browser-fit-acceptance--whole-page-p 6))
      (should-not (string-search "canvas-browser:" (with-current-buffer (messages-buffer)
                                                     (buffer-substring (1+ said-before) (point-max))))))))

(ert-deftest canvas-browser-fit-acceptance-c-v-scrolls-a-screen-in-the-width ()
  ;; GIVEN a PDF in a page buffer, fitted to the width by z pressed twice
  ;; WHEN the user presses C-v
  ;; THEN the document moves up the height of the window less 60 pixels,
  ;;      as it does without a fit
  (canvas-browser-fit-acceptance--in-pdf
    (canvas-browser-fit-acceptance--a-whole-page)
    (canvas-browser-fit-acceptance--press "z")
    (should (canvas-browser-fit-acceptance--soon #'canvas-browser-fit-acceptance--fills-the-width-p))
    (let ((before (plist-get (canvas-browser-fit-acceptance--shown) :y))
          (screen (- (cdr canvas-browser--size) 60)))
      (canvas-browser-fit-acceptance--press "C-v")
      (should (canvas-browser-fit-acceptance--soon
               (lambda () (equal (plist-get (canvas-browser-fit-acceptance--shown) :y)
                                 (+ before screen))))))))

(ert-deftest canvas-browser-fit-acceptance-z-on-a-page-of-the-web-says-why-not ()
  ;; GIVEN a page of the web, which is no PDF, in a page buffer
  ;; WHEN the user presses z
  ;; THEN Emacs says "canvas-browser: a page has nothing whole to fit",
  ;;      as it does today
  (canvas-browser-fit-acceptance--in-page "pad.html"
    (canvas-browser-fit-acceptance--press "z")
    (should (canvas-browser-fit-acceptance--soon
             (lambda () (canvas-browser-fit-acceptance--said-p
                         "canvas-browser: a page has nothing whole to fit"))))))

;;;; Running

(defun canvas-browser-fit-acceptance--leave (code)
  "Show what Emacs said on the standard error stream, and leave Emacs
with the exit CODE."
  (princ (with-current-buffer (messages-buffer) (buffer-string)) #'external-debugging-output)
  (kill-emacs code))

(defun canvas-browser-fit-acceptance-run ()
  "Run these tests in a frame of 1200 by 800 pixels at the corner of the
screen, show the result on the standard error stream and leave Emacs."
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) 1200 800 t)
  (sit-for 1)
  (let ((stats (ert-run-tests-batch "^canvas-browser-fit-acceptance-")))
    (canvas-browser-fit-acceptance--leave
     (if (zerop (ert-stats-completed-unexpected stats)) 0 1))))

;; Whatever goes wrong, this Emacs ends and says what it did so far: a
;; step that waits for ever would hold make until its timeout.
(run-with-timer 400 nil #'canvas-browser-fit-acceptance--leave 3)

;;; canvas-browser-fit-acceptance.el ends here
