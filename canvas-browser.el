;;; canvas-browser.el --- A web browser in a buffer, drawn on a canvas -*- lexical-binding: t -*-

;; Copyright (C) 2026 canvas-browser contributors

;; Author: Daskeladden
;; Version: 0.1.0
;; Package-Requires: ((emacs "32.0.50") (websocket "1.15") (transient "0.7.0") (canvas-keys "0.1.0") (canvas-diagram "0.1.0"))
;; Keywords: hypermedia, tools
;; URL: https://github.com/Daskeladden/canvas-browser

;;; Commentary:
;; A page of a headless chromium, drawn on a canvas in an Emacs buffer.
;; Emacs speaks the DevTools protocol itself; see canvas-browser-cdp.el.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'canvas-cairo)
(require 'canvas-keys)
(require 'transient)
(require 'url-util)
(require 'url-expand)
(require 'xref)
(require 'canvas-browser-cdp)
(require 'canvas-browser-translate)
(require 'canvas-browser-merge)

(defcustom canvas-browser-quality 70
  "The quality of a screencast frame, from 1 to 100.
It holds for the frames of a page that is moving, which are JPEGs.  The
still picture of a page that has stopped is a PNG and loses nothing, so
this does not reach it."
  :type 'integer
  :group 'canvas-browser)

(defvar-local canvas-browser--session nil "The session of this buffer's page.")
(defvar-local canvas-browser--target nil "The target of this buffer's page.")
(defvar-local canvas-browser--canvas nil "The canvas image of this buffer.")
(defvar-local canvas-browser--context nil "The cairo context of that canvas.")
(defvar-local canvas-browser--size nil "(W . H) of the canvas, in pixels.")
(defvar-local canvas-browser--url nil "The URL of this buffer's page.")
(defvar-local canvas-browser--title nil "The title of this buffer's page.")
(defvar-local canvas-browser--part nil
  "The part of the page that is picked, or nil for the whole page.
It is a plist.  `:place' is the place of the part among the parts that
the page holds in an array of its own.  `:scrolls' says whether the
part scrolls on its own, and then the scroll keys move it.")

(defvar-local canvas-browser--host nil
  "The buffer this page's picture is embedded in, or nil for a page of its own.")
(defvar-local canvas-browser--frames 0 "How many frames this buffer painted.")

(defvar canvas-browser--buffers (make-hash-table :test #'eq :weakness 'key)
  "The buffer of each canvas, so that a thumbnail finds its page.")

(defvar-local canvas-browser--last-frame nil "The file of the last frame painted.")

(defvar-local canvas-browser--painted-mark nil
  "The md5 of the picture the canvas holds, to know the same one again.")

(defvar-local canvas-browser--live-boxes nil
  "The boxes of the parts of this page that keep moving.
While there are any, the canvas holds a still picture of the page and a
frame is drawn inside these boxes alone.")

(defvar-local canvas-browser--live-timer nil
  "The timer that draws a page with moving parts afresh now and then.")

(defvar-local canvas-browser--crisp nil
  "Whether the canvas holds the still picture of a page that has stopped.
A page that moves is painted from the JPEGs of the screencast, and one
that has stopped is painted from a PNG, which loses nothing.")

;;;; The canvas and its frames

(defun canvas-browser--host-of (buffer)
  "The live buffer BUFFER's page is embedded in, or nil."
  (let ((host (buffer-local-value 'canvas-browser--host buffer)))
    (and (buffer-live-p host) host)))

(defun canvas-browser--showing-windows (buffer)
  "The windows that show BUFFER's page: its own, and those of its host."
  (append (get-buffer-window-list buffer nil t)
          (when-let* ((host (canvas-browser--host-of buffer)))
            (get-buffer-window-list host nil t))))

(defun canvas-browser--shown-p (&optional buffer)
  "Whether a window shows the page of BUFFER, or of this buffer.
An embedded page is shown while a window shows the buffer it is in, or
the page itself, as it is while fullscreen."
  (let* ((buffer (or buffer (current-buffer)))
         (host (canvas-browser--host-of buffer)))
    (and (or (get-buffer-window buffer t)
             (and host (get-buffer-window host t)))
         t)))

(defun canvas-browser--make-canvas (width height)
  "A canvas image WIDTH by HEIGHT, at its own pixel size."
  (list 'image :type 'canvas :id (make-symbol "canvas-browser")
        :data-width width :data-height height :scale 1.0))

(defconst canvas-browser--least-size 200
  "The smallest page chromium is asked to lay out, in pixels.
A window reports no size until the display has laid its frame out, and a
page of no width paints nothing: chromium answers a picture of it with
\"Cannot take screenshot with 0 width\".")

(defun canvas-browser--adopt (width height)
  "Give this buffer a canvas WIDTH by HEIGHT, and show it.
A window with no size yet gets a page of `canvas-browser--least-size\=',
which the next window change makes right."
  (setq width (max width canvas-browser--least-size)
        height (max height canvas-browser--least-size))
  (when canvas-browser--context
    (canvas-cairo-destroy canvas-browser--context))
  (setq canvas-browser--canvas (canvas-browser--make-canvas width height)
        canvas-browser--context (canvas-cairo-context canvas-browser--canvas)
        canvas-browser--size (cons width height)
        ;; The new canvas is empty, so the picture that was painted is
        ;; not the picture it holds, and must be painted again.
        canvas-browser--painted-mark nil
        canvas-browser--crisp nil)
  (canvas-browser--forget-live)
  (puthash canvas-browser--canvas (current-buffer) canvas-browser--buffers)
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; `propertize' keeps the spec `eq': the canvas is keyed on it.
    (insert (propertize "#" 'display canvas-browser--canvas))))

(defvar-local canvas-browser--number nil
  "What tells this page's files from the files of every other page.")

(defvar canvas-browser--pages 0
  "How many pages have been opened, so that each one names its files.")

(defconst canvas-browser--png-signature
  (unibyte-string #x89 #x50 #x4E #x47 #x0D #x0A #x1A #x0A)
  "The eight bytes that every PNG begins with.")

(defconst canvas-browser--suffixes '("jpg" "png")
  "The suffixes a page writes its pictures with.")

(defun canvas-browser--suffix (bytes)
  "The suffix BYTES deserve: a PNG says what it is in its first bytes."
  (if (string-prefix-p canvas-browser--png-signature bytes) "png" "jpg"))

(defun canvas-browser--file (name suffix)
  "The file this page writes NAME to as SUFFIX, no page sharing a name."
  (unless canvas-browser--number
    (setq canvas-browser--number (cl-incf canvas-browser--pages)))
  (expand-file-name (format "canvas-browser-%s-%d-%s.%s"
                            (emacs-pid) canvas-browser--number name suffix)
                    (if (file-writable-p "/dev/shm")
                        "/dev/shm"
                      temporary-file-directory)))

(defun canvas-browser--write (data name)
  "Write DATA, the base64 of a picture, to this page's NAME; that file."
  (canvas-browser--write-bytes (base64-decode-string data) name))

(defun canvas-browser--write-bytes (bytes name)
  "Write BYTES, a picture, to this page's NAME; that file.
The suffix says what the picture is: a frame of a page that moves is a
JPEG, and the still picture of one that has stopped is a PNG."
  (let ((file (canvas-browser--file name (canvas-browser--suffix bytes)))
        (coding-system-for-write 'binary))
    (write-region bytes nil file nil 'silent)
    file))

(defun canvas-browser--frame-file (data)
  "DATA, the base64 of a frame, written to a file; that file."
  (canvas-browser--write data "frame"))


(defconst canvas-browser--jpeg-frame-markers
  '(#xC0 #xC1 #xC2 #xC3 #xC5 #xC6 #xC7 #xC9 #xCA #xCB #xCD #xCE #xCF)
  "The JPEG markers that carry the size of the picture.")

(defun canvas-browser--be16 (position)
  "The two bytes at POSITION of this buffer, as one number."
  (+ (* 256 (char-after position)) (char-after (1+ position))))

(defun canvas-browser--be32 (position)
  "The four bytes at POSITION of this buffer, as one number."
  (+ (* 65536 (canvas-browser--be16 position))
     (canvas-browser--be16 (+ position 2))))

(defun canvas-browser--jpeg-size ()
  "The (W . H) of the JPEG in this buffer, or nil when it says nothing."
  (when (and (> (buffer-size) 4)
             (eq (char-after 1) #xFF) (eq (char-after 2) #xD8))
    (let ((position 3)
          (size nil))
      (while (and (not size) (< (+ position 9) (point-max)))
        (cond ((not (eq (char-after position) #xFF))
               (setq position (1+ position)))
              ((memq (char-after (1+ position)) canvas-browser--jpeg-frame-markers)
               (setq size (cons (canvas-browser--be16 (+ position 7))
                                (canvas-browser--be16 (+ position 5)))))
              (t (setq position (+ position 2 (canvas-browser--be16 (+ position 2)))))))
      size)))

(defun canvas-browser--png-size ()
  "The (W . H) of the PNG in this buffer, or nil when it is no PNG.
The IHDR chunk comes first in every PNG, and carries the two numbers."
  (when (and (> (buffer-size) 24)
             (equal (buffer-substring 1 9) canvas-browser--png-signature))
    (cons (canvas-browser--be32 17) (canvas-browser--be32 21))))

(defun canvas-browser--picture-size (picture)
  "The (W . H) of PICTURE, a string of bytes, or nil when it says nothing.
A frame of the screencast is a JPEG and a still picture is a PNG, and
both are drawn to the canvas, so both are measured here."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (substring picture 0 (min (length picture) 65536)))
    (or (canvas-browser--jpeg-size) (canvas-browser--png-size))))

(defconst canvas-browser--shape-tolerance 0.1
  "How far a frame's shape may differ from the canvas's and still be it.")

(defun canvas-browser--another-shape-p (picture)
  "Whether PICTURE, a string of bytes, is of another shape than this canvas.
Chromium draws the whole page to take a picture of it and sends that
draw as a frame, which is the page smeared over the window."
  (when-let* ((size (canvas-browser--picture-size picture))
              (ours canvas-browser--size))
    (let ((theirs (/ (float (car size)) (max 1 (cdr size))))
          (mine (/ (float (car ours)) (max 1 (cdr ours)))))
      (> (abs (- theirs mine)) (* canvas-browser--shape-tolerance mine)))))

(defun canvas-browser--paint (data)
  "Paint DATA, the base64 of a frame, on the canvas of this buffer.
A picture that cannot be read leaves the one before it on the canvas.
So does a picture the canvas already holds: chromium sends a frame after
every picture asked of it, that picture being a draw like any other, and
painting it again would have the still and the frame take turns on the
canvas for ever, which the reader sees as a pulse.

A frame is drawn over the whole canvas, unless the canvas holds a still
picture of the page and the parts that move on it are known.  Then the
frame goes inside those parts alone, and the text around them stays as
crisp as the still picture left it."
  (let* ((bytes (base64-decode-string data))
         (mark (md5 bytes))
         (still (equal (canvas-browser--suffix bytes) "png"))
         ;; Whether the canvas holds a picture of this page at all: a new
         ;; canvas is blank, and drawing into its moving parts alone
         ;; would leave the rest of it blank.
         (held (and canvas-browser--painted-mark t))
         (file (unless (or (equal mark canvas-browser--painted-mark)
                           (canvas-browser--another-shape-p bytes))
                 (canvas-browser--write-bytes bytes "frame"))))
    (when file
      (setq canvas-browser--last-frame file
            canvas-browser--painted-mark mark))
    (condition-case error
        (when file
          (let ((parts (and (not still) held canvas-browser--live-boxes)))
            (if parts
                (canvas-browser--paint-boxes file parts)
              ;; The canvas's own size, not the page's: a window that has
              ;; just changed leaves the two apart for a moment, and a
              ;; frame drawn to the wrong size is sheared across it.
              (canvas-cairo-image canvas-browser--context file 0 0
                                  (plist-get (cdr canvas-browser--canvas) :data-width)
                                  (plist-get (cdr canvas-browser--canvas) :data-height)))
            (setq canvas-browser--crisp (or still (and parts t)))
            (canvas-refresh canvas-browser--canvas)
            (cl-incf canvas-browser--frames)
            (force-window-update (current-buffer))
            (when-let* ((host (canvas-browser--host-of (current-buffer))))
              (force-window-update host))
            ;; A page that moved will stop moving: ask for the still
            ;; picture then.  A still picture asks for nothing, or the
            ;; two would follow one another without end, and a page whose
            ;; moving parts are known has a clock of its own.
            (unless (or canvas-browser--crisp canvas-browser--live-boxes)
              (canvas-browser--crisp-soon))))
      (error (message "canvas-browser: %s" (error-message-string error))))))

(defcustom canvas-browser-live-interval 2.0
  "Seconds between two still pictures while a part of the page keeps moving.
Between them the frames are drawn inside the moving parts alone, so the
rest of the window keeps the still picture it was given."
  :type 'number
  :group 'canvas-browser)

(defcustom canvas-browser-crisp-delay 0.4
  "Seconds of quiet before the page is drawn again without loss.
Every frame of the screencast is a JPEG, which is cheap to make and shows
its workings around small text.  A page that has stopped moving is read
rather than watched, so once the frames stop for this long the window is
asked for a still picture, which loses nothing."
  :type 'number
  :group 'canvas-browser)

(defvar-local canvas-browser--crisp-timer nil
  "The timer that draws this page again without loss once it is quiet.")

(defvar-local canvas-browser--crisp-when nil
  "When this page was last asked for a still picture of itself.")

(defvar-local canvas-browser--commanded nil
  "When a command last ran in this buffer.
A page that is being scrolled or typed in is not asked for a still
picture: each costs chromium a fifth of a second that it owes the keys.")

(defvar-local canvas-browser--commands 0
  "How many commands have run in this buffer.
A still picture asked for before a command and arriving after it shows
the page as it was, and painting it would take the window backwards.")

(defconst canvas-browser--crisp-least 0.05
  "The shortest wait before a still picture, in seconds.")

(defun canvas-browser--crisp-wait (now)
  "Seconds to wait before the still picture, NOW being the time.
The picture waits `canvas-browser-crisp-delay\=' after the last command, so
that scrolling and typing are never interrupted by one, and
`canvas-browser-live-interval\=' after the last picture, so that a page
which never stops moving is drawn afresh at a steady rate rather than at
every frame.  The frames themselves do not put it off: a page with a
spinner on it never falls quiet, and waiting for quiet would leave it
lossy for as long as the reader looked at it."
  (let ((after-key (+ (or canvas-browser--commanded 0) canvas-browser-crisp-delay))
        (after-picture (+ (or canvas-browser--crisp-when 0)
                          canvas-browser-live-interval)))
    (max canvas-browser--crisp-least (- (max after-key after-picture) now))))

(defun canvas-browser--crisp-soon ()
  "Draw this page again without loss once the frames have stopped.
Each frame puts the picture off, so a page that keeps moving is never
held up by one, and a page that settles is crisp a moment later."
  (when (timerp canvas-browser--crisp-timer)
    (cancel-timer canvas-browser--crisp-timer))
  (setq canvas-browser--crisp-timer
        (run-with-timer (canvas-browser--crisp-wait (float-time)) nil
                        #'canvas-browser--paint-crisp (current-buffer))))

(defun canvas-browser--paint-crisp (buffer)
  "Ask BUFFER\'s window for a still picture of itself, which loses nothing.
What keeps moving on the page is measured with it: between two still
pictures the frames are drawn into those parts alone.  The page is
asked for its title as well.  A page may name itself after it has
loaded, as Reddit names a post, and chromium tells nobody of that."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq canvas-browser--crisp-timer nil)
      (when (canvas-browser--may-ask-p buffer)
        (setq canvas-browser--crisp-when (float-time))
        (canvas-browser--paint-window)
        (canvas-browser--read-title)
        (canvas-browser--measure-live)))))

(defcustom canvas-browser-live-share 0.3
  "How much of the window may move and still count as parts of it.
A page whose movement covers more than this share is a moving page, and
its frames belong on the whole canvas."
  :type 'number
  :group 'canvas-browser)

(defcustom canvas-browser-live-pad 4
  "Pixels a moving part grows by before the frames are drawn into it.
A thing that turns reaches past the box it is measured in."
  :type 'integer
  :group 'canvas-browser)

(defconst canvas-browser--live-most 32
  "The most moving parts a page may have before it counts as a moving page.")

(defconst canvas-browser--live-script
  "(function () {
     const boxes = [];
     const add = e => {
       if (!e || !e.getBoundingClientRect) return;
       const r = e.getBoundingClientRect();
       if (r.width <= 0 || r.height <= 0) return;
       if (r.bottom < 0 || r.top > innerHeight) return;
       if (r.right < 0 || r.left > innerWidth) return;
       boxes.push({x: Math.floor(r.x), y: Math.floor(r.y),
                   w: Math.ceil(r.width), h: Math.ceil(r.height)});
     };
     for (const a of document.getAnimations()) {
       if (a.playState === 'running' && a.effect && a.effect.target) add(a.effect.target);
     }
     document.querySelectorAll('video, canvas, img[src$=\".gif\"]').forEach(add);
     return boxes;
   })()"
  "The JavaScript that gives the boxes of everything moving on the page.
A page that is otherwise still moves where an animation runs, where a
video or a canvas draws itself, and where a GIF turns.")

(defun canvas-browser--measure-live ()
  "Ask the page which of its parts keep moving, and take them."
  (let ((buffer (current-buffer)))
    (canvas-browser--evaluate
     canvas-browser--live-script
     (lambda (boxes)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer (canvas-browser--took-live boxes)))))))

(defun canvas-browser--grow-box (box)
  "BOX grown by `canvas-browser-live-pad\=', and kept inside the window."
  (let* ((pad canvas-browser-live-pad)
         (x (max 0 (- (plist-get box :x) pad)))
         (y (max 0 (- (plist-get box :y) pad))))
    (list :x x :y y
          :w (min (- (car canvas-browser--size) x) (+ (plist-get box :w) pad pad))
          :h (min (- (cdr canvas-browser--size) y) (+ (plist-get box :h) pad pad)))))

(defun canvas-browser--box-area (box)
  "The pixels BOX covers."
  (* (plist-get box :w) (plist-get box :h)))

(defun canvas-browser--boxes-area (boxes)
  "The pixels BOXES cover, counting an overlap twice, which is near enough."
  (cl-loop for box in boxes sum (canvas-browser--box-area box)))

(defun canvas-browser--took-live (boxes)
  "Keep BOXES as the moving parts of this page, if they are parts at all.
Too many of them, or too much of the window, means a page that moves
rather than a page with something moving on it."
  (let* ((grown (mapcar #'canvas-browser--grow-box boxes))
         (window (* (car canvas-browser--size) (cdr canvas-browser--size)))
         (parts (and grown
                     (<= (length grown) canvas-browser--live-most)
                     (< (canvas-browser--boxes-area grown)
                        (* canvas-browser-live-share window)))))
    (if (not parts)
        (canvas-browser--drop-live)
      (setq canvas-browser--live-boxes grown)
      (unless canvas-browser--live-timer
        (setq canvas-browser--live-timer
              (run-with-timer canvas-browser-live-interval canvas-browser-live-interval
                              #'canvas-browser--paint-crisp (current-buffer)))))))

(defun canvas-browser--drop-live ()
  "Draw the whole window again from every frame.
A page whose parts are forgotten is painted as it was before there were
any.  While there were parts, the frames went into them alone, so the
rest of the canvas is as old as the last still picture.  The canvas
therefore no longer counts as the still picture of the page, and the
freshness check asks for one if none comes."
  (when canvas-browser--live-timer
    (cancel-timer canvas-browser--live-timer)
    (setq canvas-browser--live-timer nil))
  (when canvas-browser--live-boxes
    (setq canvas-browser--live-boxes nil
          canvas-browser--crisp nil)))

(defun canvas-browser--forget-live ()
  "Draw the whole window again from every frame, and put the still off.
Any command may have changed the page anywhere.  A page being worked on
is left to its frames: the still picture waits for the quiet after the
last key, and one asked for before the command is dropped.  A page that
says nothing moves on it is no command, and takes `canvas-browser--drop-live'."
  (cl-incf canvas-browser--commands)
  (setq canvas-browser--commanded (float-time))
  (canvas-browser--drop-live))

(defun canvas-browser--paint-boxes (file boxes)
  "Draw FILE, a picture of the whole window, inside BOXES alone.
All the boxes make one clip, and the picture is drawn once: reading a
frame costs about four milliseconds, and reading it once for each box
costs more than drawing the whole window would."
  (let ((context canvas-browser--context))
    (canvas-cairo-save context)
    (canvas-cairo-new-path context)
    (dolist (box boxes)
      (canvas-cairo-rectangle context (plist-get box :x) (plist-get box :y)
                              (plist-get box :w) (plist-get box :h)))
    (canvas-cairo-clip context)
    (canvas-cairo-image context file 0 0
                        (plist-get (cdr canvas-browser--canvas) :data-width)
                        (plist-get (cdr canvas-browser--canvas) :data-height))
    (canvas-cairo-restore context)))

(defun canvas-browser--frame (params)
  "Take PARAMS of a screencast frame: paint it, and ask for the next."
  (canvas-browser--tell "Page.screencastFrameAck"
                        (list :sessionId (plist-get params :sessionId)))
  (canvas-browser--paint-soon (plist-get params :data))
  (canvas-browser--schedule-spots))

(defvar-local canvas-browser--pending nil
  "The newest frame this buffer has not painted yet.")

(defvar-local canvas-browser--paint-timer nil
  "The timer that paints the newest frame.")

(defvar-local canvas-browser--painted 0
  "When this buffer last painted a frame.")

(defcustom canvas-browser-frame-interval 0.08
  "The shortest time between two drawings of the page, in seconds.
An advertisement that moves makes chromium send frames without stopping,
and Emacs has other work than drawing every one of them."
  :type 'number
  :group 'canvas-browser)

(defcustom canvas-browser-hidden-interval 2.0
  "The shortest time between two drawings of a page no window shows.
The page is kept in mind for the map and for the moment you come back to
it, and takes hardly any of the time of the page you are reading."
  :type 'number
  :group 'canvas-browser)

(defun canvas-browser--paint-delay ()
  "How long this buffer waits before it draws the frame it holds."
  (let ((interval (if (canvas-browser--shown-p)
                      canvas-browser-frame-interval
                    canvas-browser-hidden-interval)))
    (max 0 (- interval (- (float-time) canvas-browser--painted)))))

(defvar-local canvas-browser--hinting nil
  "Whether the hints are on the canvas, waiting to be named.
A frame painted then would paint over them.")

(defun canvas-browser--paint-soon (data)
  "Paint DATA, the base64 of a frame, once Emacs has a moment for it.
A smooth scroll and a page that animates send frames faster than they
can be drawn; only the newest of them is worth drawing, and no faster
than `canvas-browser-frame-interval\=' allows."
  (setq canvas-browser--pending data)
  (unless (or canvas-browser--hinting canvas-browser--paint-timer)
    (setq canvas-browser--paint-timer
          (run-with-timer (canvas-browser--paint-delay) nil
                          #'canvas-browser--paint-pending (current-buffer)))))

(defun canvas-browser--paint-pending (buffer)
  "Paint the frame BUFFER is waiting to draw."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq canvas-browser--paint-timer nil)
      (when-let* (((not canvas-browser--hinting))
                  (data canvas-browser--pending))
        (setq canvas-browser--pending nil
              canvas-browser--painted (float-time))
        (canvas-browser--paint data)))))

;;;; The page

(defvar-local canvas-browser--restarting nil
  "Whether this buffer is opening its page again after chromium died.")

(defun canvas-browser--start-chromium ()
  "Start chromium, unless it runs.
Emacs is made the notification service of the session first, when that
is asked for: chromium looks for the service when it starts."
  (canvas-browser--serve-notifications)
  (canvas-browser-cdp-start))

(defun canvas-browser--restart ()
  "Start chromium again and open this page afresh.
The flag keeps the commands of the fresh page out of this path, which
would otherwise call itself for each of them."
  (let ((canvas-browser--restarting t))
    (message "canvas-browser: chromium is gone; starting it again")
    (canvas-browser--start-chromium)
    (canvas-browser--reopen)))

(defun canvas-browser--reopen ()
  "Open this buffer's page afresh, at its address and size.
The page gets a new canvas, so a host it is embedded in is given that
one to show."
  (canvas-browser--open canvas-browser--url
                        (car canvas-browser--size) (cdr canvas-browser--size))
  (when (canvas-browser--host-of (current-buffer))
    (canvas-browser--show-in-host)))

(defun canvas-browser--tell (method params &optional answer refused)
  "Send METHOD with PARAMS in the session of this buffer.
ANSWER, when given, is called with chromium's answer, and REFUSED with
what chromium says when it refuses the command.  A chromium that died
is started again, and a page that chromium has let go of is opened
afresh; a command without a session reaches no page."
  (when (string-prefix-p "Input.dispatch" method)
    (canvas-browser--mark-changed))
  (if (or canvas-browser--restarting
          (and (canvas-browser-cdp-running-p) canvas-browser--session))
      (canvas-browser-cdp-send method params answer canvas-browser--session refused)
    (canvas-browser--restart)))

(defun canvas-browser--mark-changed ()
  "Have Emacs count this buffer as changed, without a change that is seen.
The buffer holds one glyph whatever the page shows, so nothing in it
changes when the page does.  What is kept for a buffer until the buffer
changes, as consult-imenu keeps an index, would stay that of the page
as it was.  A click, a key and the wheel can each change the page."
  (with-silent-modifications
    (put-text-property (point-min) (point-max) 'canvas-browser-changed (float-time))))

(defconst canvas-browser--not-active "Not attached to an active page"
  "What chromium says when it refuses a command because of where the page is.
A page is not the active one of its tab for a moment while it opens,
and while it moves to a page of another kind, as a sign-in does.")

(defconst canvas-browser--retries 20
  "How many times a command that chromium refuses for a moment is sent again.")

(defconst canvas-browser--retry-interval 0.25
  "Seconds before a command that chromium refused for a moment is sent again.")

(defun canvas-browser--tell-again (method params &optional wanted left)
  "Send METHOD with PARAMS in this page, and again while chromium refuses it.
It is sent again only for the refusal of `canvas-browser--not-active\=',
which passes: a moment later, as long as WANTED still holds, in the
same session, and LEFT times at most, which is
`canvas-browser--retries\=' at the start.  WANTED is a function called
in this buffer, or nil for a command that is always wanted.  Any other
refusal, and the last of these, is said to the reader."
  (let ((buffer (current-buffer))
        (session canvas-browser--session)
        (left (or left canvas-browser--retries)))
    (canvas-browser--tell
     method params nil
     (lambda (says)
       (if (and (equal says canvas-browser--not-active) (> left 0))
           (run-with-timer
            canvas-browser--retry-interval nil
            (lambda ()
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (when (and (equal session canvas-browser--session)
                             (or (null wanted) (funcall wanted)))
                    (canvas-browser--tell-again method params wanted (1- left)))))))
         (message "canvas-browser: chromium refused %s: %s" method says))))))

(defun canvas-browser--resize (width height &optional then)
  "Lay the page out for WIDTH by HEIGHT pixels, at the zoom of this buffer.
THEN is called once chromium has laid it out."
  (setq canvas-browser--size (cons width height))
  ;; `:json-false' is what `json-encode' writes as false; the keyword
  ;; `:false' would go as a string, and chromium answers "Invalid
  ;; parameters" to that.  `canvas-browser--apply-zoom' writes it.
  (canvas-browser--apply-zoom then))

(defvar-local canvas-browser--screencast nil
  "Whether chromium sends frames of this page.")

(defcustom canvas-browser-fresh-interval 2.0
  "Seconds between two looks at whether the window is still fresh.
A frame can be missed or dropped: a window that painted nothing since
the last look is asked for a picture of itself."
  :type 'number
  :group 'canvas-browser)

(defvar-local canvas-browser--fresh-frames 0
  "The count of frames at the last look at this window.")

(defvar-local canvas-browser--fresh-timer nil
  "The timer that looks whether this window is still fresh.")

(defconst canvas-browser--quiet-looks 4
  "Looks at a window that painted nothing before the reader is told.")

(defvar-local canvas-browser--quiet 0
  "How many looks in a row have found this window painting nothing.")

(defvar-local canvas-browser--focused nil
  "Whether this page has been told it has the focus.")

(defun canvas-browser--set-focus (focus)
  "Tell this page whether it has the FOCUS, unless it was told so already."
  (unless (eq (and focus t) canvas-browser--focused)
    (setq canvas-browser--focused (and focus t))
    (canvas-browser--tell "Emulation.setFocusEmulationEnabled"
                          (list :enabled (if focus t :json-false)))))

(defun canvas-browser--awaken (focus)
  "Tell this page it is active, and whether it has the FOCUS.
Chromium draws the tab that is in front and throttles the ones behind
it, down to freezing them; a page of its own buffer is in front of
nothing, and would stop drawing and stop answering.  The focus goes to
the page you are looking at alone: with several pages claiming it,
chromium sends the keys and the wheel to none of them."
  (canvas-browser--set-focus focus)
  (canvas-browser--tell-again "Page.setWebLifecycleState" (list :state "active")))

(defun canvas-browser--wake (focus)
  "Draw this page again: it is shown once more.
FOCUS is as in `canvas-browser--awaken\='."
  (canvas-browser--awaken focus)
  (unless canvas-browser--screencast
    (canvas-browser--start-screencast)))

(defvar canvas-browser--rest-target nil
  "The id of the empty tab that `canvas-browser--rest\=' puts in front, or nil.")

(defun canvas-browser--make-rest ()
  "Make the empty tab that stands in front of the pages that are put away.
A tab that is new is the tab in front."
  (canvas-browser-cdp-send "Target.createTarget" (list :url "about:blank")
                           (lambda (result)
                             (setq canvas-browser--rest-target (plist-get result :targetId)))))

(defun canvas-browser--put-in-front (target &optional refused)
  "Put the tab TARGET in front in the window of chromium.
REFUSED is called with what chromium says when it has no such tab; a
tab that is gone is no news for the reader."
  (canvas-browser-cdp-send "Target.activateTarget" (list :targetId target)
                           nil nil (or refused #'ignore)))

(defun canvas-browser--rest ()
  "Put an empty tab in front of this page in the window of chromium.
The tab in front there counts as looked at, whatever the windows of
Emacs show.  A page that is shown needs no place in front: it is told
that it has the focus, and chromium then takes it as looked at.

The tab of this page goes in front first.  A page that was clicked or
typed in holds the focus of chromium, and gives it up only to a tab
that takes its place in front.

The empty tab is made when it is first needed, and again when chromium
has it no more, as after chromium started again."
  (canvas-browser--put-in-front canvas-browser--target)
  (if canvas-browser--rest-target
      (canvas-browser--put-in-front canvas-browser--rest-target
                                    (lambda (_says) (canvas-browser--make-rest)))
    (canvas-browser--make-rest)))

(defun canvas-browser--sleep ()
  "Let this page be: no window shows it.
Chromium is left to throttle it as it throttles any tab behind another,
which keeps a handful of news sites from fighting for the machine.

A page that was looked at until now is looked at no more: it gives up
the focus, and an empty tab goes in front of it.  A site holds its
notifications back while it believes that you look at it, and a page
that was put away with the focus, as the tab in front, believed so for
good."
  (when (or canvas-browser--focused canvas-browser--screencast)
    (canvas-browser--rest))
  (canvas-browser--set-focus nil)
  (canvas-browser--stop-screencast))

(defun canvas-browser--follow-windows (&rest _)
  "Fit the pages that are shown to their windows, and let the others be.
Nothing is done while chromium is gone: the windows change as buffers
are killed, and a window change is no reason to start a browser."
  (when (canvas-browser-cdp-running-p)
    (canvas-browser--follow-shown-windows)))

(defun canvas-browser--follow-shown-windows ()
  "Wake the pages a window shows, and let the others be."
  (let* ((here (window-buffer (selected-window)))
         ;; The window you are in may hold the map, the minibuffer or
         ;; anything else while the windows are being changed, and a page
         ;; that lost the focus then would stop answering the keys.
         (page (and (buffer-live-p here)
                    (with-current-buffer here
                      (derived-mode-p 'canvas-browser-mode))
                    here)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (derived-mode-p 'canvas-browser-mode) canvas-browser--session)
          (cond
           ;; A window of its own comes first: an embedded page has one
           ;; while it is fullscreen.
           ((get-buffer-window buffer t)
            (canvas-browser--window-change (get-buffer-window buffer t))
            (canvas-browser--wake (if page (eq buffer page) canvas-browser--focused)))
           ((canvas-browser--host-of buffer)
            ;; An embedded page keeps the size it was given, and the keys
            ;; stay with its host.
            (if (canvas-browser--shown-p) (canvas-browser--wake nil) (canvas-browser--sleep)))
           (t (canvas-browser--sleep))))))))

(add-hook 'window-configuration-change-hook #'canvas-browser--follow-windows)
;; The size hook is called with the frame, not with a window, and its
;; buffer-local values are not run at all, so it is taken globally too.
(add-hook 'window-size-change-functions #'canvas-browser--follow-windows)

(defun canvas-browser--stop-screencast ()
  "Stop the frames of this page until they are asked for again."
  (when canvas-browser--screencast
    (canvas-browser--tell "Page.stopScreencast" nil)
    (setq canvas-browser--screencast nil)))

(defun canvas-browser--start-screencast ()
  "Ask chromium for a frame whenever the page changes.
A screencast that runs is stopped first, because chromium answers a
second one with an error that it is active already."
  (canvas-browser--stop-screencast)
  (setq canvas-browser--screencast t)
  ;; Chromium refuses a screencast for a page that is not the active
  ;; one of its tab, and a page that is refused gets no frame at all.
  (canvas-browser--tell-again "Page.startScreencast"
                              (list :format "jpeg" :quality canvas-browser-quality
                                    :maxWidth (car canvas-browser--size)
                                    :maxHeight (cdr canvas-browser--size))
                              (lambda () canvas-browser--screencast)))

(defun canvas-browser--listen ()
  "Take the events of this buffer's session."
  (let ((buffer (current-buffer)))
    (canvas-browser-cdp-listen
     canvas-browser--session "Page.screencastFrame"
     (lambda (params)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer (canvas-browser--frame params)))))
    (canvas-browser-cdp-listen
     canvas-browser--session "Inspector.detached"
     (lambda (params)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer (canvas-browser--detached params)))))
    (canvas-browser-cdp-listen
     canvas-browser--session "Page.loadEventFired"
     (lambda (params)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer (canvas-browser--loaded params)))))
    (canvas-browser-cdp-listen
     canvas-browser--session "Page.frameNavigated"
     (lambda (params)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer (canvas-browser--frame-navigated params)))))
    (canvas-browser-cdp-listen
     canvas-browser--session "Page.fileChooserOpened"
     (canvas-browser--here #'canvas-browser--file-chooser-opened))))

(defun canvas-browser--frame-navigated (params)
  "Take PARAMS of the event that says a frame of this page navigated.
Chromium restores a page from its back and forward cache without the
dark it forces on a page that has no dark of its own, though the page
still hears that the reader prefers dark.  So the dark is told again
after a restore of the page itself.  A navigation to a new page keeps it.

A frame at a site that may show notifications is given the way to tell
Emacs of them.

A new page has other fields, so the buffers of the fields of the page
before it go.  A page that is restored still has its fields."
  (canvas-browser--frame-may-notify (plist-get (plist-get params :frame) :url))
  (canvas-browser--mark-changed)
  (when (null (plist-get (plist-get params :frame) :parentId))
    (if (equal (plist-get params :type) "BackForwardCacheRestore")
        (canvas-browser--apply-dark)
      (canvas-browser--forget-fields))))

(defun canvas-browser--loaded (_params)
  "Ask the page that has loaded for its title, for the header line.
A page that was opened for a position in it scrolls there."
  (canvas-browser--read-title)
  (canvas-browser--go-to-wanted-position))

(defun canvas-browser--read-title ()
  "Ask the page for its title, and keep it.
Chromium names a page after its file until the page changes address
again, whatever the page calls itself, so the page is asked."
  (let ((buffer (current-buffer)))
    (canvas-browser--evaluate
     "document.title"
     (lambda (title)
       (when (and (buffer-live-p buffer) (stringp title) (not (string-empty-p title)))
         (with-current-buffer buffer
           (setq canvas-browser--title title)
           (canvas-browser--remember-page)
           (force-mode-line-update)))))))

(defun canvas-browser--detached (params)
  "Take PARAMS of the event that says chromium has let this page go.
The session is forgotten: chromium answers every command sent in it with
\"Not attached to an active page\", and the page is opened afresh when
something is next asked of it."
  (ignore params)
  (canvas-browser--forget-page))

(defun canvas-browser--forget-page ()
  "Forget the session of this buffer's page, which is opened afresh when
something is next asked of it."
  (canvas-browser-cdp-forget canvas-browser--session)
  (setq canvas-browser--session nil
        canvas-browser--target nil
        canvas-browser--screencast nil))

(defun canvas-browser--attach (&optional url)
  "Attach to this buffer's target, size it, and go to URL.
Without URL the page stays where it is, as a window another page opened
does: it was opened at its address."
  (let ((buffer (current-buffer)))
    (canvas-browser-cdp-send
     "Target.attachToTarget" (list :targetId canvas-browser--target :flatten t)
     (lambda (result)
       (when-let* ((session (plist-get result :sessionId))
                   ((buffer-live-p buffer)))
         (with-current-buffer buffer
           (setq canvas-browser--session session)
           (canvas-browser--listen)
           (canvas-browser--tell "Page.enable" nil)
           ;; Chromium's own file dialog opens on a display that
           ;; nobody sees, so the page hands the choice to Emacs.
           (canvas-browser--tell "Page.setInterceptFileChooserDialog" (list :enabled t))
           (canvas-browser--watch-notifications)
           ;; A page embedded in another buffer leaves the keys to the
           ;; page you browse.
           (canvas-browser--awaken (not canvas-browser--host))
           (canvas-browser--apply-dark)
           (canvas-browser--resize (car canvas-browser--size) (cdr canvas-browser--size))
           (when canvas-browser--host
             (canvas-browser--watch-fullscreen))
           (when url
             (canvas-browser--tell "Page.navigate" (list :url url)))
           (canvas-browser--start-screencast)))))))

(defun canvas-browser--buffer-of-target (target)
  "The page buffer that shows TARGET, or nil."
  (and target
       (seq-find (lambda (buffer)
                   (and (eq (buffer-local-value 'major-mode buffer) 'canvas-browser-mode)
                        (equal (buffer-local-value 'canvas-browser--target buffer) target)))
                 (buffer-list))))

(defun canvas-browser--watch-targets ()
  "Hear of the pages chromium opens and closes.
A button to sign in with Google or Apple opens a window of its own, and
a link may open a tab: each is a page of chromium's, which would else
open on a display nobody sees."
  (canvas-browser-cdp-listen nil "Target.targetCreated" #'canvas-browser--target-created)
  (canvas-browser-cdp-listen nil "Target.targetDestroyed" #'canvas-browser--target-destroyed)
  (canvas-browser-cdp-listen nil "Target.targetInfoChanged" #'canvas-browser--target-changed)
  (canvas-browser-cdp-send "Target.setDiscoverTargets" (list :discover t)))

(defun canvas-browser--target-changed (params)
  "Keep the address and the title of the page PARAMS names, if it is ours.
A page buffer takes the name of the address it shows, so that `C-x b\='
says what each page is; an embedded page keeps the name its host gave
it, since the host finds it by that name."
  (let* ((info (plist-get params :targetInfo))
         (buffer (canvas-browser--buffer-of-target (plist-get info :targetId)))
         (url (plist-get info :url))
         (title (plist-get info :title)))
    (if (not buffer)
        ;; A tab that chromium opened on its own may get its address
        ;; only now.
        (when (canvas-browser--own-tab-p info)
          (canvas-browser--show-tab (plist-get info :targetId) url))
      (with-current-buffer buffer
        ;; Chromium names a page after its file until it moves again, so
        ;; its title stands only for a new address, until the page has
        ;; loaded and given its own.
        (unless (equal url canvas-browser--url)
          ;; The part that was picked belonged to the page before.
          (canvas-browser--set-part nil)
          (setq canvas-browser--url url
                canvas-browser--title (and (stringp title) (not (string-empty-p title)) title))
          ;; A page that moves without a load, as an application does,
          ;; is a visit as well.
          (canvas-browser--remember-page))
        (unless canvas-browser--host
          (let ((name (canvas-browser--buffer-name canvas-browser--url)))
            ;; A name made unique with <2> is the name already.
            (unless (string-prefix-p name (buffer-name))
              (rename-buffer name t))))
        (force-mode-line-update)))))

(defvar canvas-browser--closed-targets nil
  "The ids of the tabs that page buffers closed of late, newest first.
Chromium may still tell of such a tab while it closes it.")

(defun canvas-browser--own-tab-p (info)
  "Whether INFO is of a tab that chromium opened on its own, at a web address.
The service worker of a site opens such a tab when its notification is
clicked.  It is no tab that a page opened, none that a page buffer made
or closed, no page that chromium loads ahead of a click, and not the
empty tab that stands in front of the pages that are put away."
  (let ((target (plist-get info :targetId)))
    (and (equal (plist-get info :type) "page")
         ;; A page that chromium loads ahead of a click has a subtype.
         (not (plist-get info :subtype))
         (not (plist-get info :openerId))
         (string-match-p "\\`https?://" (or (plist-get info :url) ""))
         (not (equal target canvas-browser--rest-target))
         (not (member target canvas-browser--closed-targets))
         (not (canvas-browser--buffer-of-target target)))))

(defun canvas-browser--target-created (params)
  "Show the page of PARAMS in a buffer of its own, if it belongs here.
A window that one of our pages opened does, and so does a tab that
chromium opened on its own at a web address.  A frame, a worker, or a
page that a page of nobody\='s here opened is left alone."
  (let* ((info (plist-get params :targetInfo))
         (target (plist-get info :targetId))
         (opener (canvas-browser--buffer-of-target (plist-get info :openerId))))
    (cond ((and opener
                (equal (plist-get info :type) "page")
                (not (canvas-browser--buffer-of-target target)))
           (canvas-browser--show-window target (plist-get info :url) opener))
          ((canvas-browser--own-tab-p info)
           (canvas-browser--show-tab target (plist-get info :url))))))

(defun canvas-browser--show-target (target url width height)
  "Show TARGET, a page that chromium has at URL, in a buffer of its own.
The page is laid out WIDTH by HEIGHT.  It is not sent anywhere: it is
at its address already.  Return the buffer."
  (let ((buffer (canvas-browser--page-buffer url)))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (canvas-browser--adopt width height)
      (setq canvas-browser--url url
            canvas-browser--target target)
      (canvas-browser--attach))
    buffer))

(defun canvas-browser--show-window (target url opener)
  "Show TARGET, a window the page of OPENER opened at URL; its buffer.
It is laid out at the size of the page that opened it, until a window of
Emacs shows it and it is fitted to that."
  (let ((size (buffer-local-value 'canvas-browser--size opener)))
    (canvas-browser--show-target target url (car size) (cdr size))))

(defun canvas-browser--show-tab (target url)
  "Show TARGET, a tab that chromium opened on its own at URL; its buffer.
A tab that no buffer holds stands in front in chromium where nobody
sees it.  Its site takes it as looked at, and holds back the
notifications that it shows only while nobody looks.  The tab is laid
out at the size of the window that is selected, until a window of Emacs
shows it and it is fitted to that."
  (canvas-browser--show-target target url
                               (window-body-width nil t) (window-body-height nil t)))

(defun canvas-browser--target-destroyed (params)
  "Take the buffer of the page PARAMS names away: the page closed itself.
A window to sign in closes itself once you have, and a browser takes its
window away then.  The page is gone, so chromium is not asked to close
it again."
  (when-let* ((buffer (canvas-browser--buffer-of-target (plist-get params :targetId))))
    (with-current-buffer buffer
      (setq canvas-browser--target nil))
    (kill-buffer buffer)))

(defun canvas-browser--open (url width height)
  "Open URL in this buffer, on a canvas WIDTH by HEIGHT; URL."
  (canvas-browser--start-chromium)
  (canvas-browser--watch-targets)
  (canvas-browser--adopt width height)
  (setq canvas-browser--url url)
  (let ((buffer (current-buffer)))
    (canvas-browser-cdp-send
     "Target.createTarget"
     ;; A tab behind another is hidden, and chromium stops drawing its
     ;; video.  A page embedded elsewhere is in no window of Emacs that
     ;; brings it to the front, so it gets a window of its own.
     (if canvas-browser--host
         (list :url "about:blank" :newWindow t)
       (list :url "about:blank"))
     (lambda (result)
       (when-let* ((target (plist-get result :targetId))
                   ((buffer-live-p buffer)))
         (with-current-buffer buffer
           (setq canvas-browser--target target)
           (canvas-browser--attach url))))))
  url)

(defvar canvas-browser--chooser)

(defun canvas-browser--release ()
  "Close this buffer's target and forget its session.
A choice of files that this page waited for is forgotten with it."
  (when (eq (plist-get canvas-browser--chooser :buffer) (current-buffer))
    (canvas-browser--attach-finish))
  (setq canvas-browser--screencast nil)
  (when canvas-browser--session
    (canvas-browser-cdp-forget canvas-browser--session))
  (when canvas-browser--target
    (setq canvas-browser--closed-targets
          (seq-take (cons canvas-browser--target canvas-browser--closed-targets) 20)))
  (when (and canvas-browser--target (canvas-browser-cdp-running-p))
    (canvas-browser-cdp-send "Target.closeTarget"
                             (list :targetId canvas-browser--target)))
  (when canvas-browser--context
    (canvas-cairo-destroy canvas-browser--context)
    (setq canvas-browser--context nil))
  (when canvas-browser--fresh-timer
    (cancel-timer canvas-browser--fresh-timer)
    (setq canvas-browser--fresh-timer nil))
  (when canvas-browser--crisp-timer
    (cancel-timer canvas-browser--crisp-timer)
    (setq canvas-browser--crisp-timer nil))
  (canvas-browser--forget-live)
  (canvas-browser--forget-fields)
  (canvas-browser--forget-files))

(defun canvas-browser--forget-files ()
  "Delete the pictures this page wrote, now that nobody reads them."
  (when canvas-browser--number
    (dolist (suffix canvas-browser--suffixes)
      (let ((file (canvas-browser--file "frame" suffix)))
        (when (file-exists-p file)
          (delete-file file))))
    (setq canvas-browser--last-frame nil)))

;;;; The keyboard and the mouse

(defvar-local canvas-browser--insert nil
  "Whether the keys of this buffer go to the page.")

(defconst canvas-browser--keys
  '(("Enter" 13 "\r") ("Tab" 9) ("Backspace" 8) ("Delete" 46) ("Escape" 27)
    ("ArrowLeft" 37) ("ArrowUp" 38) ("ArrowRight" 39) ("ArrowDown" 40)
    ("Home" 36) ("End" 35) ("PageUp" 33) ("PageDown" 34)
    ("a" 65 nil "KeyA") ("z" 90 nil "KeyZ"))
  "The keys chromium is sent: the name, the number Windows gives the key,
the text it types, if any, and its code where that is not its name.
Chromium edits a field by that number: a key sent by its name alone
reaches the page as an event that deletes and moves nothing.")

(defconst canvas-browser--modifier-keys '((control . 2) (shift . 8))
  "The bits of the modifiers DevTools knows, by their name in Emacs.")

(defconst canvas-browser--insert-keys
  '(("RET" "Enter") ("DEL" "Backspace") ("<backspace>" "Backspace")
    ("<deletechar>" "Delete") ("<delete>" "Delete")
    ("<up>" "ArrowUp") ("<down>" "ArrowDown")
    ("<left>" "ArrowLeft") ("<right>" "ArrowRight")
    ("<home>" "Home") ("<end>" "End") ("<prior>" "PageUp") ("<next>" "PageDown")
    ;; The editing keys of Emacs, as a field of a browser knows them.
    ("C-a" "Home") ("C-e" "End") ("C-f" "ArrowRight") ("C-b" "ArrowLeft")
    ("C-n" "ArrowDown") ("C-p" "ArrowUp")
    ("M-f" "ArrowRight" control) ("M-b" "ArrowLeft" control)
    ("M-<" "Home" control) ("M->" "End" control)
    ("C-<home>" "Home" control) ("C-<end>" "End" control)
    ("C-d" "Delete") ("M-d" "Delete" control) ("M-DEL" "Backspace" control)
    ;; The keys of a browser itself.  Emacs binds them to the same
    ;; motions and deletions, in the buffer, where a page has no text.
    ("C-<right>" "ArrowRight" control) ("C-<left>" "ArrowLeft" control)
    ("M-<right>" "ArrowRight" control) ("M-<left>" "ArrowLeft" control)
    ("C-<up>" "ArrowUp" control) ("C-<down>" "ArrowDown" control)
    ("C-<backspace>" "Backspace" control) ("C-<delete>" "Delete" control)
    ;; Shift with a motion marks, as it does in a browser.
    ("S-<left>" "ArrowLeft" shift) ("S-<right>" "ArrowRight" shift)
    ("S-<up>" "ArrowUp" shift) ("S-<down>" "ArrowDown" shift)
    ("S-<home>" "Home" shift) ("S-<end>" "End" shift)
    ("C-S-<left>" "ArrowLeft" control shift) ("C-S-<right>" "ArrowRight" control shift)
    ("C-S-<home>" "Home" control shift) ("C-S-<end>" "End" control shift)
    ;; A chat sends on Enter and breaks the line on Shift and Enter, and
    ;; many a form is sent with Control and Enter.
    ("S-<return>" "Enter" shift) ("C-<return>" "Enter" control) ("C-j" "Enter")
    ("C-v" "PageDown") ("M-v" "PageUp")
    ("M-{" "ArrowUp" control) ("M-}" "ArrowDown" control))
  "The keys of insert state that go to the page as a key rather than text.
Each is the key in Emacs, the name of the key it is in a browser, and
the modifiers held with it: a browser moves and deletes a word with
Control where Emacs does it with Meta, and goes to the ends of a field
with Control and Home or End.")

(defun canvas-browser--modifiers (names)
  "The DevTools bits of the modifiers NAMES."
  (cl-loop for name in names
           sum (or (alist-get name canvas-browser--modifier-keys)
                   (error "canvas-browser: no modifier named %S" name))))

(defun canvas-browser--key (key &optional modifiers answer)
  "Send KEY, a DevTools key name, to the page, down and up.
MODIFIERS, a list such as (shift), are held with it.  A key with a text
goes down as a key that types, unless Control is held with it: a page
that sends its form on Control and Enter must not get a line break as
well.  Any other key goes down as a raw key.  ANSWER, when given, is
called once chromium has handled the key's release."
  (pcase-let* ((`(,_ ,number ,text ,code)
                (or (assoc key canvas-browser--keys)
                    (error "canvas-browser: no key named %S" key)))
               (text (and (not (memq 'control modifiers)) text))
               (event (list :key key :code (or code key)
                            :windowsVirtualKeyCode number :nativeVirtualKeyCode number
                            :modifiers (canvas-browser--modifiers modifiers))))
    (canvas-browser--tell "Input.dispatchKeyEvent"
                          (append (if text
                                      (list :type "keyDown" :text text :unmodifiedText text)
                                    (list :type "rawKeyDown"))
                                  event))
    (canvas-browser--tell "Input.dispatchKeyEvent" (cons :type (cons "keyUp" event))
                          answer)))

(defun canvas-browser--character-number (character)
  "The number Windows gives the key of CHARACTER, or nil for none.
Letters, digits and the space have one; a page may look at it."
  (cond ((<= ?a character ?z) (upcase character))
        ((or (<= ?A character ?Z) (<= ?0 character ?9) (= character ?\s)) character)))

(defun canvas-browser--type-character (character)
  "Type CHARACTER into whatever has the focus of the page, as a key does.
Text put in without a key lands at the caret, and the caret stays behind
in a field the focus has moved on from: a letter typed on a button would
land in the field before it."
  (let* ((text (string character))
         (number (canvas-browser--character-number character))
         (event (append (list :key text)
                        (when number
                          (list :windowsVirtualKeyCode number :nativeVirtualKeyCode number)))))
    (canvas-browser--tell "Input.dispatchKeyEvent"
                          (append (list :type "keyDown" :text text :unmodifiedText text) event))
    (canvas-browser--tell "Input.dispatchKeyEvent" (append (list :type "keyUp") event))))

(defvar canvas-browser--focus-box)

(defvar-local canvas-browser--field-mark nil
  "Whether a region is being marked in the field that has the focus.")

(defconst canvas-browser--motion-keys
  '("ArrowLeft" "ArrowRight" "ArrowUp" "ArrowDown" "Home" "End" "PageUp" "PageDown")
  "The keys that move the caret of a field; held with Shift, they mark.")

(defconst canvas-browser--field-region-js
  "(function (collapse) {
     let e = document.activeElement;
     while (e && e.shadowRoot && e.shadowRoot.activeElement) e = e.shadowRoot.activeElement;
     if (!e) return '';
     let start = null;
     try { start = e.selectionStart; } catch (other) { start = null; }
     if (typeof start === 'number' && typeof e.value === 'string') {
       const text = e.value.substring(e.selectionStart, e.selectionEnd);
       if (collapse) {
         const at = e.selectionDirection === 'backward' ? e.selectionStart : e.selectionEnd;
         e.setSelectionRange(at, at);
       }
       return text;
     }
     const root = e.getRootNode && e.getRootNode();
     const s = root && root.getSelection ? root.getSelection() : getSelection();
     const text = s ? s.toString() : '';
     if (collapse && s && s.focusNode) s.collapse(s.focusNode, s.focusOffset);
     return text;
   })(%s)"
  "The JavaScript that gives the text marked in the field with the focus.
With true, the mark is dropped as well, and the caret stays where it
was, as it does after `M-w\=' in a buffer.")

(defun canvas-browser-field-set-mark ()
  "Set the mark in the field, so that the motions after it mark a region.
Pressed again, it drops the mark."
  (interactive)
  (if canvas-browser--field-mark
      (canvas-browser--field-drop-mark)
    (setq canvas-browser--field-mark t)
    (message "Mark set")))

(defun canvas-browser-field-mark-whole ()
  "Mark all of the field, as `C-x h\=' marks all of a buffer.
Control and A go to the page, which is how a browser marks all of a
field, so an editor that a page built itself takes it as well."
  (interactive)
  (canvas-browser--key "a" '(control))
  (setq canvas-browser--field-mark t))

(defun canvas-browser-field-undo ()
  "Undo the last change of the field, as a browser does on Control and Z."
  (interactive)
  (canvas-browser--key "z" '(control)))

(defun canvas-browser-field-redo ()
  "Do again what was undone in the field."
  (interactive)
  (canvas-browser--key "z" '(control shift)))

(defun canvas-browser--field-drop-mark ()
  "Drop the mark of the field, and the region it marks."
  (setq canvas-browser--field-mark nil)
  (canvas-browser--evaluate (format canvas-browser--field-region-js "true") #'ignore))

(defun canvas-browser--field-take (collapse then)
  "Put the region of the field in the kill ring, pulse the field, and THEN.
With COLLAPSE the region is dropped as well; THEN is called in this
buffer with the text."
  (let ((buffer (current-buffer))
        (box canvas-browser--focus-box))
    (setq canvas-browser--field-mark nil)
    (canvas-browser--evaluate
     (format canvas-browser--field-region-js (if collapse "true" "false"))
     (lambda (text)
       (if (and (stringp text) (not (string-empty-p text)))
           (progn (kill-new text)
                  (when box (canvas-browser--pulse buffer box))
                  (when (buffer-live-p buffer)
                    (with-current-buffer buffer (funcall then text))))
         (message "canvas-browser: nothing is marked in the field"))))))

(defun canvas-browser-field-copy ()
  "Copy the region of the field to the kill ring."
  (interactive)
  (canvas-browser--field-take
   t (lambda (text) (message "canvas-browser: copied %d characters" (length text)))))

(defun canvas-browser-field-cut ()
  "Move the region of the field to the kill ring."
  (interactive)
  (canvas-browser--field-take nil (lambda (_text) (canvas-browser--key "Backspace"))))

(defun canvas-browser-insert-quit ()
  "Drop the mark of the field when it is set, else give the keys back to Emacs.
This is `C-g\=' while typing into the page, as it is in a buffer: a
region goes first, and the second `C-g\=' leaves."
  (interactive)
  (if canvas-browser--field-mark
      (canvas-browser--field-drop-mark)
    (canvas-browser-normal-mode)))

(defun canvas-browser-self-insert ()
  "Type the key that called this command into the page.
The key is the last event, never the keys of the command: letters read
while a hint was chosen count among those, and would be typed with it."
  (interactive)
  (cl-assert (characterp last-command-event) nil
             "canvas-browser: %S types no text" last-command-event)
  ;; A letter takes the place of the region, which is gone with the mark.
  (setq canvas-browser--field-mark nil)
  (canvas-browser--type-character last-command-event))

(defun canvas-browser-send-key ()
  "Send the key that called this command to the page, as a browser knows it."
  (interactive)
  (let* ((keys (key-description (vector last-command-event)))
         (entry (or (assoc keys canvas-browser--insert-keys)
                    (error "canvas-browser: %s is no key of insert state" keys)))
         (key (nth 1 entry))
         (modifiers (nthcdr 2 entry)))
    ;; With the mark set a motion marks, as it does in a buffer, and any
    ;; other key, a deletion say, is done with the region.
    (if (and canvas-browser--field-mark (member key canvas-browser--motion-keys))
        (setq modifiers (cl-adjoin 'shift modifiers))
      (setq canvas-browser--field-mark nil))
    (canvas-browser--key key modifiers)))

(defconst canvas-browser--selection-change-js
  "new Promise(resolve => {
     let done = false;
     const finish = how => {
       if (done) return;
       done = true;
       document.removeEventListener('selectionchange', onChange, true);
       resolve(how);
     };
     const onChange = () => finish('changed');
     document.addEventListener('selectionchange', onChange, true);
     setTimeout(() => finish('unchanged'), 100);
   })"
  "The JavaScript of a promise that the next change of the selection keeps.
The page tells its listeners of a change in the order they were added,
so an editor that listens is told before this promise is kept.  When
nothing changes within a tenth of a second, the promise is kept as well.")

(defun canvas-browser--here (function)
  "A function that calls FUNCTION with its arguments in this buffer.
It does nothing once the buffer is gone."
  (let ((buffer (current-buffer)))
    (lambda (&rest arguments)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (apply function arguments))))))

(defun canvas-browser--after-selection-change (then)
  "Call THEN in this buffer once the page has told of a selection change.
Call this before the key that changes the selection is sent.  An editor
that keeps a selection of its own, as the one of Reddit does, learns of
a new selection a moment after the key that made it.  A second key sent
right after the first reaches it before that, and acts on the old
selection."
  (canvas-browser-cdp-send
   "Runtime.evaluate"
   (list :expression canvas-browser--selection-change-js :awaitPromise t :returnByValue t)
   (canvas-browser--here (lambda (_result) (funcall then)))
   canvas-browser--session))

(defconst canvas-browser--field-length-js
  "(function () {
     let e = document.activeElement;
     while (e && e.shadowRoot && e.shadowRoot.activeElement) e = e.shadowRoot.activeElement;
     if (!e) return 0;
     return (typeof e.value === 'string' ? e.value : e.innerText || '').length;
   })()"
  "The JavaScript that gives the length of the text of the field with the focus.")

(defun canvas-browser--kill-text (text append)
  "Put TEXT in the kill ring, at the end of the newest kill if APPEND."
  (if append (kill-append text nil) (kill-new text)))

(defun canvas-browser--field-length (then)
  "Call THEN in this buffer with the length of the text of the field."
  (canvas-browser--evaluate canvas-browser--field-length-js (canvas-browser--here then)))

(defun canvas-browser--kill-line-break (append)
  "Delete forward in the field, and kill a newline if a line break went.
The field tells that by its text, which is shorter after the key.  At
the end of the field nothing goes, and nothing is killed.  APPEND is as
in `canvas-browser--kill-text\='."
  (canvas-browser--field-length
   (lambda (before)
     (canvas-browser--key
      "Delete" nil
      (canvas-browser--here
       (lambda (_result)
         (canvas-browser--field-length
          (lambda (after)
            (when (< after before)
              (canvas-browser--kill-text "\n" append))))))))))

(defun canvas-browser--kill-marked (append)
  "Kill what is marked in the field, or the line break if nothing is.
APPEND is as in `canvas-browser--kill-text\='."
  (canvas-browser--evaluate
   (format canvas-browser--field-region-js "false")
   (canvas-browser--here
    (lambda (text)
      (if (and (stringp text) (not (string-empty-p text)))
          (progn (canvas-browser--kill-text text append)
                 (canvas-browser--key "Delete"))
        (canvas-browser--kill-line-break append))))))

(defun canvas-browser-kill-line ()
  "Kill from the cursor to the end of the line of the field.
At the end of a line, the line break goes, as with `C-k\=' in a buffer,
and two of them in a row make one kill.  Shift and End mark the rest of
the line, and Delete follows once the page has told of the mark."
  (interactive)
  (let ((append (eq last-command 'kill-region)))
    ;; The name by which Emacs knows a kill, so that the next kill adds
    ;; to this one, whichever command makes it.
    (setq this-command 'kill-region)
    (canvas-browser--after-selection-change
     (lambda () (canvas-browser--kill-marked append)))
    (canvas-browser--key "End" '(shift))))

(defun canvas-browser-yank ()
  "Type the newest kill of Emacs into the field."
  (interactive)
  (canvas-browser--tell "Input.insertText" (list :text (current-kill 0))))

(defun canvas-browser-copy-url ()
  "Copy the address of this page to the kill ring.
The key is `y\=', which copies the address of a link after `f\=' as well.
`w\=', the key of eww, is a key of every canvas buffer."
  (interactive)
  (unless canvas-browser--url
    (user-error "canvas-browser: this page has no address yet"))
  (kill-new canvas-browser--url)
  (message "canvas-browser: copied %s" canvas-browser--url))

(defun canvas-browser--wheel (x y delta)
  "Turn the wheel DELTA pixels at the page pixel X Y, down for a positive one.
Chromium sends the turn to whatever lies under that pixel, so a part of
the page that scrolls on its own scrolls when the pointer is over it."
  (canvas-browser--tell "Input.dispatchMouseEvent"
                        (list :type "mouseWheel" :x x :y y :deltaX 0 :deltaY delta)))

(defun canvas-browser--scroll (mode value expression)
  "Scroll the picked part of the page, or all of it.
MODE and VALUE say how the part moves, and EXPRESSION is the JavaScript
that moves the whole page instead, when no part is picked.  The page
scrolls itself: chromium animates a wheel event and swallows a small one,
and a key that does nothing for a second reads as a key that does nothing
at all."
  (if (plist-get canvas-browser--part :scrolls)
      (canvas-browser--scroll-part mode value)
    (canvas-browser--tell "Runtime.evaluate" (list :expression expression))))

(defun canvas-browser--scroll-by (delta)
  "Scroll DELTA pixels, further down for a positive DELTA."
  (canvas-browser--scroll "by" delta (format "window.scrollBy(0, %d)" delta)))

(defun canvas-browser-scroll-up ()
  "Scroll the page one screen further down."
  (interactive)
  (canvas-browser--scroll-by (- (cdr canvas-browser--size) 60)))

(defun canvas-browser-scroll-down ()
  "Scroll the page one screen back."
  (interactive)
  (canvas-browser--scroll-by (- 60 (cdr canvas-browser--size))))

(defun canvas-browser-beginning-of-page ()
  "Go to the top of the page, or of the part picked."
  (interactive)
  (canvas-browser--push-mark t)
  (canvas-browser--scroll "to" 0 "window.scrollTo(0, 0)"))

(defun canvas-browser-end-of-page ()
  "Go to the foot of the page, or of the part picked, as far as it reaches."
  (interactive)
  (canvas-browser--push-mark t)
  (canvas-browser--scroll "end" 0 "window.scrollTo(0, document.body.scrollHeight)"))

(defcustom canvas-browser-line-height 40
  "Pixels that one scroll of a line moves the page."
  :type 'integer
  :group 'canvas-browser)

(defun canvas-browser-scroll-line-up ()
  "Scroll the page one line further down."
  (interactive)
  (canvas-browser--scroll-by canvas-browser-line-height))

(defun canvas-browser-scroll-line-down ()
  "Scroll the page one line back."
  (interactive)
  (canvas-browser--scroll-by (- canvas-browser-line-height)))

(defun canvas-browser-wheel (event)
  "Scroll where EVENT, a turn of the wheel over the canvas, points.
The turn goes to the page at that very pixel, so the part under the
pointer scrolls, as it does in a window of chromium\='s own.  Emacs reports
a fast turn as a double or a triple event, and each one scrolls a line."
  (interactive "e")
  (let ((turn (event-basic-type event)))
    (cl-assert (memq turn '(wheel-up wheel-down)) nil
               "canvas-browser: %S is not a turn of the wheel" turn)
    (let ((at (posn-object-x-y (event-start event))))
      (canvas-browser--wheel (car at) (cdr at)
                             (if (eq turn 'wheel-up)
                                 (- canvas-browser-line-height)
                               canvas-browser-line-height)))))

(defconst canvas-browser--takes-typing-js
  "const takesTyping = e => !!e && (e.isContentEditable || e.tagName === 'TEXTAREA' ||
     (e.tagName === 'INPUT' &&
      !['button', 'submit', 'reset', 'checkbox', 'radio', 'file', 'image',
        'range', 'color', 'hidden'].includes((e.type || 'text').toLowerCase())));"
  "The JavaScript of `takesTyping\=', whether an element is a field to type in.")

(defconst canvas-browser--focus-script
  (concat "(function () {" canvas-browser--takes-typing-js "
     var e = document.activeElement, ox = 0, oy = 0;
     while (e) {
       if (e.shadowRoot && e.shadowRoot.activeElement) {
         e = e.shadowRoot.activeElement;
         continue;
       }
       if (e.tagName === 'IFRAME') {
         var inner = null;
         try { inner = e.contentDocument && e.contentDocument.activeElement; }
         catch (other) { inner = null; }
         if (inner && inner !== e.contentDocument.body) {
           var frame = e.getBoundingClientRect();
           ox += frame.left; oy += frame.top;
           e = inner;
           continue;
         }
       }
       break;
     }
     var box = null;
     if (e && e !== document.body && e !== document.documentElement) {
       var r = e.getBoundingClientRect();
       var left = Math.max(r.left + ox, 0), top = Math.max(r.top + oy, 0);
       var right = Math.min(r.right + ox, innerWidth), bottom = Math.min(r.bottom + oy, innerHeight);
       if (right - left >= 1 && bottom - top >= 1)
         box = [Math.round(left), Math.round(top), Math.round(right - left), Math.round(bottom - top)];
     }
     return {typing: takesTyping(e), box: box};
   })()")
  "The JavaScript that tells what has the focus: whether it takes typing,
and the box of it in the window, or null when nothing shows it.
The focus of a document is the web component that holds the field, or
the frame; the field itself is the focus of the component\'s shadow root,
or of the frame\'s document, so the script follows it down, and a frame
moves the box by where the frame stands.  A frame of another site cannot
be looked into, and `i\=' sends the keys there.")

(defun canvas-browser--click (x y)
  "Click the page at the pixel X Y, and type there if it takes typing.
The page is asked what has the focus once chromium has handled the
click: asked sooner, it names what had the focus before it."
  (canvas-browser--tell "Input.dispatchMouseEvent"
                        (list :type "mousePressed" :x x :y y
                              :button "left" :clickCount 1))
  (canvas-browser--tell "Input.dispatchMouseEvent"
                        (list :type "mouseReleased" :x x :y y
                              :button "left" :clickCount 1)
                        (canvas-browser--follow-focus-later))
  (canvas-browser--look-at-clipboard-later))

(defun canvas-browser--drag (x1 y1 x2 y2)
  "Drag the mouse over the page from the pixel X1 Y1 to X2 Y2.
The page gets a press, a move with the left button held, and a release,
so it marks what lies between as it does in any browser.  The focus is
followed as after a click: a drag in a field types there.  Text marked
outside a field goes to the caret of the page."
  (let ((left (list :button "left" :clickCount 1)))
    (canvas-browser--tell "Input.dispatchMouseEvent"
                          (append (list :type "mousePressed" :x x1 :y y1) left))
    (canvas-browser--tell "Input.dispatchMouseEvent"
                          (list :type "mouseMoved" :x x2 :y y2 :button "left" :buttons 1))
    (canvas-browser--tell "Input.dispatchMouseEvent"
                          (append (list :type "mouseReleased" :x x2 :y y2) left)
                          (canvas-browser--here #'canvas-browser--after-drag))))

(defun canvas-browser--after-drag (&rest _)
  "Follow the focus as after a click, and give marked text to the caret.
The page is asked for the text once it has said what has the focus, so
the caret knows whether the drag was in a field."
  (canvas-browser--follow-focus)
  (canvas-browser--evaluate-here "getSelection().toString()"
                                 #'canvas-browser--caret-take-region))

(defun canvas-browser--follow-focus-later ()
  "A function that follows the focus of this page when chromium answers.
Asked before chromium has handled a click or a tab, the page names what
had the focus before it."
  (let ((buffer (current-buffer)))
    (lambda (_result)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (canvas-browser--follow-focus))))))

(defconst canvas-browser--next-field-script
  (concat "(function (step) {" canvas-browser--takes-typing-js "
     const fields = [];
     const counts = e => takesTyping(e) && !e.disabled && !e.readOnly &&
       !(e.parentElement && e.parentElement.isContentEditable) &&
       e.getBoundingClientRect().width > 0 && e.getBoundingClientRect().height > 0;
     const walk = root => root.querySelectorAll('*').forEach(e => {
       if (counts(e)) fields.push(e);
       if (e.shadowRoot) walk(e.shadowRoot);
     });
     walk(document);
     if (!fields.length) return false;
     let focus = document.activeElement;
     while (focus && focus.shadowRoot && focus.shadowRoot.activeElement)
       focus = focus.shadowRoot.activeElement;
     const here = fields.indexOf(focus);
     const next = here < 0 ? (step > 0 ? 0 : fields.length - 1)
                           : (here + step + fields.length) %% fields.length;
     fields[next].focus();
     return true;
   })(%d)")
  "The JavaScript that focuses the field STEP fields on from the focus.
The fields are taken in the order of the page, web components included,
and the first and the last follow one another.")

(defun canvas-browser--go-to-field (step)
  "Focus the field STEP fields on from the one with the focus, and type there.
Only fields count, as with the `gi\=' of Vimium: a page puts buttons
between its fields, as Reddit puts the one that shows the password
between the name and the password, and a tab of the browser's own stops
at every one."
  (canvas-browser--tell "Runtime.evaluate"
                        (list :expression (format canvas-browser--next-field-script step))
                        (canvas-browser--follow-focus-later)))

(defun canvas-browser-next-field ()
  "Go to the next field of the page, and type there."
  (interactive)
  (canvas-browser--go-to-field 1))

(defun canvas-browser-previous-field ()
  "Go to the field before, and type there."
  (interactive)
  (canvas-browser--go-to-field -1))

(defun canvas-browser--follow-focus ()
  "Send the keys to the page while a box you can type in has the focus.
A click in a field is how a reader says they want to type there, and a
click anywhere else is how they say they have finished.  The eye is
drawn from where the focus was to where it is."
  (let ((buffer (current-buffer)))
    (canvas-browser--evaluate
     canvas-browser--focus-script
     (lambda (focus)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (if (eq (plist-get focus :typing) t)
               (unless canvas-browser--insert (canvas-browser-insert-mode))
             (when canvas-browser--insert (canvas-browser-normal-mode)))
           (canvas-browser--focus-moved (plist-get focus :box))))))))

(defvar-local canvas-browser--focus-box nil
  "The box of what had the focus of this page when last asked, or nil.")

(defcustom canvas-browser-focus-function #'canvas-browser--fly-focus
  "Function that draws the eye from where the focus was to where it is.
It is called in the page buffer with the two boxes, plists of x, y, w
and h in the pixels of the page, when a click or a tab moves the focus,
and when the caret of the page moves.
The default flies smear-cursor\='s cursor from one to the other while
smear-cursor is on, and with it off does nothing.  nil never flies."
  :type '(choice (const nil) function)
  :group 'canvas-browser)

(defun canvas-browser--box-of (box)
  "BOX, (X Y W H) as the page gives it, as a plist; nil for anything else.
The page gives null for a box that nothing shows."
  (and (consp box)
       (list :x (nth 0 box) :y (nth 1 box) :w (nth 2 box) :h (nth 3 box))))

(defun canvas-browser--fly (from to)
  "Draw the eye from FROM to TO, boxes of this page, when both are known."
  (when (and from to canvas-browser-focus-function (not (equal from to)))
    (funcall canvas-browser-focus-function from to)))

(defun canvas-browser--focus-moved (box)
  "Keep BOX, (X Y W H) or not a list, as the focus, and fly to it.
A focus the reader cannot see is forgotten, so that the next one is not
flown to from a place out of sight."
  (let ((to (canvas-browser--box-of box)))
    (canvas-browser--fly canvas-browser--focus-box to)
    (setq canvas-browser--focus-box to)))

(defun canvas-browser-click (event)
  "Click the page where EVENT, a click on the canvas, points."
  (interactive "e")
  (let ((at (posn-object-x-y (event-start event))))
    (canvas-browser--click (car at) (cdr at))))

(defun canvas-browser--page-pixel (position)
  "The pixel of the page that POSITION, a mouse position, is on, as (X . Y).
Nil when the position is on something else in the window."
  (and (posn-image position) (posn-object-x-y position)))

(defun canvas-browser-drag (event)
  "Drag the mouse over the page as EVENT, a drag on the canvas, did.
The page marks the text between the two ends.  In a field, `M-w\=' then
copies it."
  (interactive "e")
  (let ((from (canvas-browser--page-pixel (event-start event)))
        (to (canvas-browser--page-pixel (event-end event))))
    (unless (and from to)
      (user-error "canvas-browser: the drag ended off the page"))
    (canvas-browser--drag (car from) (cdr from) (car to) (cdr to))))

(defun canvas-browser-back ()
  "Go back in the history of the page."
  (interactive)
  (canvas-browser--tell "Runtime.evaluate" (list :expression "history.back()")))

(defun canvas-browser-forward ()
  "Go forward in the history of the page."
  (interactive)
  (canvas-browser--tell "Runtime.evaluate" (list :expression "history.forward()")))

(defcustom canvas-browser-search-url "https://duckduckgo.com/?q=%s"
  "Where words that are not a URL are searched for."
  :type 'string
  :group 'canvas-browser)

(defconst canvas-browser--local-host "\\`\\(localhost\\|127\\.0\\.0\\.1\\|\\[::1\\]\\)\\([:/]\\|\\'\\)"
  "How an address of this machine starts, which http serves.")

(defun canvas-browser--url-of (text)
  "TEXT as a URL: its own scheme, else one that fits, else a search.
A local address gets http, since a server on this machine rarely has a
certificate, and anything else gets https."
  (let ((text (string-trim text)))
    (cond ((string-match-p canvas-browser--local-host text) (concat "http://" text))
          ;; A port is no scheme: localhost:8080 is a host, mailto:a@b is not.
          ((string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:\\(//\\|[^0-9]\\)" text) text)
          ((string-match-p "[ \t]" text)
           (format canvas-browser-search-url (url-hexify-string text)))
          (t (concat "https://" text)))))

(defun canvas-browser--snap-can-read-p (file)
  "Whether a snap chromium can read FILE: a file of the home, in no hidden
directory of it."
  (let ((relative (file-relative-name (expand-file-name file) (expand-file-name "~"))))
    (not (or (string-prefix-p "../" relative)
             (string-match-p "\\(\\`\\|/\\)\\." relative)))))

(defun canvas-browser--copy-for-snap (file)
  "Copy FILE where a snap chromium can read it; return the copy."
  (let ((directory (expand-file-name "canvas-browser-files" (canvas-browser-cdp-snap-home))))
    (make-directory directory t)
    (let ((copy (expand-file-name (file-name-nondirectory file) directory)))
      (copy-file file copy t)
      copy)))

(defun canvas-browser--readable-url (url)
  "URL as the chromium in use can read it.
A snap chromium has a /tmp of its own and reads no hidden directory of
the home.  So a local file elsewhere, such as a page another package
wrote to /tmp, is copied to the snap's own directory first.  Only the
file is copied, so a page that loads files beside it finds none."
  (let* ((parsed (and (string-prefix-p "file://" url) (url-generic-parse-url url)))
         (file (and parsed (url-unhex-string (url-filename parsed)))))
    (if (and file (canvas-browser-cdp-snap-p) (not (canvas-browser--snap-can-read-p file)))
        (concat "file://" (canvas-browser--copy-for-snap file)
                (if (url-target parsed) (concat "#" (url-target parsed)) ""))
      url)))

(defun canvas-browser--reachable-url (text)
  "TEXT as a URL the chromium in use can reach; see `canvas-browser--url-of'."
  (canvas-browser--readable-url (canvas-browser--url-of text)))

(defun canvas-browser-open-url (url)
  "Go to URL in this buffer.
A URL without a scheme gets one, and words become a search."
  (interactive "sURL: ")
  (setq canvas-browser--url (canvas-browser--reachable-url url))
  (canvas-browser--tell "Page.navigate" (list :url canvas-browser--url)))

;;;; Link hints

(defcustom canvas-browser-hint-keys "asdfghjkl"
  "The letters that a hint is made of."
  :type 'string
  :group 'canvas-browser)

(defconst canvas-browser--reach-js
  "const walk = (root, selector, visit) => {
     root.querySelectorAll(selector).forEach(visit);
     root.querySelectorAll('*').forEach(e => { if (e.shadowRoot) walk(e.shadowRoot, selector, visit); });
   };
   const up = n => n.assignedSlot || n.parentNode || n.host;
   const hit = (x, y) => {
     let e = document.elementFromPoint(x, y);
     while (e && e.shadowRoot) {
       const inner = e.shadowRoot.elementFromPoint(x, y);
       if (!inner || inner === e) break;
       e = inner;
     }
     return e;
   };
   const shown = e => {
     const r = e.getBoundingClientRect();
     const left = Math.max(r.left, 0), top = Math.max(r.top, 0);
     const right = Math.min(r.right, innerWidth), bottom = Math.min(r.bottom, innerHeight);
     return {x: Math.round(left), y: Math.round(top),
             w: Math.round(right - left), h: Math.round(bottom - top)};
   };
   const middle = box => [box.x + box.w / 2, box.y + box.h / 2];"
  "The JavaScript both sets of hints are found with.
`walk\\=' looks into the open shadow root of every web component, since
`querySelectorAll\\=' stops at one, and the fields of a login form may sit
inside.  `up\\=' goes to the parent a click passes through, the slot a web
component puts the page\\='s own text in included.  `hit\\=' is what a click
at a point reaches, inside web components too, and `shown\\=' is the part
of an element inside the window.")

(defconst canvas-browser--boxes-script
  (concat "(function () {" canvas-browser--reach-js "
     const found = new Set();
     walk(document, 'a,button,input,select,textarea,iframe,[onclick],' +
          '[role=button],[role=link],[role=checkbox],[role=radio],[role=switch],' +
          '[role=tab],[role=menuitem],[role=menuitemcheckbox],[role=menuitemradio],' +
          '[role=option],[role=combobox],[role=textbox],[role=searchbox],' +
          '[role=slider],[role=spinbutton],[role=treeitem]',
          e => { if (e.tagName !== 'IFRAME' || e.getBoundingClientRect().height <= 100) found.add(e); });
     const pointer = e => getComputedStyle(e).cursor === 'pointer';
     const half = innerWidth * innerHeight / 2;
     const pointerRoots = (e, parentPointer) => {
       if (found.has(e)) return;
       const here = pointer(e);
       if (here && !parentPointer && e !== document.body) {
         const box = shown(e);
         if (box.w >= 5 && box.h >= 5 && box.w * box.h <= half) found.add(e);
         return;
       }
       if (e.shadowRoot) for (const child of e.shadowRoot.children) pointerRoots(child, here);
       for (const child of e.children) pointerRoots(child, here);
     };
     pointerRoots(document.documentElement, false);
     const owner = e => {
       for (let n = e; n; n = up(n)) if (found.has(n)) return n;
       return null;
     };
     const reached = e => {
       const box = shown(e);
       if (box.w < 5 || box.h < 5) return null;
       const [x, y] = middle(box);
       const h = hit(x, y);
       if (!h) return null;
       const label = h.closest && h.closest('label');
       return {box, owner: label && label.control === e ? e : owner(h)};
     };
     const places = new Map();
     const covered = [];
     found.forEach(e => {
       const at = reached(e);
       if (!at) return;
       if (at.owner === e) places.set(e, at.box);
       else covered.push(at);
     });
     covered.forEach(at => {
       if (at.owner && !places.has(at.owner)) places.set(at.owner, at.box);
     });
     window.__canvasBrowserTargets = Array.from(places.keys());
     return Array.from(places.values());
   })()")
  "The JavaScript that gives the boxes of everything a click can reach.
A region the page shows the pointer over counts as well, as a script
that listens for clicks marks one: the outermost element of it, unless
it fills half the window.  A picture of Reddit opens so.
A thing counts where a click in the middle of what shows of it reaches
it, or reaches its label.  A thing covered there by another thing that
can be clicked gives its place to that one, which a click there reaches:
the link over a post of Reddit covers its title, and the title is where
that link takes its hint.  Otherwise what lies outside the window, under a
dialog, or around something else that can be clicked, as a link around
a button does, takes no hint.  A hint spent on those is a hint the
things that can be clicked go without.  A role counts only where it says
the thing does something, as Vimium has it: an icon or a heading that
says it only looks takes none, and inside a button it would crowd the
button.  Nor does a thing of a few pixels, as a tracker is.  A frame the
size of a button counts, since a click in it reaches what it holds, as
with Google\\='s button to sign in; a larger one is an advertisement.  The
things are kept in the page, in the order of their boxes, so that the
action of a hint can ask for the address or the text of one.")

(defconst canvas-browser--blocks-script
  (concat "(function () {" canvas-browser--reach-js "
     const blocks = 'header,nav,main,aside,footer,article,section,form,dialog,table,pre,' +
       'figure,img,video,canvas,blockquote,iframe,[role=dialog],[role=article],' +
       '[role=region],[role=main],[role=navigation],[role=complementary],' +
       '[role=banner],[role=contentinfo],[role=feed]';
     const inside = (outer, e) => { for (let n = e; n; n = up(n)) if (n === outer) return true; return false; };
     const same = (a, b) => Math.abs(a.x - b.x) < 3 && Math.abs(a.y - b.y) < 3 &&
                            Math.abs(a.w - b.w) < 3 && Math.abs(a.h - b.h) < 3;
     const targets = [], boxes = [];
     walk(document, blocks, e => {
       const box = shown(e);
       if (box.w < 40 || box.h < 20) return;
       const [x, y] = middle(box);
       const h = hit(x, y);
       if (!h || !inside(e, h)) return;
       if (boxes.some(other => same(other, box))) return;
       targets.push(e);
       boxes.push(box);
     });
     window.__canvasBrowserTargets = targets;
     return boxes;
   })()")
  "The JavaScript that gives the boxes of the blocks of a page, to copy.
A block is a part the page itself marks as one: a header, a sidebar, an
article, a dialog, a table, a piece of code, a picture.  Only what shows
counts, so a block under a dialog takes no hint, and a block whose box
is that of one taken already is the same picture and takes none.")

(defun canvas-browser--evaluate (script answer)
  "Run SCRIPT in the page, and call ANSWER with the value it gives."
  (canvas-browser-cdp-send
   "Runtime.evaluate"
   (list :expression script :returnByValue t)
   (lambda (result) (funcall answer (plist-get (plist-get result :result) :value)))
   canvas-browser--session))

(defun canvas-browser--evaluate-here (script answer)
  "Run SCRIPT in the page, and call ANSWER in this buffer with its value.
Chromium answers in whichever buffer is current when its answer comes,
and a buffer killed before then gets none."
  (let ((buffer (current-buffer)))
    (canvas-browser--evaluate
     script
     (lambda (value)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (funcall answer value)))))))

(defun canvas-browser--boxes (answer)
  "Call ANSWER with the boxes of the page, each a plist of x, y, w and h."
  (canvas-browser--evaluate canvas-browser--boxes-script answer))

(defun canvas-browser--hint-letters (count)
  "COUNT hints of `canvas-browser-hint-keys\=', all of one length.
The length is the shortest that names COUNT boxes, so no hint is the
start of another, and a box past the last hint of two letters gets one
of three rather than none."
  (let* ((keys (mapcar #'char-to-string canvas-browser-hint-keys))
         (hints keys))
    (cl-assert (> (length keys) 1) nil
               "canvas-browser: `canvas-browser-hint-keys' needs two letters at least")
    (while (< (length hints) count)
      (setq hints (cl-loop for hint in hints
                           append (mapcar (lambda (key) (concat hint key)) keys))))
    (seq-take hints count)))

(defun canvas-browser--box-middle (box)
  "The middle of BOX, a plist of x, y, w and h, as (X . Y).
The client parses JSON into plists, so a box is one too."
  (cons (+ (plist-get box :x) (/ (plist-get box :w) 2))
        (+ (plist-get box :y) (/ (plist-get box :h) 2))))

(defcustom canvas-browser-hint-font "Sans Bold 11"
  "The font a hint is written in, as pango describes a font."
  :type 'string
  :group 'canvas-browser)

(defconst canvas-browser--hint-colour #xFFFFD633
  "The colour a hint is written on, as ARGB32.")

(defun canvas-browser--hint-size (hint)
  "The (W . H) of the label of HINT on the canvas of this buffer."
  (let ((size (canvas-cairo-text-size canvas-browser--context hint canvas-browser-hint-font)))
    (cons (+ 6 (car size)) (+ 4 (cdr size)))))

(defun canvas-browser--rects-meet-p (a b)
  "Whether the rectangles A and B, each (X Y W H), overlap."
  (pcase-let ((`(,ax ,ay ,aw ,ah) a)
              (`(,bx ,by ,bw ,bh) b))
    (and (< ax (+ bx bw)) (< bx (+ ax aw)) (< ay (+ by bh)) (< by (+ ay ah)))))

(defun canvas-browser--hint-places (boxes sizes)
  "Where the label of each of BOXES goes, as (X . Y), in their order.
SIZES are the (W . H) of the labels.  A label goes at the top left corner
of its box.  The small boxes are placed first, and a label whose corner a
label placed before it covers goes in the middle of its box instead,
where a click on it lands anyway: two labels at one corner hide one, as
a post of Reddit that is a link hides under the link of its author."
  (let ((places (make-vector (length boxes) nil))
        (taken nil))
    (dolist (place (sort (number-sequence 0 (1- (length boxes)))
                         (lambda (i j) (< (canvas-browser--box-area (nth i boxes))
                                          (canvas-browser--box-area (nth j boxes))))))
      (let* ((box (nth place boxes))
             (size (nth place sizes))
             (corner (cons (plist-get box :x) (plist-get box :y)))
             (at (if (cl-some (lambda (rect)
                                (canvas-browser--rects-meet-p
                                 (list (car corner) (cdr corner) (car size) (cdr size)) rect))
                              taken)
                     (let ((middle (canvas-browser--box-middle box)))
                       (cons (- (car middle) (/ (car size) 2))
                             (- (cdr middle) (/ (cdr size) 2))))
                   corner)))
        (aset places place at)
        (push (list (car at) (cdr at) (car size) (cdr size)) taken)))
    (append places nil)))

(defun canvas-browser--draw-hints (boxes hints)
  "Draw HINTS over BOXES on the canvas of this buffer."
  (cl-loop for at in (canvas-browser--hint-places boxes (mapcar #'canvas-browser--hint-size hints))
           for hint in hints
           do (canvas-browser--draw-hint (car at) (cdr at) hint))
  (canvas-refresh canvas-browser--canvas)
  (force-window-update (current-buffer)))

(defun canvas-browser--set-colour (context colour)
  "Have CONTEXT draw in COLOUR, an ARGB32."
  (canvas-cairo-set-color context
                          (/ (ash (logand colour #xFF0000) -16) 255.0)
                          (/ (ash (logand colour #xFF00) -8) 255.0)
                          (/ (logand colour #xFF) 255.0)
                          (/ (ash (logand colour #xFF000000) -24) 255.0)))

(defun canvas-browser--draw-hint (x y hint)
  "Draw HINT at X Y: its letters on a box of the hint's colour."
  (let ((context canvas-browser--context)
        (size (canvas-browser--hint-size hint)))
    (canvas-browser--set-colour context canvas-browser--hint-colour)
    (canvas-cairo-rectangle context x y (car size) (cdr size))
    (canvas-cairo-fill context)
    (canvas-cairo-set-color context 0 0 0 1)
    (canvas-cairo-text context (+ x 3) (+ y 2) hint canvas-browser-hint-font)))

(defun canvas-browser--check-actions (actions)
  "Signal an error when a key of ACTIONS, or `?', is a letter of the hints.
A key that is both would pick an action or name a hint, and nobody could
say which; avy refuses such a key as well.  `?' lists the actions."
  (when actions
    (when (seq-contains-p canvas-browser-hint-keys ??)
      (error "canvas-browser: `?' is a hint letter, and it lists the actions of a hint"))
    (when-let* ((clash (seq-find (lambda (action)
                                   (seq-contains-p canvas-browser-hint-keys (car action)))
                                 actions)))
      (error "canvas-browser: `%c' is a hint letter and the key of %s too"
             (car clash) (nth 1 clash)))))

(defun canvas-browser--hint-prompt (typed action help actions)
  "The prompt for a hint: the letters TYPED so far, and ACTION if chosen.
With HELP it lists ACTIONS, each key and what it does; with ACTIONS it
says that `?' lists them."
  (format "Hint%s: %s"
          (cond (action (concat ", " (nth 1 action)))
                (help (concat " (" (mapconcat (lambda (entry)
                                                (format "%c %s" (car entry) (nth 1 entry)))
                                              actions ", ")
                              ")"))
                (actions ", ? for actions")
                (t ""))
          typed))

(defun canvas-browser--read-hint (hints &optional actions)
  "Read letters until they name one of HINTS; (PLACE . ACTION), or nil.
Before the first letter, a key of ACTIONS picks what the hint does, as
the dispatch of avy does, and the prompt then names it; ACTION is that
entry, or nil for none.  `?\=' lists the actions in the prompt, as it
does in avy.  `ESC\\=' gives nil, and so does a key that is no
character, which is then run as the next key.  The hints are read while
chromium's answer is handled, between two commands, and Emacs counts
keys read there among the keys of the next one; they are forgotten as
such once read, and kept in the record of keys that `view-lossage\\='
shows."
  (canvas-browser--check-actions actions)
  (let ((typed "") (action nil) (help nil))
    (prog1
        (catch 'done
          (while t
            (let ((key (read-key (canvas-browser--hint-prompt typed action help actions))))
              (cond ((eq key ?\e) (throw 'done nil))
                    ((and actions (string-empty-p typed) (eq key ??))
                     (setq help t))
                    ((and (string-empty-p typed) (assq key actions))
                     (setq action (assq key actions)))
                    ;; A key that is no character names no hint: a key
                    ;; with Control or Meta, or a click.  It ends the
                    ;; hints, and then does what it does without them.
                    ((not (characterp key))
                     (push key unread-command-events)
                     (throw 'done nil))
                    (t
                     (setq typed (concat typed (char-to-string key)))
                     (when-let* ((place (cl-position typed hints :test #'equal)))
                       (throw 'done (cons place action)))
                     (unless (cl-some (lambda (hint) (string-prefix-p typed hint)) hints)
                       (throw 'done nil)))))))
      (clear-this-command-keys t))))

(defun canvas-browser--take-marks-off ()
  "Take what was drawn over the page off the canvas again.
The page is asked for a picture of itself.  The canvas holds the marks
and not the picture that was painted last, so that picture is forgotten:
a page that stands still gives the same one again, and it would be
dropped as a picture the canvas already holds."
  (setq canvas-browser--hinting nil
        canvas-browser--painted-mark nil)
  (canvas-browser--paint-window))

(defun canvas-browser--choose-box (boxes &optional actions)
  "Label BOXES on the canvas; (PLACE . ACTION) of the one named, or nil.
ACTIONS are the keys that may pick what the hint does first.
The labels are drawn over the page, so the page is asked for a picture of
itself afterwards, which takes them off again."
  (let ((hints (canvas-browser--hint-letters (length boxes))))
    (canvas-browser--draw-hints boxes hints)
    (setq canvas-browser--hinting t)
    (unwind-protect
        (canvas-browser--read-hint hints actions)
      (canvas-browser--take-marks-off))))

(defconst canvas-browser--hint-actions
  '((?y "copy address" canvas-browser--copy-address)
    (?w "copy text" canvas-browser--copy-text)
    (?c "copy picture" canvas-browser--copy-picture)
    (?o "open" canvas-browser--open-target)
    (?e "eww" canvas-browser--eww-target))
  "What a hint may do, each on a key pressed before its letters.
The key, the words the prompt shows, and the function, called with the
place of the thing named and its box.  None of these keys may be a
letter of the hints.")

(defun canvas-browser--act (chosen boxes default)
  "Do to the box CHOSEN names, of BOXES, what its action says, or DEFAULT.
CHOSEN is the (PLACE . ACTION) of `canvas-browser--read-hint\'."
  (funcall (or (nth 2 (cdr chosen)) default) (car chosen) (nth (car chosen) boxes)))

(defun canvas-browser--on-target (place script answer)
  "Run SCRIPT, a function of one element, on the thing at PLACE; to ANSWER.
The things are those the last hints were put on, kept in the page."
  (canvas-browser--evaluate
   (format "(%s)(window.__canvasBrowserTargets[%d])" script place) answer))

(defconst canvas-browser--address-js
  "e => { const link = e.closest ? e.closest('a[href]') : null;
          return (link && link.href) || e.href || e.src || ''; }"
  "The JavaScript function that gives the address of an element, or \"\".")

(defun canvas-browser--with-address (place then)
  "Call THEN with the address of the thing at PLACE, or say it has none."
  (canvas-browser--on-target
   place canvas-browser--address-js
   (lambda (address)
     (if (and (stringp address) (not (string-empty-p address)))
         (funcall then address)
       (message "canvas-browser: that has no address")))))

(defcustom canvas-browser-pulse-function #'canvas-browser--pulse-box
  "Function that draws the eye to what a hint copied, or nil for none.
It is called in the page buffer with the box of the thing copied, a
plist of x, y, w and h in the pixels of the page.  The default plays
smear-cursor's copy effect over the box while smear-cursor is on, and
with it off does nothing, as the pulse of canvas-diagram does."
  :type '(choice (const nil) function)
  :group 'canvas-browser)

(declare-function smear-cursor-flash-in-picture "smear-cursor"
                  (occasion pos rects &optional window))
(defvar smear-cursor-mode)
(defvar canvas-browser--zoom)

(defun canvas-browser--in-picture (box)
  "BOX of the page, a plist of x, y, w and h, as [X Y W H] in the picture.
The page is one glyph, the picture of the canvas, drawn at the zoom of
the page."
  (vector (* canvas-browser--zoom (plist-get box :x))
          (* canvas-browser--zoom (plist-get box :y))
          (* canvas-browser--zoom (plist-get box :w))
          (* canvas-browser--zoom (plist-get box :h))))

(defun canvas-browser--smear-window (function)
  "The window showing this page, when smear-cursor is on and has FUNCTION."
  (and (bound-and-true-p smear-cursor-mode)
       (fboundp function)
       (get-buffer-window (current-buffer) t)))

(defun canvas-browser--pulse-box (box)
  "Flash BOX of this page with smear-cursor's copy effect, if it is on."
  (when-let* ((window (canvas-browser--smear-window 'smear-cursor-flash-in-picture)))
    (smear-cursor-flash-in-picture 'copy (point-min)
                                   (list (canvas-browser--in-picture box)) window)))

(declare-function smear-cursor-fly-in-picture "smear-cursor"
                  (pos from to &optional window))

(defun canvas-browser--fly-focus (from to)
  "Fly smear-cursor's cursor from FROM to TO, boxes of this page, if it is on."
  (when-let* ((window (canvas-browser--smear-window 'smear-cursor-fly-in-picture)))
    (smear-cursor-fly-in-picture (point-min)
                                 (canvas-browser--in-picture from)
                                 (canvas-browser--in-picture to)
                                 window)))

(defun canvas-browser--pulse (buffer box)
  "Draw the eye to BOX of BUFFER's page, which was just copied."
  (when (and canvas-browser-pulse-function (buffer-live-p buffer))
    (with-current-buffer buffer
      (funcall canvas-browser-pulse-function box))))

(defun canvas-browser--click-target (_place box)
  "Click the middle of BOX."
  (let ((middle (canvas-browser--box-middle box)))
    (canvas-browser--click (car middle) (cdr middle))))

(defun canvas-browser--copy-address (place box)
  "Put the address of the thing at PLACE in the kill ring, and pulse BOX."
  (let ((buffer (current-buffer)))
    (canvas-browser--with-address
     place (lambda (address)
             (kill-new address)
             (canvas-browser--pulse buffer box)
             (message "canvas-browser: copied %s" address)))))

(defun canvas-browser--copy-text (place box)
  "Put the text of the thing at PLACE in the kill ring, and pulse BOX."
  (let ((buffer (current-buffer)))
    (canvas-browser--on-target
     place "e => (e.innerText || e.value || e.textContent || '').trim()"
     (lambda (text)
       (if (and (stringp text) (not (string-empty-p text)))
           (progn (kill-new text)
                  (canvas-browser--pulse buffer box)
                  (message "canvas-browser: copied %d characters" (length text)))
         (message "canvas-browser: that has no text"))))))

(defconst canvas-browser--shown-part-js
  "e => { const r = e.getBoundingClientRect();
          const left = Math.max(r.left, 0), top = Math.max(r.top, 0);
          const right = Math.min(r.right, innerWidth), bottom = Math.min(r.bottom, innerHeight);
          return [left, top, right - left, bottom - top, innerWidth]; }"
  "The JavaScript function that gives the part of an element in the window.
It answers with its place in the window, its size, and the width of the
window, all in the pixels of the page, which is what a picture of the
window is cut by.")

(defun canvas-browser--copy-picture (place _box)
  "Copy the picture of the thing at PLACE, as much of it as shows, as a PNG."
  (let ((buffer (current-buffer)))
    (canvas-browser--on-target
     place canvas-browser--shown-part-js
     (lambda (part)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (canvas-browser--capture-part part)))))))

(defun canvas-browser--crop-png (png x y width height)
  "The part of PNG, a string of bytes, WIDTH by HEIGHT at X Y, as a PNG."
  (let* ((size (or (canvas-browser--picture-size png)
                   (error "canvas-browser: that is no picture to cut")))
         (whole (make-temp-file "canvas-browser-whole-" nil ".png"))
         (part (make-temp-file "canvas-browser-part-" nil ".png"))
         (context (canvas-cairo-context
                   (list 'image :type 'canvas :id (make-symbol "canvas-browser-part")
                         :data-width width :data-height height))))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'binary))
            (write-region png nil whole nil 'silent))
          (canvas-cairo-image context whole (- x) (- y) (car size) (cdr size))
          (canvas-cairo-write-png context part)
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally part)
            (buffer-string)))
      (canvas-cairo-destroy context)
      (delete-file whole)
      (delete-file part))))

(defun canvas-browser--capture-part (part)
  "Copy PART of the window, (X Y WIDTH HEIGHT VIEW-WIDTH), as a PNG.
The part is cut in Emacs from a picture of the whole window, in its own
pixels, which are VIEW-WIDTH across the window.  Chromium cuts a part by
moving the view of the page to it for a while, and every frame and every
picture of that while shows the page shifted into the corner of the
window, or a part of it repeated across the window."
  (pcase-let ((`(,x ,y ,width ,height ,view) part)
              (buffer (current-buffer)))
    (canvas-browser-cdp-send
     "Page.captureScreenshot"
     (list :format "png")
     (lambda (result)
       (if-let* ((data (plist-get result :data)))
           (let* ((whole (base64-decode-string data))
                  (scale (/ (float (car (canvas-browser--picture-size whole))) view)))
             (canvas-keys-copy-png
              (canvas-browser--crop-png whole
                                        (round (* x scale)) (round (* y scale))
                                        (round (* width scale)) (round (* height scale))))
             (canvas-browser--pulse buffer (list :x x :y y :w width :h height))
             (message "canvas-browser: copied a picture of %d by %d"
                      (round width) (round height)))
         (message "canvas-browser: chromium took no picture of that")))
     canvas-browser--session)))

(defun canvas-browser--open-target (place _box)
  "Open the address of the thing at PLACE in a page buffer of its own."
  (canvas-browser--with-address place #'canvas-browser))

(defun canvas-browser--eww-target (place _box)
  "Open the address of the thing at PLACE in eww."
  (canvas-browser--with-address place #'eww))

(defun canvas-browser-hints ()
  "Label everything that can be clicked, and click the one you name.
A key of `canvas-browser--hint-actions\' pressed before the letters does
something else with it: copy its address, its text or its picture, or
open it in a buffer of its own or in eww."
  (interactive)
  (canvas-browser--boxes
   (lambda (boxes)
     (if (null boxes)
         (message "canvas-browser: nothing to click on this page")
       (when-let* ((chosen (canvas-browser--choose-box boxes canvas-browser--hint-actions)))
         (canvas-browser--act chosen boxes #'canvas-browser--click-target))))))

(defun canvas-browser--blocks (answer)
  "Call ANSWER with the boxes of the blocks of the page."
  (canvas-browser--evaluate canvas-browser--blocks-script answer))

(defun canvas-browser-copy-block ()
  "Label the blocks of the page, and copy the picture of the one you name.
A key of `canvas-browser--hint-actions\' pressed before the letters
copies something else of it, such as its text with `w\'.  This is what
`M-w\' does in a page; with a prefix it copies the whole window."
  (interactive)
  (canvas-browser--blocks
   (lambda (boxes)
     (if (null boxes)
         (message "canvas-browser: nothing on this page to copy")
       (when-let* ((chosen (canvas-browser--choose-box boxes canvas-browser--hint-actions)))
         (canvas-browser--act chosen boxes #'canvas-browser--copy-picture))))))

;;;; The part of the page that is picked

(defconst canvas-browser--parts-script
  (concat "(function () {" canvas-browser--reach-js "
     const marked = 'main,aside,nav,header,footer,article,section,dialog,' +
       '[role=main],[role=complementary],[role=navigation],[role=banner],' +
       '[role=contentinfo],[role=region],[role=feed],[role=dialog],[role=article]';
     const could = e => {
       const overflow = getComputedStyle(e).overflowY;
       return overflow === 'auto' || overflow === 'scroll';
     };
     const scrolls = e => e.scrollHeight > e.clientHeight + 4 && could(e);
     const pane = e => e.clientWidth * e.clientHeight * 10 >= innerWidth * innerHeight && could(e);
     const inside = (outer, e) => { for (let n = e; n; n = up(n)) if (n === outer) return true; return false; };
     const same = (a, b) => Math.abs(a.x - b.x) < 3 && Math.abs(a.y - b.y) < 3 &&
                            Math.abs(a.w - b.w) < 3 && Math.abs(a.h - b.h) < 3;
     const parts = [], boxes = [];
     walk(document, '*', e => {
       const moves = scrolls(e);
       if (!moves && !e.matches(marked) && !pane(e)) return;
       const box = shown(e);
       if (box.w < 40 || box.h < 40) return;
       if (!moves) {
         const [x, y] = middle(box);
         const h = hit(x, y);
         if (!h || !inside(e, h)) return;
       }
       box.scrolls = moves;
       const twin = boxes.findIndex(other => same(other, box));
       if (twin < 0) { parts.push(e); boxes.push(box); }
       else if (moves && !boxes[twin].scrolls) { parts[twin] = e; boxes[twin] = box; }
     });
     window.__canvasBrowserParts = parts;
     return boxes;
   })()")
  "The JavaScript that gives the boxes of the parts of a page, to pick one.
A part is what scrolls on its own, or what the page itself marks as a
part: its main content, a sidebar, a header, an article.  A pane is a
part too: a box that could scroll and takes a tenth of the window or
more, though its content fits now.  An application lays itself out in
such panes and marks none of them, and a short talk fits in its pane.
A small box that could scroll, as a line of code is, is no part.  A box
says whether its part scrolls.  Of two parts with one box, the one that
scrolls is kept.  The parts are kept in the page as well, so that a
later key finds one of them by the place it has there.")

(defun canvas-browser--parts (answer)
  "Call ANSWER with the boxes of the parts of the page."
  (canvas-browser--evaluate canvas-browser--parts-script answer))

(defun canvas-browser--part-js ()
  "The JavaScript that names the part that is picked, in the page."
  (format "(window.__canvasBrowserParts && window.__canvasBrowserParts[%d])"
          (plist-get canvas-browser--part :place)))

(defconst canvas-browser--scroll-part-script
  "(function (part, mode, value) {
     if (!part || !part.isConnected) return false;
     if (mode === 'by') part.scrollBy(0, value);
     else if (mode === 'end') part.scrollTo(0, part.scrollHeight);
     else part.scrollTo(0, value);
     return true;
   })(%s, '%s', %d)"
  "The JavaScript that scrolls the part picked.
It answers false when that part has gone, which a new page or a page that
drew itself again both do.")

(defun canvas-browser--scroll-part (mode value)
  "Scroll the part picked: MODE is `by\=', `to\=' or `end\=', by or to VALUE.
A part the page has thrown away is forgotten, and the keys go back to the
whole page: a key that silently does nothing reads as a broken key."
  (let ((buffer (current-buffer)))
    (canvas-browser--evaluate
     (format canvas-browser--scroll-part-script (canvas-browser--part-js) mode value)
     (lambda (there)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (unless (eq there t) (canvas-browser--forget-part))))))))

(defconst canvas-browser--mark-part-js
  (format "(function (part) {
     const old = window.__canvasBrowserMark;
     if (old) { cancelAnimationFrame(old.frame); old.box.remove(); }
     window.__canvasBrowserMark = null;
     if (!part || !part.isConnected) return false;
     const box = document.createElement('div');
     box.id = 'canvas-browser-mark';
     box.style.cssText = 'position:fixed;box-sizing:border-box;pointer-events:none;' +
       'z-index:2147483647;margin:0;padding:0;background:none;border:2px solid #%06X';
     document.documentElement.appendChild(box);
     const mark = {part, box, frame: 0, at: ''};
     const place = () => {
       if (!part.isConnected) { box.remove(); window.__canvasBrowserMark = null; return; }
       const r = part.getBoundingClientRect();
       const at = [r.left, r.top, r.width, r.height].join(' ');
       if (at !== mark.at) {
         mark.at = at;
         box.style.left = r.left + 'px';
         box.style.top = r.top + 'px';
         box.style.width = r.width + 'px';
         box.style.height = r.height + 'px';
       }
       mark.frame = requestAnimationFrame(place);
     };
     window.__canvasBrowserMark = mark;
     place();
     return true;
   })"
          (logand canvas-browser--hint-colour #xFFFFFF))
  "The JavaScript function that marks one element as the part that is picked.
The mark is a box of its own over the page, with a border in the colour
of a hint, at the place of the part.  It is no outline of the part
itself: what lies inside a part can cover such an outline, as the
replies of a thread on Reddit do.  The page looks where the part is
before each of its frames, and moves the box only when the part has
moved, so a page that stands still draws nothing for it.  A click goes
through the box.  The mark that was there before is taken away.
Called with null, the function takes the mark off.")

(defun canvas-browser--mark-part ()
  "Have the page outline the part that is picked, or no part when none is.
The page changes with it, as with any command, so a still picture that
was asked for before is dropped, and the frame that the change brings
is painted."
  (canvas-browser--forget-live)
  (canvas-browser--tell
   "Runtime.evaluate"
   (list :expression (concat canvas-browser--mark-part-js "("
                             (if canvas-browser--part (canvas-browser--part-js) "null")
                             ")"))))

(defun canvas-browser--set-part (part)
  "Make PART the part of this page that is picked, nil for the whole page.
The mark in the page and the header line follow.  The part that is
picked already is left as it is: a mark put where one was changes
nothing in the page, so no frame would come to paint."
  (unless (equal part canvas-browser--part)
    (setq canvas-browser--part part)
    (canvas-browser--mark-part)
    (force-mode-line-update)))

(defun canvas-browser--forget-part ()
  "Give the keys back to the whole page, and say why."
  (canvas-browser--set-part nil)
  (message "canvas-browser: that part has gone; the keys act on the whole page again"))

(defun canvas-browser--took-part (place boxes)
  "Pick the part at PLACE of BOXES, or the whole page for nil."
  (let ((scrolls (and place (eq (plist-get (nth place boxes) :scrolls) t))))
    (canvas-browser--set-part (and place (list :place place :scrolls scrolls)))
    (message
     (cond ((null place) "canvas-browser: no part is picked, the keys act on the whole page")
           (scrolls "canvas-browser: part picked: > sends it, and the scroll keys move it")
           (t "canvas-browser: part picked: > sends it; the scroll keys still move the whole page")))))

(defun canvas-browser--page-box ()
  "A box for the whole page, in the top corner, where few parts begin.
The page is labelled along with its parts, so that the keys go back to it
by a letter as well, the way every window `ace-window\=' labels carries
one, whether the keys are in it or not."
  (list :x (max 0 (- (car canvas-browser--size) 30)) :y 0 :w 30 :h 24))

(defun canvas-browser--box-path (box)
  "Make BOX, a plist of x, y, w and h, the path of the canvas of this buffer."
  (canvas-cairo-rectangle canvas-browser--context
                          (plist-get box :x) (plist-get box :y)
                          (plist-get box :w) (plist-get box :h)))

(defun canvas-browser--draw-outline (box)
  "Draw the outline of BOX on the canvas, in the colour of a hint."
  (canvas-browser--set-colour canvas-browser--context canvas-browser--hint-colour)
  (canvas-cairo-set-line-width canvas-browser--context 2)
  (canvas-browser--box-path box)
  (canvas-cairo-stroke canvas-browser--context))

(defun canvas-browser-pick-part ()
  "Outline and label the parts of the page, and pick the one you name.
A part is what scrolls on its own, or what the page marks as a part: its
main content, a sidebar, a header, an article.  The keys then act on
that part, the way `ace-window\=' hands the keys to the window you
name.  The part keeps an outline while it is picked.  The scroll keys
move it when it scrolls on its own, and `>\=' sends its text and not
that of the whole page.  The whole page carries the first letter, which
is the way back to it, and so is `C-g\=' once a part is picked.
`ESC\=' names nothing, and leaves what was picked as it was.  The wheel
needs none of this: it scrolls whatever the pointer is over."
  (interactive)
  (canvas-browser--parts
   (lambda (boxes)
     (if (null boxes)
         (message "canvas-browser: this page has no parts to pick")
       (mapc #'canvas-browser--draw-outline boxes)
       ;; A choice that is given up changes nothing.
       (when-let* ((place (car (canvas-browser--choose-box
                                (cons (canvas-browser--page-box) boxes)))))
         (canvas-browser--took-part (and (> place 0) (1- place)) boxes))))))

(defun canvas-browser-quit ()
  "Give the keys back to the whole page when a part of it is picked.
With no part picked, quit as `C-g\=' does in any buffer."
  (interactive)
  (if canvas-browser--part
      (canvas-browser--took-part nil nil)
    (keyboard-quit)))

(define-obsolete-function-alias 'canvas-browser-pick-scroller
  #'canvas-browser-pick-part "0.2.0")

;;;; Find in page, and the text of a page

(defvar-local canvas-browser--last-search nil
  "The last string searched for in this page.")

(defvar-local canvas-browser--find-index 0
  "Which hit of the last search the page shows.")

(defvar-local canvas-browser--find-regexp nil
  "Whether this page is searched for a regexp, and not for a string.")

(defvar-local canvas-browser--find-anchor nil
  "Where the hit that the page shows starts, as the page counts its text.
The page gives it with each answer, and the next search goes on from
it.  It is nil for a search that starts where the window stands.")

(defvar-local canvas-browser--find-ticket 0
  "The number of the newest search that this page was asked for.")

(defvar canvas-browser-find-history nil
  "What the pages were searched for.")

(defconst canvas-browser--paint-js "
  const paint = (ranges, current) => {
    const style = 'canvas-browser-find-style';
    if (!document.getElementById(style)) {
      const sheet = document.createElement('style');
      sheet.id = style;
      sheet.textContent = '::highlight(canvas-browser-find) { background: #ffd54f; color: #000; }' +
        '::highlight(canvas-browser-find-current) { background: #ff6d00; color: #000; }';
      document.documentElement.appendChild(sheet);
    }
    const all = ranges.length ? new Highlight(...ranges) : null;
    if (all) CSS.highlights.set('canvas-browser-find', all);
    else CSS.highlights.delete('canvas-browser-find');
    if (current) {
      const mark = new Highlight(current);
      mark.priority = 1;
      CSS.highlights.set('canvas-browser-find-current', mark);
    } else CSS.highlights.delete('canvas-browser-find-current');
    return all;
  };"
  "The JavaScript that paints ranges of a page as hits, and one as the hit shown.
The page paints them itself, with the highlight API of CSS:
`window.find\\=' answers in a headless chromium but leaves nothing to
see.  `paint\\=' gives the paint of the hits, so that it can be told
from the paint of a later search.")

(defconst canvas-browser--flat-js "
  const unread = new Set(['SCRIPT', 'STYLE', 'NOSCRIPT', 'TEMPLATE']);
  const unseen = (element, style) => {
    if (style.overflowX === 'visible' && style.clip === 'auto' && style.clipPath === 'none') return false;
    const box = element.getBoundingClientRect();
    return box.width <= 2 || box.height <= 2;
  };
  const eachShown = (root, see) => {
    const visit = node => {
      if (node.nodeType === 3) { see(node); return; }
      if (node.nodeType === 1) {
        if (unread.has(node.tagName)) return;
        const style = getComputedStyle(node);
        if (style.display !== 'contents'
            && (!node.checkVisibility({checkVisibilityCSS: true, visibilityProperty: true,
                                       opacityProperty: true})
                || unseen(node, style))) return;
        see(node);
        if (node.shadowRoot) { visit(node.shadowRoot); return; }
        if (node.tagName === 'SLOT') {
          const given = node.assignedNodes({flatten: true});
          if (given.length) { given.forEach(visit); return; }
        }
      } else if (node.nodeType !== 11) return;
      node.childNodes.forEach(visit);
    };
    visit(root);
  };
  const flatten = root => {
    const nodes = [];
    eachShown(root, node => { if (node.nodeType === 3 && node.textContent.trim()) nodes.push(node); });
    const above = node => node.parentNode || node.host;
    const block = node => {
      let e = above(node);
      while (e && e.nodeType === 1 && /^(inline|contents)/.test(getComputedStyle(e).display)) e = above(e);
      return e;
    };
    let flat = '', last = null;
    const starts = [], drops = [];
    const shaped = node => {
      const shape = getComputedStyle(above(node).nodeType === 1 ? above(node) : above(node).host).textTransform;
      const text = node.textContent.replace(/\\s+/g, ' ');
      return shape === 'uppercase' ? text.toUpperCase() : shape === 'lowercase' ? text.toLowerCase() : text;
    };
    for (const node of nodes) {
      const text = shaped(node);
      const here = block(node);
      if (flat && !flat.endsWith(' ') && !text.startsWith(' ') && here !== last) flat += ' ';
      const drop = (!flat || flat.endsWith(' ')) && text.startsWith(' ') ? 1 : 0;
      starts.push(flat.length);
      drops.push(drop);
      flat += text.slice(drop);
      last = here;
    }
    const point = at => {
      let i = starts.length - 1;
      while (i > 0 && starts[i] > at) i--;
      const raw = nodes[i].textContent;
      let left = at - starts[i] + drops[i], offset = 0;
      while (left > 0 && offset < raw.length) {
        if (/\\s/.test(raw[offset])) { while (offset < raw.length && /\\s/.test(raw[offset])) offset++; }
        else offset++;
        left--;
      }
      return [nodes[i], offset];
    };
    const range = (from, to) => {
      const [first, start] = point(from), [final, end] = point(to);
      const r = document.createRange();
      r.setStart(first, start);
      r.setEnd(final, end);
      return r;
    };
    return {flat, range};
  };
  const boxOf = range => {
    const box = range.getBoundingClientRect();
    return [Math.round(box.x), Math.round(box.y), Math.round(box.width), Math.round(box.height)];
  };
  const blinkOn = (range, ms) => {
    const mine = paint([range], null);
    setTimeout(() => {
      if (CSS.highlights.get('canvas-browser-find') === mine) CSS.highlights.delete('canvas-browser-find');
    }, ms);
  };
  const reveal = range => {
    const element = range.startContainer.parentElement || range.startContainer.parentNode.host;
    const origin = window.__canvasBrowserFindOrigin;
    if (origin) {
      for (let e = element; e; e = e.parentNode || e.host)
        if (e.nodeType === 1 && !origin.has(e)) origin.set(e, [e.scrollLeft, e.scrollTop]);
      if (!origin.has(window)) origin.set(window, [scrollX, scrollY]);
    }
    const box = range.getBoundingClientRect();
    const inside = box.top >= 0 && box.bottom <= innerHeight;
    element.scrollIntoView({block: inside ? 'nearest' : 'center', inline: 'nearest'});
  };"
  "The JavaScript that reads the text of a part of a page as one line.
`eachShown\\=' walks what an element shows, in the order of the page
and through web components too.  What the page clips to nothing is not
shown: a page keeps words for a screen reader in a box of one pixel,
and such words have no place that the eye can be drawn to.
`flatten\\=' gives the text that an element shows, with one space for
white space, and a way to make a
range of the page of a stretch of that text.  So a stretch can lie in
more than one piece of the page, as a sentence with a link in it does.
Text that the page shows in capitals is in capitals there.
`reveal\\=' scrolls a range into view, also in a part that scrolls on
its own, and moves nothing when the range is in view already.  While a
search runs, it notes where each thing stood before it scrolled it.
`boxOf\\=' gives the box of a range in the window, as (X Y W H), and
`blinkOn\\=' paints a range for some milliseconds.  It needs `paint\\='
of `canvas-browser--paint-js\\='.")

(defconst canvas-browser--find-script (concat "
(function (text, regexp, anchor, step) {" canvas-browser--paint-js canvas-browser--flat-js "
  if (!text) { paint([], null); return {count: 0}; }
  const view = flatten(document.body);
  const bare = regexp ? text.replace(/\\\\./g, '') : text;
  const exact = /[A-Z]/.test(bare);
  const hits = [];
  if (regexp) {
    let pattern;
    try { pattern = new RegExp(text, exact ? 'gu' : 'giu'); }
    catch (error) { return {error: error.message.replace(/^Invalid regular expression: /, '')}; }
    for (const match of view.flat.matchAll(pattern)) {
      if (match[0].length) hits.push([match.index, match.index + match[0].length]);
      if (hits.length >= 2000) break;
    }
  } else {
    const hay = exact ? view.flat : view.flat.toLowerCase();
    const needle = exact ? text : text.toLowerCase();
    for (let at = hay.indexOf(needle); at !== -1 && hits.length < 2000;
         at = hay.indexOf(needle, at + needle.length))
      hits.push([at, at + needle.length]);
  }
  if (!hits.length) { paint([], null); return {count: 0}; }
  const ranges = hits.map(hit => view.range(hit[0], hit[1]));
  let index = anchor === null
    ? ranges.findIndex(range => range.getBoundingClientRect().bottom >= 0)
    : hits.findIndex(hit => hit[0] >= anchor);
  if (index < 0) index = 0;
  index = (((index + step) %% hits.length) + hits.length) %% hits.length;
  if (!window.__canvasBrowserFindOrigin) window.__canvasBrowserFindOrigin = new Map();
  reveal(ranges[index]);
  paint(ranges, ranges[index]);
  return {count: hits.length, index, anchor: hits[index][0], box: boxOf(ranges[index])};
})(%s, %s, %s, %d)")
  "The JavaScript that searches a page and shows one of its hits.
It takes the text, whether the text is a regexp, where the search stands
and how many hits to step from there.  A search that stands nowhere yet
starts at the first hit that the window shows or that lies below it.
The text is looked for without regard to case, unless it has a capital
letter, as isearch does.  A regexp is one of JavaScript.  Every hit is
painted, and the one that is shown in a colour of its own.")

(defconst canvas-browser--find-end-script "
(function (back) {
  const origin = window.__canvasBrowserFindOrigin;
  delete window.__canvasBrowserFindOrigin;
  if (!back) return;
  CSS.highlights.delete('canvas-browser-find');
  CSS.highlights.delete('canvas-browser-find-current');
  if (origin) for (const [thing, [left, top]] of origin) thing.scrollTo(left, top);
})(%s)"
  "The JavaScript that ends a search of a page.
With true, the page scrolls back to where it stood when the search
began, and the paint is taken away: the search was given up.")

(defvar canvas-browser--find-page nil
  "The page buffer that is being searched from the minibuffer, or nil.")

(defvar canvas-browser--find-overlay nil
  "The overlay that says in the minibuffer what a search found.")

(defun canvas-browser--find-say (text)
  "Tell the reader TEXT about a search, or nothing for nil.
While the page is searched from the minibuffer, it stands after what is
typed there, and stays.  Else it is a message."
  (if (and canvas-browser--find-page (active-minibuffer-window))
      (with-current-buffer (window-buffer (active-minibuffer-window))
        (unless (overlayp canvas-browser--find-overlay)
          (setq canvas-browser--find-overlay (make-overlay (point-max) (point-max) nil t t)))
        (move-overlay canvas-browser--find-overlay (point-max) (point-max) (current-buffer))
        (overlay-put canvas-browser--find-overlay 'after-string
                     (and text (propertize (format " [%s]" text)
                                           'face 'minibuffer-prompt 'cursor t))))
    (when text (message "canvas-browser: %s" text))))

(defun canvas-browser--found-text (value string)
  "What to tell the reader of VALUE, the answer of a search for STRING.
It is nil for a search for nothing."
  (let ((count (or (plist-get value :count) 0))
        (mode (if canvas-browser--find-regexp "regexp, " "")))
    (cond ((plist-get value :error) (format "%s%s" mode (plist-get value :error)))
          ((string-empty-p string) nil)
          ((zerop count) (format "%sno hit" mode))
          (t (format "%shit %d of %d" mode (1+ (plist-get value :index)) count)))))

(defvar-local canvas-browser--find-box nil
  "The box of the hit that a search of this page showed last, or nil.")

(defun canvas-browser--land (box)
  "Draw the eye to BOX, where a search of this page landed.
BOX is (X Y W H) as the page gives it.  The first hit of a search is
flashed, as what a hint copied is, and the eye is flown from one hit to
the next, as it is when the focus moves: with the effects of
smear-cursor while that is on."
  (when-let* ((to (canvas-browser--box-of box)))
    (if canvas-browser--find-box
        (canvas-browser--fly canvas-browser--find-box to)
      (canvas-browser--pulse (current-buffer) to))
    (setq canvas-browser--find-box to)))

(defun canvas-browser--land-anew (box)
  "Draw the eye to BOX of this page, as to the first hit of a search."
  (setq canvas-browser--find-box nil)
  (canvas-browser--land box))

(defun canvas-browser--found (value string ticket)
  "Take VALUE, the answer of the page to the search for STRING with TICKET.
An answer to an older search than the newest one is dropped: it says
where the page stood, not where it stands."
  (when (= ticket canvas-browser--find-ticket)
    (when (plist-get value :anchor)
      (setq canvas-browser--find-anchor (plist-get value :anchor)
            canvas-browser--find-index (plist-get value :index)))
    (canvas-browser--land (plist-get value :box))
    (canvas-browser--find-say (canvas-browser--found-text value string))))

(defun canvas-browser--find (string step)
  "Search this page for STRING, and show the hit STEP hits from where it stands."
  (setq canvas-browser--last-search string)
  (let ((ticket (cl-incf canvas-browser--find-ticket)))
    (canvas-browser--tell
     "Runtime.evaluate"
     (list :expression (format canvas-browser--find-script
                               (json-encode string)
                               (if canvas-browser--find-regexp "true" "false")
                               (or canvas-browser--find-anchor "null")
                               step)
           :returnByValue t)
     (canvas-browser--here
      (lambda (result)
        (canvas-browser--found (plist-get (plist-get result :result) :value) string ticket))))))

(defun canvas-browser--find-typed (string)
  "Search this page for STRING, which was typed so far, from where it stands."
  (canvas-browser--find string 0))

(defun canvas-browser--find-step (step typed)
  "Step STEP hits in the search of this page for TYPED.
With nothing typed, give the last search of the page instead, to type:
so the key that steps takes the last search up again, as in isearch.
Else give nil."
  (if (and (string-empty-p typed) canvas-browser--last-search)
      canvas-browser--last-search
    (canvas-browser--find typed step)
    nil))

(defun canvas-browser--keep-origins ()
  "Have this page note where it stands before something scrolls it.
`canvas-browser--end-origins\=' can then put it back."
  (canvas-browser--tell "Runtime.evaluate"
                        (list :expression "window.__canvasBrowserFindOrigin = new Map()")))

(defun canvas-browser--end-origins (back)
  "Have this page forget where it stood; with BACK, it goes back there first.
It then also loses the paint of a search."
  (canvas-browser--tell "Runtime.evaluate"
                        (list :expression (format canvas-browser--find-end-script
                                                  (if back "true" "false")))))

(defun canvas-browser--find-end (string given-up)
  "End the search of this page for STRING.
With GIVEN-UP, the page goes back to where it stood and loses its paint.
Else STRING is kept as the last search, and the page stays."
  (setq canvas-browser--last-search (and (not given-up) (not (string-empty-p string)) string))
  (canvas-browser--end-origins given-up))

(defun canvas-browser--in-found-page (function)
  "Call FUNCTION in the page that is searched: with what is typed, or with nil.
From the minibuffer of a search it is called in the page of that search
with the text of the minibuffer, and what it gives is typed there when
it is a string.  In a page buffer it is called with nil."
  (if (and canvas-browser--find-page (minibufferp))
      (let ((typed (minibuffer-contents-no-properties)))
        (when-let* ((text (with-current-buffer canvas-browser--find-page
                            (funcall function typed))))
          (insert text)))
    (funcall function nil)))

(defun canvas-browser-find-next ()
  "Show the next hit of the search of this page."
  (interactive)
  (canvas-browser--in-found-page
   (lambda (typed)
     (canvas-browser--find-step 1 (or typed canvas-browser--last-search "")))))

(defun canvas-browser-find-previous ()
  "Show the hit before the one that the page shows."
  (interactive)
  (canvas-browser--in-found-page
   (lambda (typed)
     (canvas-browser--find-step -1 (or typed canvas-browser--last-search "")))))

(defun canvas-browser-find-toggle-regexp ()
  "Search this page for a regexp, or for a string again.
The regexp is one of JavaScript, which the page reads: a group is
\\(a|b), not \\\\(a\\\\|b\\\\)."
  (interactive)
  (canvas-browser--in-found-page
   (lambda (typed)
     (setq canvas-browser--find-regexp (not canvas-browser--find-regexp))
     (canvas-browser--find (or typed canvas-browser--last-search "") 0)
     nil)))

(defvar-keymap canvas-browser-find-map
  :doc "Keymap of the minibuffer while a page is searched."
  :parent minibuffer-local-map
  "C-s" #'canvas-browser-find-next
  "C-r" #'canvas-browser-find-previous
  "M-r" #'canvas-browser-find-toggle-regexp)

(defun canvas-browser--find-read ()
  "Read what to search this page for, and search it while it is typed.
The answer is what was typed.  `C-g\\=' is a quit, as in any minibuffer."
  (let ((canvas-browser--find-page (current-buffer)))
    (minibuffer-with-setup-hook
        (lambda ()
          (add-hook 'after-change-functions
                    (lambda (&rest _)
                      (let ((typed (minibuffer-contents-no-properties)))
                        (with-current-buffer canvas-browser--find-page
                          (canvas-browser--find-typed typed))))
                    nil t))
      (read-from-minibuffer "Find in page: " nil canvas-browser-find-map nil
                            'canvas-browser-find-history))))

(defun canvas-browser-find (&optional string)
  "Search the page while you type, as isearch searches a buffer.
Each letter that you type shows the next hit from where the page
stands, and every hit is painted.  `C-s\\=' and `C-r\\=' step to the next
hit and to the one before, and with nothing typed `C-s\\=' takes the
last search up again.  `M-r\\=' switches between a string and a regexp
of JavaScript.  `RET\\=' ends the search where it stands, and `C-g\\='
puts the page back where it was.  Capital letters make the search
exact about case.  A search that ends with `RET\\=' leaves a mark where
the page stood when it began.

From Lisp, with STRING, the page is searched for it at once."
  (interactive)
  (setq canvas-browser--find-anchor nil
        canvas-browser--find-box nil)
  (if string
      (canvas-browser--find string 0)
    (let ((typed ""))
      (canvas-browser--push-mark t)
      (condition-case nil
          (progn (setq typed (canvas-browser--find-read))
                 (canvas-browser--find-end typed nil))
        (quit (canvas-browser--find-end typed t)
              (canvas-browser--drop-mark)
              (signal 'quit nil))))))

;;;; Dark mode

(defcustom canvas-browser-dark nil
  "Whether a page is shown dark.
`canvas-browser-toggle-dark\=' sets it, and `C-x C-s\=' in the menu keeps
it, so that the pages you open after it are shown the same way."
  :type 'boolean
  :group 'canvas-browser)

(defun canvas-browser-toggle-dark ()
  "Turn dark mode on for this page, or off again.
The page hears that the reader prefers dark, and chromium darkens a page
that carries no dark of its own."
  (interactive)
  (setq canvas-browser-dark (not canvas-browser-dark))
  (canvas-browser--apply-dark)
  (message "canvas-browser: dark mode is %s" (if canvas-browser-dark "on" "off")))

(defun canvas-browser--apply-dark ()
  "Tell the page whether it is shown dark.
It is told again after a picture of the whole page: chromium draws that
picture with its own dark turned off, and leaves the page that way."
  (canvas-browser--tell "Emulation.setEmulatedMedia"
                        (list :features (vector (list :name "prefers-color-scheme"
                                                      :value (if canvas-browser-dark
                                                                 "dark" "light")))))
  (canvas-browser--tell "Emulation.setAutoDarkModeOverride"
                        (list :enabled (if canvas-browser-dark t :json-false))))

(defun canvas-browser-search-text ()
  "Put the text of the page in a buffer and search it with `consult-line\='.
Without consult, isearch takes over in that buffer."
  (interactive)
  (canvas-browser-text)
  (with-current-buffer (canvas-browser--text-buffer)
    (if (fboundp 'consult-line)
        (call-interactively 'consult-line)
      (call-interactively 'isearch-forward))))

(defconst canvas-browser--text-js
  "(root => {
     const up = n => n.assignedSlot || n.parentNode || n.host;
     const deep = new Set();
     const mark = n => { for (; n && !deep.has(n); n = up(n)) deep.add(n); };
     const scan = within => within.querySelectorAll('*').forEach(e => {
       if (!e.shadowRoot) return;
       mark(e);
       e.shadowRoot.querySelectorAll('slot').forEach(mark);
       scan(e.shadowRoot);
     });
     scan(document);
     if (!deep.has(root)) return root.innerText;
     const display = e => getComputedStyle(e).display;
     const shown = e => display(e) === 'contents'
       || e.checkVisibility({checkVisibilityCSS: true, visibilityProperty: true});
     const kids = e => e.shadowRoot ? e.shadowRoot.childNodes
       : e.localName !== 'slot' ? e.childNodes
       : e.assignedNodes({flatten: true}).length ? e.assignedNodes({flatten: true})
       : e.childNodes;
     const lines = [];
     let run = '';
     const flush = () => {
       const text = run.replace(/\\s+/g, ' ').trim();
       if (text) lines.push(text);
       run = '';
     };
     const visit = node => {
       for (const child of kids(node)) {
         if (child.nodeType === Node.TEXT_NODE) { run += child.data; continue; }
         if (child.nodeType !== Node.ELEMENT_NODE || !shown(child)) continue;
         if (child.localName === 'br') { flush(); continue; }
         const look = display(child);
         const inside = deep.has(child) || look === 'contents'
           || typeof child.innerText !== 'string';
         if (look.startsWith('inline') || look === 'contents') {
           if (inside) visit(child); else run += child.innerText;
           continue;
         }
         flush();
         const apart = child.localName === 'p';
         if (apart) lines.push('');
         if (inside) { visit(child); flush(); } else lines.push(...child.innerText.split('\\n'));
         if (apart) lines.push('');
       }
     };
     visit(root);
     flush();
     return lines.join('\\n').replace(/\\n{3,}/g, '\\n\\n').trim();
   })"
  "The JavaScript function that gives the text of an element of a page.
It is all the text that the page shows there, in the order it shows it,
and nothing is read into it.

Chromium gives the text of an element from the children that the page
gave the element.  A web component draws content of its own, which is
not among them, so chromium leaves it out: of a list that is a component
it gives the heading that the page gave the list, and not its rows.

An element with no component inside it gives its text as chromium makes
it, to the letter.  An element with a component inside it is read child
by child, through what each component draws and through the places it
shows its own children in.  A block there is a line, a paragraph has an
empty line around it, and what the page does not show is left out.")

(defconst canvas-browser--text-script
  (concat canvas-browser--text-js "(document.body)")
  "The JavaScript that gives the text of a page.")

(defconst canvas-browser--html-js
  "(root => {
     if (!root.getHTML) return root.outerHTML;
     const roots = [];
     const scan = within => within.querySelectorAll('*').forEach(take);
     const take = e => { if (e.shadowRoot) { roots.push(e.shadowRoot); scan(e.shadowRoot); } };
     take(root);
     scan(root);
     const shell = root.cloneNode(false).outerHTML;
     const close = shell.lastIndexOf('</');
     return shell.slice(0, close) + root.getHTML({shadowRoots: roots}) + shell.slice(close);
   })"
  "The JavaScript function that gives the HTML of an element of a page.
It is the HTML as the page has it.  What a web component draws of its
own is in it too, the way HTML writes that: in a template inside the
component that says it is the inside of one.")

(defun canvas-browser--value-of (result)
  "The value in RESULT, the answer of chromium to a script.
The null of JSON is nil.  An error names what went wrong when the
script failed in the page."
  (when-let* ((failure (plist-get result :exceptionDetails)))
    (error "canvas-browser: a script failed in the page: %s"
           (or (plist-get (plist-get failure :exception) :description)
               (plist-get failure :text))))
  (let ((value (plist-get (plist-get result :result) :value)))
    (and (not (eq value :null)) value)))

(defun canvas-browser--text-buffer ()
  "The buffer that holds the text of this page."
  (get-buffer-create (format "*canvas-browser-text: %s*"
                             (or canvas-browser--title canvas-browser--url))))

(defun canvas-browser-text ()
  "Put the text of the page in a buffer, where the keys of Emacs work.
When a part of the page is picked, with `S\=', it is the text of that
part.  It is the text that `>\=' sends: translated, when the address
of the page has a translator."
  (interactive)
  (let ((text (if canvas-browser--part
                  (canvas-browser--text-to-read 'part #'canvas-browser--part-text)
                (canvas-browser--text-to-read 'page #'canvas-browser--page-text)))
        (target (canvas-browser--text-buffer)))
    (canvas-browser--fill-text-buffer
     target text (current-buffer) (plist-get canvas-browser--part :place))
    (display-buffer target)))

;;;; From a line of the text to its place in the page

(defvar-local canvas-browser--text-page nil
  "The page buffer that the text of this buffer came from.")

(defvar-local canvas-browser--text-place nil
  "The place of the part that the text of this buffer is of, or nil.
It is nil for the text of a whole page.")

(defvar-keymap canvas-browser-text-mode-map
  :doc "Keymap of the buffer that holds the text of a page."
  "M-." #'canvas-browser-text-show-in-page)

(define-minor-mode canvas-browser-text-mode
  "The buffer holds the text of a page of canvas-browser.
`M-.\=' shows where the line at point stands in the page, and the key
that goes back in xref comes back to the line."
  :lighter nil)

(defun canvas-browser--fill-text-buffer (buffer text page place)
  "Put TEXT in BUFFER as the text of PAGE, or of its part at PLACE."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert text)
      (goto-char (point-min)))
    (canvas-browser-text-mode 1)
    (setq canvas-browser--text-page page
          canvas-browser--text-place place)))

(defconst canvas-browser--locate-js (concat "
(function (root, wanted, blink) {" canvas-browser--paint-js canvas-browser--flat-js "
  const view = flatten(root && root.isConnected ? root : document.body);
  const low = text => [...text].map(c => c.toLowerCase().length === c.length ? c.toLowerCase() : c).join('');
  const tight = text => low(text.replace(/\\s+/g, ''));
  const places = [];
  for (let at = 0; at < view.flat.length; at++) if (view.flat[at] !== ' ') places.push(at);
  const hay = tight(view.flat);
  for (let found = 0; found < wanted.length; found++) {
    const needle = tight(wanted[found].text);
    if (!needle) throw new Error('canvas-browser: asked for a line without text');
    const hits = [];
    for (let at = hay.indexOf(needle); at !== -1; at = hay.indexOf(needle, at + 1)) hits.push(at);
    if (!hits.length) continue;
    const rank = Math.min(wanted[found].rank, hits.length - 1);
    const length = wanted[found].paint ? tight(wanted[found].text.slice(0, wanted[found].paint)).length
                                       : needle.length;
    const range = view.range(places[hits[rank]], places[hits[rank] + length - 1] + 1);
    reveal(range);
    blinkOn(range, blink);
    return {found, count: hits.length, rank, box: boxOf(range)};
  }
  paint([], null);
  return {found: -1};
})")
  "The JavaScript function that shows where a line of text stands in a page.
It takes an element and a list of lines, each with its text and its
rank: how many times the text stands before the line.  It looks in the
text that the element shows, through web components too, and takes the
first line that stands there.  A line can say how much of its text to
paint, from its start, when it was asked for with more text than
itself.  The text is compared without regard to case and to white
space: a page can show its text in capitals, it breaks its lines as it
likes, and it can show two parts side by side with no space between
them that its text has on two lines.  A line can lie in more than one
piece of the page, as a sentence with a link in it does.  The
hit of the rank is scrolled into view, also in a part that scrolls on
its own, and painted for as many milliseconds as its third argument
says.  It gives which line it took, how many hits that
line has and which one it shows, or -1 for the line when the page holds
none of them.")

(defconst canvas-browser--locate-blink 1500
  "For how many milliseconds a line shown from the text of a page is painted.
The reader came to see where the line is, not to keep it marked.")

(defconst canvas-browser--locate-tries 5
  "How many lines `M-.\=' in the text of a page tries, from point on.
A line that a translator made, as the one that names who wrote a
message, is not in the page.  The text of the message under it is.")

(defconst canvas-browser--white-space
  "[ \t\n\r\f\v\u00a0\u1680\u2000-\u200a\u2028\u2029\u202f\u205f\u3000\ufeff]+"
  "A regexp for a run of white space, as a page takes it.
It names what \\s names in JavaScript, so that Emacs and the page take
the same letters out of a text.")

(defun canvas-browser--squeeze (text)
  "TEXT with one space for each run of white space, and none at its ends."
  (string-trim (replace-regexp-in-string canvas-browser--white-space " " text)))

(defun canvas-browser--tight (text)
  "TEXT without its white space, as a page compares it."
  (replace-regexp-in-string canvas-browser--white-space "" text))

(defun canvas-browser--count-in (needle hay)
  "How many times NEEDLE stands in HAY, without regard to case."
  (let ((needle (downcase needle))
        (hay (downcase hay))
        (count 0)
        (start 0))
    (while (setq start (string-search needle hay start))
      (setq count (1+ count)
            start (1+ start)))
    count))

(defun canvas-browser--without-pictures (text)
  "TEXT without what stands in it for a picture of the page.
The text of a page names a picture in brackets, as [image], and has an
emoji that the page draws as a picture as the emoji itself.  Each has
the text property `canvas-browser-for-picture\='.  Brackets and emoji
that are words of the page itself have not, and stay."
  (let ((kept nil)
        (start 0)
        (end (length text)))
    (while (< start end)
      (let ((next (next-single-property-change start 'canvas-browser-for-picture text end)))
        (unless (get-text-property start 'canvas-browser-for-picture text)
          (push (substring-no-properties text start next) kept))
        (setq start next)))
    (apply #'concat (nreverse kept))))

(defun canvas-browser--to-find (text before)
  "TEXT as a page is asked for it, or nil for no text.
It is a plist of the text, with one space for white space, and of its
rank: how many times it stands in BEFORE, the text before it.  The rank
is counted as the page compares, without regard to white space."
  (let ((text (canvas-browser--squeeze text)))
    (unless (string-empty-p text)
      (list :text text
            :rank (canvas-browser--count-in (canvas-browser--tight text)
                                            (canvas-browser--tight before))))))

(defun canvas-browser--line-to-find ()
  "The ways to ask a page for the line at point, or nil for an empty line.
Each is a plist of `canvas-browser--to-find\='.  The first is the line
as it is written.  A line that names pictures has a second way, without
them: the page shows a picture there and has no text for it."
  (let* ((line (buffer-substring (line-beginning-position) (line-end-position)))
         (before (buffer-substring (point-min) (line-beginning-position)))
         (bare (canvas-browser--without-pictures line)))
    (delq nil (list (canvas-browser--to-find (substring-no-properties line)
                                             (substring-no-properties before))
                    (and (not (equal bare (substring-no-properties line)))
                         (canvas-browser--to-find
                          bare (canvas-browser--without-pictures before)))))))

(defvar canvas-browser--locate-stretch 120
  "How many letters of text a page is asked for at most, from a line on.")

(defun canvas-browser--cut-at-word (text length)
  "TEXT, or its start of at most LENGTH letters that ends at a word."
  (if (<= (length text) length)
      text
    (substring text 0 (or (string-match-p " [^ ]*\\'" (substring text 0 (1+ length)))
                          length))))

(defun canvas-browser--stretch-way (text line before)
  "The way to ask a page for TEXT, which starts with LINE, or nil.
BEFORE is the text before it, for its rank.  TEXT is cut to
`canvas-browser--locate-stretch\=' letters.  The way says with :paint
how much of it is LINE.  It is nil for an empty LINE, and when the cut
TEXT holds no more than LINE."
  (let ((line (canvas-browser--squeeze line))
        (stretch (canvas-browser--cut-at-word (canvas-browser--squeeze text)
                                              canvas-browser--locate-stretch)))
    (when (and (not (string-empty-p line)) (> (length stretch) (length line)))
      (append (canvas-browser--to-find stretch before) (list :paint (length line))))))

(defun canvas-browser--stretch-ways (text line before)
  "The ways to ask a page for TEXT, which starts with LINE.
BEFORE is a pair of the text before it: as it is written, and without
what stands for a picture.  As for a line, a second way leaves out what
stands for a picture."
  (let ((bare (canvas-browser--without-pictures text)))
    (delq nil (list (canvas-browser--stretch-way (substring-no-properties text)
                                                 (substring-no-properties line)
                                                 (car before))
                    (and (not (equal bare (substring-no-properties text)))
                         (canvas-browser--stretch-way
                          bare (canvas-browser--without-pictures line) (cdr before)))))))

(defun canvas-browser--lines-with-text ()
  "The line at point and the lines after it, those that hold text.
No more than `canvas-browser--locate-tries\=' lines are read."
  (save-excursion
    (let ((end (save-excursion (forward-line canvas-browser--locate-tries) (point))))
      (seq-remove #'string-blank-p
                  (split-string (buffer-substring (line-beginning-position) end) "\n")))))

(defun canvas-browser--stretches (lines)
  "LINES as one line: all of them, then one line fewer, down to two lines."
  (let ((stretches nil))
    (while (cdr lines)
      (push (string-join lines " ") stretches)
      (setq lines (butlast lines)))
    (nreverse stretches)))

(defun canvas-browser--stretch-to-find ()
  "The ways to ask a page for the line at point with the lines after it.
The text of a page breaks a line where the HTML has a block, and the
page can show that block inside a line, as it shows a mention.  So the
line alone can be a name that the page holds in many places, while the
line and what comes after it, as one line, stand in the page once.

The longest stretch comes first, then stretches of fewer lines: a page
can have text between two messages that its text here does not have,
and then it holds only a stretch that ends before the next message."
  (let* ((start (line-beginning-position))
         (line (buffer-substring start (line-end-position)))
         (before (buffer-substring (point-min) start))
         (before (cons (substring-no-properties before)
                       (canvas-browser--without-pictures before))))
    (unless (string-blank-p line)
      (seq-uniq (mapcan (lambda (text) (canvas-browser--stretch-ways text line before))
                        (canvas-browser--stretches (canvas-browser--lines-with-text)))
                (lambda (one other)
                  (equal (plist-get one :text) (plist-get other :text)))))))

(defun canvas-browser--lines-to-find ()
  "The line at point and those after it, as a page is asked for them.
The first ways are those of `canvas-browser--stretch-to-find\='.  Then
come the lines one by one: empty lines are left out, and no more than
`canvas-browser--locate-tries\=' lines are given.  Each way to ask is
a plist, which also says with :line which line it is of: 0 for the line
at point, or the first after it that holds text."
  (save-excursion
    (let ((ways (nreverse (mapcar (lambda (way) (append way (list :line 0)))
                                  (canvas-browser--stretch-to-find))))
          (count 0)
          (more t))
      (while (and more (< count canvas-browser--locate-tries))
        (when-let* ((line (canvas-browser--line-to-find)))
          (dolist (way line)
            (push (append way (list :line count)) ways))
          (setq count (1+ count)))
        (setq more (and (zerop (forward-line 1)) (not (eobp)))))
      (nreverse ways))))

(defun canvas-browser--text-root-js (place)
  "The JavaScript that names what a text was taken from, in this page.
It is the part at PLACE, while the page still has that part picked, and
else the whole page."
  (if (and place (equal place (plist-get canvas-browser--part :place)))
      (canvas-browser--part-js)
    "document.body"))

(defun canvas-browser--located (answer lines)
  "Say what ANSWER, of the page, found of LINES.
LINES are the ways to ask that `canvas-browser--lines-to-find\=' gave."
  (let* ((found (or (plist-get answer :found) -1))
         (way (and (>= found 0) (nth found lines))))
    (canvas-browser--land-anew (plist-get answer :box))
    (cond ((null way)
           (message "canvas-browser: the page does not hold this line as it is written"))
          ((> (plist-get way :line) 0)
           (message "canvas-browser: this line is not in the page; shown is: %s"
                    (canvas-browser--first-line (plist-get way :text))))
          (t (message "canvas-browser: hit %d of %d"
                      (1+ (plist-get answer :rank)) (plist-get answer :count))))))

(defun canvas-browser--way-to-send (way)
  "WAY to ask for a line as the page is sent it: what it needs of the plist."
  (append (list :text (plist-get way :text) :rank (plist-get way :rank))
          (and (plist-get way :paint) (list :paint (plist-get way :paint)))))

(defun canvas-browser--locate (lines place)
  "Show in this page the first of LINES that it holds, of the text at PLACE."
  (canvas-browser--tell
   "Runtime.evaluate"
   (list :expression (format "%s(%s, %s, %d)" canvas-browser--locate-js
                             (canvas-browser--text-root-js place)
                             (json-encode
                              (vconcat (mapcar #'canvas-browser--way-to-send lines)))
                             canvas-browser--locate-blink)
         :returnByValue t)
   (lambda (result)
     (canvas-browser--located (canvas-browser--value-of result) lines))))

(defun canvas-browser-text-show-in-page ()
  "Show where the line at point stands in the page that this text is of.
The page buffer is shown, the page scrolls to the line and paints it
for a moment.
A line that stands in the page more than once is the one with the same
rank as here.  When the page does not hold the line, as one that a
translator made, the first line after it that the page holds is shown.
The key that goes back in xref comes back here."
  (interactive nil canvas-browser-text-mode)
  (let ((page canvas-browser--text-page)
        (place canvas-browser--text-place)
        (lines (canvas-browser--lines-to-find)))
    (unless (buffer-live-p page)
      (user-error "canvas-browser: the page of this text is gone"))
    (unless lines
      (user-error "canvas-browser: no text here to look for"))
    (xref-push-marker-stack)
    (pop-to-buffer page)
    (canvas-browser--push-mark t)
    (canvas-browser--locate lines place)))

;;;; The index of a page, for imenu

(defconst canvas-browser--headings-script
  (concat "(function () {" canvas-browser--flat-js "
     const found = [];
     eachShown(document.body, e => {
       if (e.nodeType !== 1 || !e.matches('h1,h2,h3,h4,h5,h6,[role=heading]')) return;
       const text = (e.innerText || '').replace(/\\s+/g, ' ').trim();
       if (!text) return;
       const level = /^H[1-6]$/.test(e.tagName) ? Number(e.tagName[1])
                   : Number(e.getAttribute('aria-level')) || 2;
       found.push([e, level, text.slice(0, 80)]);
     });
     window.__canvasBrowserHeadings = found.map(heading => heading[0]);
     return found.map(heading => [heading[1], heading[2]]);
   })()")
  "The JavaScript that gives the headings of a page, each as (LEVEL TEXT).
A heading is what the page marks as one, inside web components too, and
that it shows: a heading that is there for a screen reader only is
none.  They come in the order of the page, so the headings of one
column come before those of the next.  They are kept in the page as
well, so that one of them is found later by the place it has there.")

(defun canvas-browser--kept-js (kept)
  "The JavaScript function that shows an element that a page kept in KEPT.
KEPT names what the page keeps its elements in, in its window.  The
function takes the place of the element there, and for how many
milliseconds to paint it.  It gives the box of the element, or null
for an element that the page has thrown away."
  (concat "
(function (index, blink) {" canvas-browser--paint-js canvas-browser--flat-js "
  const element = (window." kept " || [])[index];
  if (!element || !element.isConnected) return null;
  const range = document.createRange();
  range.selectNodeContents(element);
  reveal(range);
  blinkOn(range, blink);
  return boxOf(range);
})"))

(defconst canvas-browser--heading-js (canvas-browser--kept-js "__canvasBrowserHeadings")
  "The JavaScript function that shows a heading of a page.
See `canvas-browser--kept-js\='.")

(defun canvas-browser--show-kept (script index gone)
  "Show the element at INDEX of those that this page kept, with SCRIPT.
SCRIPT is a function of `canvas-browser--kept-js\='.  GONE is what the
reader is told when the page has thrown the element away."
  (canvas-browser--tell
   "Runtime.evaluate"
   (list :expression (format "%s(%d, %d)" script index canvas-browser--locate-blink)
         :returnByValue t)
   (canvas-browser--here
    (lambda (result)
      (if-let* ((box (canvas-browser--value-of result)))
          (canvas-browser--land-anew box)
        (message "%s" gone))))))

(defun canvas-browser--show-heading (_name _position index)
  "Show the heading at INDEX of this page, as imenu calls an entry of its index."
  (canvas-browser--show-kept canvas-browser--heading-js index
                             "canvas-browser: that heading has gone, the page has changed"))

(defun canvas-browser--show-message (_name _position lines)
  "Show the message whose text starts with LINES, as imenu calls an entry."
  (canvas-browser--locate lines (plist-get canvas-browser--part :place)))

(defun canvas-browser--heading-entries ()
  "The headings of this page, each (LEVEL NAME FUNCTION ARGUMENT)."
  (cl-loop for (level text) in (canvas-browser--value-now canvas-browser--headings-script)
           for index from 0
           collect (list level text #'canvas-browser--show-heading index)))

(defconst canvas-browser--name-length 40
  "How many letters of the text of a message stand in its name in an index.")

(defun canvas-browser--text-start (text)
  "The first line of TEXT, cut to `canvas-browser--name-length\=' letters."
  (let ((line (canvas-browser--squeeze (car (split-string text "\n")))))
    (if (> (length line) canvas-browser--name-length)
        (concat (substring line 0 canvas-browser--name-length) "…")
      line)))

(defun canvas-browser--item-name (item)
  "What an index calls ITEM of a thread.
It is its title.  Without one it is who wrote it and how its text
starts, which tells two messages of one author apart.  Without a text
it is who wrote it and when."
  (let ((author (plist-get item :author))
        (time (plist-get item :time))
        (text (plist-get item :text)))
    (cond ((plist-get item :title))
          (text (concat (and author (concat author ": "))
                        (canvas-browser--text-start text)))
          (t (string-join (delq nil (list author (and time (canvas-browser--time-text time))))
                          " · ")))))

(defun canvas-browser--paint-whole (way)
  "WAY to ask for a line, with all of its text to paint.
A message that was picked is what the eye must find, not its first line."
  (list :text (plist-get way :text)
        :rank (plist-get way :rank)
        :line (plist-get way :line)))

(defun canvas-browser--message-entries (thread level)
  "The items of THREAD and their replies, each (LEVEL NAME FUNCTION LINES).
The text of each item is put in this buffer, after the texts before it.
LINES are the first lines of the text as a page is asked for them, each
with its rank: how many times it stands in the texts before it."
  (mapcan (lambda (item)
            (let ((start (point)))
              (insert (or (plist-get item :text) "") "\n\n")
              (cons (list level (canvas-browser--item-name item)
                          #'canvas-browser--show-message
                          (save-excursion
                            (goto-char start)
                            (mapcar #'canvas-browser--paint-whole
                                    (canvas-browser--lines-to-find))))
                    (canvas-browser--message-entries (plist-get item :replies)
                                                     (1+ level)))))
          thread))

(defun canvas-browser--unique-names (entries)
  "ENTRIES, each (LEVEL NAME . REST), with a number after a name that came before.
An index finds an entry by its name, so two with one name are one."
  (let ((seen (make-hash-table :test 'equal)))
    (mapcar (lambda (entry)
              (pcase-let* ((`(,level ,name . ,rest) entry)
                           (count (cl-incf (gethash name seen 0))))
                (if (= count 1)
                    entry
                  (cons level (cons (format "%s <%d>" name count) rest)))))
            entries)))

(defun canvas-browser--index-entry (entry)
  "ENTRY, (LEVEL NAME FUNCTION ARGUMENT), as an entry of imenu that calls FUNCTION."
  (pcase-let ((`(,_ ,name ,function ,argument) entry))
    (list name (point-min) function argument)))

(defun canvas-browser--outline (entries)
  "ENTRIES as an index of imenu.
Each is (LEVEL NAME FUNCTION ARGUMENT), and they stand in their order.
An entry with entries of a deeper level after it is a menu of its own,
which starts with the entry itself, as the index of org has it."
  (let ((index nil))
    (while entries
      (let* ((entry (pop entries))
             (under nil))
        (while (and entries (> (car (car entries)) (car entry)))
          (push (pop entries) under))
        (push (if under
                  (cons (nth 1 entry)
                        (cons (canvas-browser--index-entry entry)
                              (canvas-browser--outline (nreverse under))))
                (canvas-browser--index-entry entry))
              index)))
    (nreverse index)))

(defun canvas-browser--index-of (entries)
  "ENTRIES, each (LEVEL NAME FUNCTION ARGUMENT), as an index of imenu."
  (canvas-browser--outline (canvas-browser--unique-names entries)))

(defun canvas-browser--thread-index ()
  "The messages of the thread of this page as a menu of an index, or nil.
A page has a thread when its address has a translator that gives one."
  (when-let* ((thread (plist-get (canvas-browser--translation
                                  (if canvas-browser--part 'part 'page))
                                 :thread)))
    (list (cons "Messages"
                (canvas-browser--index-of
                 (with-temp-buffer (canvas-browser--message-entries thread 1)))))))

(defun canvas-browser-imenu-index ()
  "The index of this page, for imenu.
The headings of the page are its outline: a heading with headings under
it is a menu of its own.  The messages of a thread are a menu too, each
named by who wrote it and when, a reply under the message it answers.
A page has them when its address has a translator that gives a thread.
An entry that is picked is shown in the page, and painted for a moment."
  (append (canvas-browser--index-of (canvas-browser--heading-entries))
          (canvas-browser--thread-index)))

;;;;; Choosing from the index

(declare-function consult--read "consult" (table &rest options))
(declare-function consult--lookup-cons "consult" (selected candidates &rest _))

(defvar canvas-browser-imenu-history nil
  "The entries of the indexes of pages that were chosen.")

(defun canvas-browser--menu-p (entry)
  "Whether ENTRY of an index of imenu is a menu, and not something to pick."
  (and (consp (cdr entry)) (consp (cadr entry))))

(defun canvas-browser--flat-index (index &optional path)
  "INDEX, an index of imenu, as a list of (NAME . ENTRY) to choose from.
An entry of a menu is named by the path to it, MENU/ENTRY, under PATH
when given.  The entry that a menu starts with, which has the name of
the menu, is named as the menu: it is the heading that the menu is of."
  (mapcan (lambda (entry)
            (let ((name (if path (concat path "/" (car entry)) (car entry))))
              (cond ((not (canvas-browser--menu-p entry))
                     (list (cons name entry)))
                    ((and (not (canvas-browser--menu-p (cadr entry)))
                          (equal (car (cadr entry)) (car entry)))
                     (cons (cons name (cadr entry))
                           (canvas-browser--flat-index (cddr entry) name)))
                    (t (canvas-browser--flat-index (cdr entry) name)))))
          index))

(defun canvas-browser--pick-entry (entry)
  "Show ENTRY of the index of this page, as imenu does for a picked entry."
  (pcase-let ((`(,name ,position ,function . ,arguments) entry))
    (apply function name position arguments)))

(defun canvas-browser--preview-state (show)
  "The function that shows to consult what is offered of this page.
SHOW shows one thing in the page: an entry of its index, a link.
consult calls the function with what happens and a candidate, (NAME .
THING) or nil: before the reader chooses, for each candidate that the
reader moves to, and at the end with the one that was picked.  The page
notes where it stands at the start and shows each thing.  At the end it
stays at what was picked, with a mark where it stood, or goes back when
nothing was.

The candidate that consult shows first, before the reader moved, is not
shown in the page: the page must not jump to its first heading because
its index was opened."
  (let ((page (current-buffer))
        (opened nil))
    (lambda (action candidate)
      (with-current-buffer page
        (pcase action
          ('setup (canvas-browser--push-mark t)
                  (canvas-browser--keep-origins))
          ('preview (when candidate
                      (if opened
                          (funcall show (cdr candidate))
                        (setq opened t))))
          ('return (canvas-browser--end-origins (null candidate))
                   (if candidate
                       (funcall show (cdr candidate))
                     (canvas-browser--drop-mark))))))))

(defun canvas-browser-imenu ()
  "Go to a heading or a message of this page, chosen from its index.
With consult, the page shows each entry while you move to it, and goes
back to where it stood when you give up.  Without consult, this is
`imenu\\='.  The keys of `imenu\\=' and of `consult-imenu\\=' run it in a
page buffer."
  (interactive nil canvas-browser-mode)
  (if (or (fboundp 'consult--read) (require 'consult nil t))
      (let ((entries (canvas-browser--flat-index (canvas-browser-imenu-index))))
        (unless entries
          (user-error "canvas-browser: this page has no heading and no thread"))
        (consult--read entries
                       :prompt "Go to: "
                       :require-match t
                       :sort nil
                       :lookup #'consult--lookup-cons
                       :state (canvas-browser--preview-state #'canvas-browser--pick-entry)
                       :history 'canvas-browser-imenu-history))
    (call-interactively #'imenu)))

;;;; The links of a page, to choose from

(defconst canvas-browser--links-script
  (concat "(function () {" canvas-browser--flat-js "
     const found = [];
     eachShown(document.body, e => {
       if (e.nodeType !== 1 || e.tagName !== 'A' || !e.href) return;
       if (/^javascript:/i.test(e.href)) return;
       const picture = e.querySelector('img[alt]');
       const text = (e.innerText || '').replace(/\\s+/g, ' ').trim()
                 || e.getAttribute('aria-label') || e.title || (picture && picture.alt) || '';
       found.push([e, text.slice(0, 120), e.href]);
     });
     window.__canvasBrowserLinks = found.map(link => link[0]);
     return found.map(link => [link[1], link[2]]);
   })()")
  "The JavaScript that gives the links of a page, each as (TEXT ADDRESS).
A link is one that the page shows, inside web components too, and that
has an address to go to.  Its text is what it shows, or else what it is
called for a screen reader, or the name of its picture.  The links come
in the order of the page, and they are kept in the page as well, so
that one of them is found later by the place it has there.")

(defconst canvas-browser--link-js (canvas-browser--kept-js "__canvasBrowserLinks")
  "The JavaScript function that shows a link of a page.
See `canvas-browser--kept-js\='.")

(defconst canvas-browser--follow-link-script "(function (index) {
  const link = (window.__canvasBrowserLinks || [])[index];
  if (!link || !link.isConnected) return null;
  link.click();
  return true;
})(%d)"
  "The JavaScript that clicks a link that the page kept; %d is its place.
The answer is null for a link that the page has thrown away.")

(defvar canvas-browser-link-history nil
  "The links of pages that were chosen.")

(defun canvas-browser--link-name (text url)
  "What the link with TEXT to URL is offered as: its text and its address.
The reader can then choose by either.  A link without a text is
offered as its address."
  (if (string-empty-p text)
      url
    (concat text "  " (propertize url 'face 'shadow))))

(defun canvas-browser--link-candidates ()
  "The links that this page shows, each (NAME INDEX URL), to choose from.
INDEX is the place of the link among those that the page kept.  A link
that stands in the page twice is offered once."
  (let ((seen (make-hash-table :test 'equal)))
    (cl-loop for (text url) in (canvas-browser--value-now canvas-browser--links-script)
             for index from 0
             for name = (canvas-browser--link-name text url)
             unless (gethash name seen)
             collect (progn (puthash name t seen)
                            (list name index url)))))

(defun canvas-browser--show-link (link)
  "Show LINK, (INDEX URL), in this page."
  (canvas-browser--show-kept canvas-browser--link-js (car link)
                             "canvas-browser: that link has gone, the page has changed"))

(defun canvas-browser--follow-link (link)
  "Click LINK, (INDEX URL), in this page.
The page takes the click as a gesture of the reader, so a link that
opens a window is not held back as a popup.  When the page has thrown
the link away, the page goes to its address."
  (pcase-let ((`(,index ,url) link))
    (canvas-browser--tell
     "Runtime.evaluate"
     (list :expression (format canvas-browser--follow-link-script index)
           :returnByValue t :userGesture t)
     (canvas-browser--here
      (lambda (result)
        (unless (canvas-browser--value-of result)
          (canvas-browser-open-url url)))))))

(defun canvas-browser--link-table (candidates)
  "CANDIDATES as a completion table that keeps their order.
It says that they are links, so that embark knows what to do with one."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        '(metadata (category . canvas-browser-link)
                   (display-sort-function . identity)
                   (cycle-sort-function . identity))
      (complete-with-action action candidates string predicate))))

(defun canvas-browser--read-link (candidates)
  "Ask the reader for one of CANDIDATES, the links of this page.
Give (INDEX URL) of the link that was chosen.  With consult, the page
shows each link while the reader moves to it, and goes back to where it
stood when the reader gives up."
  (cdr (if (or (fboundp 'consult--read) (require 'consult nil t))
           (consult--read candidates
                          :prompt "Link: "
                          :require-match t
                          :sort nil
                          :category 'canvas-browser-link
                          :lookup #'consult--lookup-cons
                          :state (canvas-browser--preview-state #'canvas-browser--show-link)
                          :history 'canvas-browser-link-history)
         (assoc (completing-read "Link: " (canvas-browser--link-table candidates)
                                 nil t nil 'canvas-browser-link-history)
                candidates))))

(defun canvas-browser-links (&optional other)
  "Follow a link of this page, chosen by its text or by its address.
With consult, the page shows each link while you move to it, and goes
back to where it stood when you give up.  The link that you choose is
clicked, as a hint clicks it.  With a prefix argument, OTHER, its
address opens in a page buffer of its own.  Embark acts on a link that
is offered as on any address."
  (interactive "P" canvas-browser-mode)
  (let ((candidates (canvas-browser--link-candidates)))
    (unless candidates
      (user-error "canvas-browser: this page shows no link"))
    (when-let* ((link (canvas-browser--read-link candidates)))
      (if other
          (canvas-browser (cadr link))
        (canvas-browser--follow-link link)))))

;;;; The positions of a page: marks and registers

(cl-defstruct (canvas-browser-position (:constructor canvas-browser--make-position)
                                       (:copier nil))
  "Where a page stood: its address, and how far it was scrolled.
ID names what the page itself keeps of the position: how far each of
its parts was scrolled.  The page keeps that until it loads anew, so X
and Y say how far the page as a whole was scrolled."
  url title x y id)

(defconst canvas-browser--position-js
  (concat canvas-browser--reach-js "
  const positions = () => window.__canvasBrowserPositions || (window.__canvasBrowserPositions =
    {key: Math.random().toString(36).slice(2), kept: new Map(), last: 0});
  const scrolled = () => {
    const found = [];
    walk(document, '*', e => { if (e.scrollTop || e.scrollLeft) found.push(e); });
    return found;
  };
  const takePosition = () => {
    const all = positions();
    const id = all.key + '-' + (++all.last);
    all.kept.set(id, scrolled().map(e => [new WeakRef(e), e.scrollLeft, e.scrollTop]));
    return {id, x: Math.round(scrollX), y: Math.round(scrollY)};
  };
  const goToPosition = (id, x, y) => {
    const kept = positions().kept.get(id);
    if (!kept) { scrollTo(x, y); return false; }
    const there = new Map(kept.map(([part, left, top]) => [part.deref(), [left, top]]));
    for (const e of scrolled()) if (!there.has(e)) e.scrollTo(0, 0);
    for (const [e, [left, top]] of there) if (e && e.isConnected) e.scrollTo(left, top);
    return true;
  };")
  "The JavaScript that notes where a page stands, and puts it back there.
A page is often more than one thing that scrolls.  So `takePosition\\='
keeps how far each part of the page is scrolled, under a name that no
other position has, in this page or in one that was at its address
before.  `goToPosition\\=' scrolls each part back, and a part that was
not scrolled then to its start.  A page that has loaded anew keeps
nothing, and then scrolls as a whole to X and Y.")

(defconst canvas-browser--position-script
  (concat "(function () {" canvas-browser--position-js "
  return takePosition();
})()")
  "The JavaScript that gives where the page stands: a name, and X and Y.")

(defconst canvas-browser--go-to-position-script
  (concat "(function (id, x, y) {" canvas-browser--position-js "
  return goToPosition(id, x, y);
})(%s, %d, %d)")
  "The JavaScript that puts the page where a position says.
%s is the name of the position, as JSON, and the two %d are its X and Y.")

(defvar-local canvas-browser--mark-ring nil
  "The positions that were marked in this page buffer, the newest first.")

(defvar-local canvas-browser--position-wanted nil
  "The position that this page goes to once it has loaded, or nil.")

(defun canvas-browser--position (then)
  "Call THEN in this buffer with the position where the page stands now."
  (let ((url canvas-browser--url)
        (title canvas-browser--title))
    (canvas-browser--ask-page
     canvas-browser--position-script
     (lambda (answer)
       (funcall then (canvas-browser--make-position
                      :url url :title title
                      :x (plist-get answer :x) :y (plist-get answer :y)
                      :id (plist-get answer :id)))))))

(defun canvas-browser--scroll-to-position (position)
  "Scroll this page to where POSITION says."
  (canvas-browser--tell
   "Runtime.evaluate"
   (list :expression (format canvas-browser--go-to-position-script
                             (json-encode (canvas-browser-position-id position))
                             (canvas-browser-position-x position)
                             (canvas-browser-position-y position)))))

(defun canvas-browser--go-to-position (position)
  "Have this page stand where POSITION says.
A position of another address is gone to first, and the page scrolls
once it has loaded."
  (if (equal (canvas-browser-position-url position) canvas-browser--url)
      (canvas-browser--scroll-to-position position)
    (setq canvas-browser--position-wanted position)
    (canvas-browser-open-url (canvas-browser-position-url position))))

(defun canvas-browser--go-to-wanted-position ()
  "Scroll this page, which has loaded, to the position that it was opened for."
  (when-let* ((position canvas-browser--position-wanted))
    (setq canvas-browser--position-wanted nil)
    (canvas-browser--scroll-to-position position)))

(defun canvas-browser--push-mark (&optional quiet)
  "Keep where this page stands as the newest mark.
The ring is as long as that of a buffer.  The reader is told, unless
QUIET."
  (canvas-browser--position
   (lambda (position)
     (setq canvas-browser--mark-ring
           (seq-take (cons position canvas-browser--mark-ring) mark-ring-max))
     (unless quiet
       (message "canvas-browser: the mark is set where the page stands")))))

(defun canvas-browser--drop-mark ()
  "Forget the newest mark of this page buffer.
It was set before a jump that did not happen after all."
  (pop canvas-browser--mark-ring))

(defun canvas-browser--newest-mark ()
  "The newest mark of this page buffer; a user error when it has none."
  (or (car canvas-browser--mark-ring)
      (user-error "canvas-browser: no mark is set in this page buffer")))

(defun canvas-browser-pop-mark ()
  "Go to the newest mark of this page, and turn the ring of its marks.
The next time goes to the mark before it, as `C-u C-SPC\=' does in a
buffer."
  (interactive nil canvas-browser-mode)
  (let ((mark (canvas-browser--newest-mark)))
    (setq canvas-browser--mark-ring (append (cdr canvas-browser--mark-ring) (list mark)))
    (canvas-browser--go-to-position mark)))

(defun canvas-browser-set-mark (&optional back)
  "Mark where this page stands, to come back to it later.
A mark keeps how far the page and each of its parts are scrolled.  With
a prefix argument, BACK, go to the newest mark instead, as
`canvas-browser-pop-mark\=' does.  A jump in the page sets the mark too:
to the top or the end, to an entry of the index, to the hit of a
search."
  (interactive "P" canvas-browser-mode)
  (if back
      (canvas-browser-pop-mark)
    (canvas-browser--push-mark)))

(defun canvas-browser-exchange-position-and-mark ()
  "Go to the newest mark of this page, and mark where the page stood."
  (interactive nil canvas-browser-mode)
  (let ((mark (canvas-browser--newest-mark)))
    (canvas-browser--position
     (lambda (here)
       (setcar canvas-browser--mark-ring here)
       (canvas-browser--go-to-position mark)))))

(defun canvas-browser-position-to-register (register)
  "Keep where this page stands in REGISTER.
`jump-to-register\=' comes back to it from any buffer: in the page
buffer that shows the page, or else in one that opens its address."
  (interactive (list (register-read-with-preview "Position of the page to register: "))
               canvas-browser-mode)
  (canvas-browser--position
   (lambda (position)
     (set-register register position)
     (message "canvas-browser: register %s holds where the page stands"
              (single-key-description register)))))

(cl-defmethod register-val-jump-to ((position canvas-browser-position) _arg)
  "Go to POSITION of a page: the content of a register."
  (let* ((url (canvas-browser-position-url position))
         (showing (canvas-browser--buffer-showing url)))
    (cond (showing
           (pop-to-buffer showing)
           (with-current-buffer showing
             (canvas-browser--go-to-position position)))
          ((derived-mode-p 'canvas-browser-mode)
           (canvas-browser--go-to-position position))
          (t (with-current-buffer (canvas-browser url)
               (setq canvas-browser--position-wanted position))))))

(cl-defmethod register-val-describe ((position canvas-browser-position) _verbose)
  "Say what POSITION of a page is: the content of a register."
  (princ (format "a position in the page %s"
                 (or (canvas-browser-position-title position)
                     (canvas-browser-position-url position)))))

;;;; The history of the pages that were visited

(defcustom canvas-browser-history-length 1000
  "How many pages the history keeps: those that were visited last.
With nil no visit is kept."
  :type '(choice (integer :tag "Pages") (const :tag "Keep no history" nil))
  :group 'canvas-browser)

(defcustom canvas-browser-history-file
  (expand-file-name "canvas-browser/history.eld"
                    (or (getenv "XDG_DATA_HOME") (expand-file-name "~/.local/share")))
  "The file that keeps the history between two sessions of Emacs.
With nil the history lasts as long as the session."
  :type '(choice file (const :tag "Keep the history in the session only" nil))
  :group 'canvas-browser)

(defvar canvas-browser--visited 'unread
  "The pages that were visited, the newest first, each (URL TITLE TIME).
TIME is the time of the last visit, in seconds.  It is `unread\=' until
the file of the history was read.")

(defvar canvas-browser--history-timer nil
  "The timer that writes the history to its file, while a visit waits for that.")

(defun canvas-browser--read-history ()
  "The history that its file keeps, or nil without a file."
  (let ((file canvas-browser-history-file))
    (when (and file (file-readable-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (read (current-buffer))))))

(defun canvas-browser--visited ()
  "The pages that were visited, the newest first, each (URL TITLE TIME).
The file of the history is read the first time."
  (when (eq canvas-browser--visited 'unread)
    (setq canvas-browser--visited (canvas-browser--read-history)))
  canvas-browser--visited)

(defun canvas-browser--write-history ()
  "Write the history to its file, for the reader alone to read.
A history that was never read is not written: it would take the place
of the one in the file."
  (when canvas-browser--history-timer
    (cancel-timer canvas-browser--history-timer)
    (setq canvas-browser--history-timer nil))
  (when (and canvas-browser-history-file (listp canvas-browser--visited))
    (make-directory (file-name-directory canvas-browser-history-file) t)
    (with-file-modes #o600
      (with-temp-file canvas-browser-history-file
        (let ((print-length nil) (print-level nil))
          (prin1 canvas-browser--visited (current-buffer)))))
    (set-file-modes canvas-browser-history-file #o600)))

(defun canvas-browser--write-history-if-changed ()
  "Write the history to its file when a visit still waits for that."
  (when canvas-browser--history-timer
    (canvas-browser--write-history)))

(add-hook 'kill-emacs-hook #'canvas-browser--write-history-if-changed)

(defun canvas-browser--save-history-later ()
  "Write the history to its file once Emacs has been idle for a while."
  (when (and canvas-browser-history-file (not canvas-browser--history-timer))
    (setq canvas-browser--history-timer
          (run-with-idle-timer 10 nil #'canvas-browser--write-history))))

(defun canvas-browser--visit-p ()
  "Whether the page of this buffer is one to keep in the history.
It is one that the reader opened, at an address to come back to.  A
page that another buffer embeds is part of that buffer."
  (and canvas-browser-history-length
       canvas-browser--url
       (not canvas-browser--host)
       (string-match-p "\\`\\(?:https?\\|file\\):" canvas-browser--url)))

(defun canvas-browser--remember-page ()
  "Note the page of this buffer as the newest visit of the history.
A page that was visited before stands there once, with its newest title
and time."
  (when (canvas-browser--visit-p)
    (let ((url canvas-browser--url))
      (setq canvas-browser--visited
            (seq-take (cons (list url (or canvas-browser--title url) (float-time))
                            (seq-remove (lambda (visit) (equal (car visit) url))
                                        (canvas-browser--visited)))
                      canvas-browser-history-length))
      (canvas-browser--save-history-later))))

(defun canvas-browser--history-candidates ()
  "The pages of the history, each (NAME . URL), the newest first.
NAME is the title and the address, as a link is offered."
  (mapcar (lambda (visit)
            (pcase-let ((`(,url ,title . ,_) visit))
              (cons (canvas-browser--link-name (if (equal title url) "" title) url) url)))
          (canvas-browser--visited)))

;;;###autoload
(defun canvas-browser-history (&optional other)
  "Open a page that was visited, chosen by its title or by its address.
The newest visit comes first.  In a page buffer the page opens there.
With a prefix argument, OTHER, or in any other buffer, it opens in a
page buffer of its own.  Embark acts on a page that is offered as on
any address.  The key of `consult-history\\=' runs this command in a page
buffer."
  (interactive "P")
  (let* ((candidates (or (canvas-browser--history-candidates)
                         (user-error "canvas-browser: no page was visited yet")))
         (url (cdr (assoc (completing-read "Visited page: "
                                           (canvas-browser--link-table candidates)
                                           nil t nil 'canvas-browser-link-history)
                          candidates))))
    (if (and (derived-mode-p 'canvas-browser-mode) (not other))
        (canvas-browser-open-url url)
      (canvas-browser url))))

;;;; The zoom and the picture

(defcustom canvas-browser-zoom-step 1.2
  "How much one zoom key changes the scale of the page."
  :type 'number
  :group 'canvas-browser)

(defvar-local canvas-browser--zoom 1.0
  "The scale of this page: 1.0 is its natural size.")

(defun canvas-browser--apply-zoom (&optional then)
  "Lay the page out for the zoom of this buffer, and scale it to the canvas.
THEN is called in this buffer once chromium has laid the page out: a
picture asked for before that is of the layout the page had."
  (let ((buffer (current-buffer)))
    (canvas-browser-cdp-send
     "Emulation.setDeviceMetricsOverride"
     (list :width (round (/ (car canvas-browser--size) canvas-browser--zoom))
           :height (round (/ (cdr canvas-browser--size) canvas-browser--zoom))
           :deviceScaleFactor canvas-browser--zoom
           :mobile :json-false)
     (when then
       (lambda (_result)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer (funcall then)))))
     canvas-browser--session)))

(defun canvas-browser--zoom-by-key (direction)
  "Zoom the page: DIRECTION is `in\=', `out\=' or `reset\='.
It is this buffer's `canvas-keys-zoom-function\='."
  (setq canvas-browser--zoom
        (pcase direction
          ('in (min 5.0 (* canvas-browser--zoom canvas-browser-zoom-step)))
          ('out (max 0.25 (/ canvas-browser--zoom canvas-browser-zoom-step)))
          ('reset 1.0)
          ('fit (user-error "canvas-browser: a page has nothing whole to fit"))
          (other (error "canvas-browser: %S is no zoom" other))))
  (canvas-browser--apply-zoom))

(defun canvas-browser-write-picture (file)
  "Write the last frame of this page to FILE."
  (interactive "FWrite the picture to: ")
  (unless canvas-browser--last-frame
    (user-error "canvas-browser: this page has drawn nothing yet"))
  (copy-file canvas-browser--last-frame (expand-file-name file) t)
  (message "canvas-browser: wrote %s" file))

(defun canvas-browser-refresh (&rest _)
  "Read the page again; this buffer's `revert-buffer-function\='."
  (interactive)
  (canvas-browser--tell "Page.reload" nil))

;;;; A picture of the window

(defun canvas-browser--paint-window ()
  "Ask chromium for a picture of the window, and paint it.
The screencast sends a frame when the page changes, and a page that has
settled changes no more: after the draw of a whole page the window would
else keep the frame it had before that draw.  The picture is a PNG: a
page that has stopped moving is read rather than watched, and the JPEG of
a frame shows its workings around small text.  It costs about a fifth of
a second to make, which is far too slow for a frame and nothing for a
page that is standing still."
  (let ((buffer (current-buffer))
        (commands canvas-browser--commands))
    (canvas-browser-cdp-send
     "Page.captureScreenshot"
     (list :format "png")
     (lambda (result)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when-let* (((= commands canvas-browser--commands))
                       (data (plist-get result :data)))
             (canvas-browser--paint-soon data)))))
     canvas-browser--session)))

;;;; The pointer over a link

(defcustom canvas-browser-spots-delay 0.3
  "Seconds of quiet before the pointer areas of the page are read again."
  :type 'number
  :group 'canvas-browser)

(defvar-local canvas-browser--spots-timer nil
  "The timer that reads the pointer areas of this page.")

(defun canvas-browser--hot-spots (boxes)
  "The image map of BOXES: a rectangle each, under a hand pointer."
  (mapcar (lambda (box)
            (let ((x (plist-get box :x))
                  (y (plist-get box :y)))
              `((rect . ((,x . ,y) . (,(+ x (plist-get box :w))
                                      . ,(+ y (plist-get box :h)))))
                canvas-browser-link
                (pointer hand))))
          boxes))

(defun canvas-browser--flush-image ()
  "Drop this buffer's canvas from the image cache, through a frame that shows it.
`image-flush' with FRAME t frees the image through a hidden child frame,
and the frame that draws the canvas then keeps a line that points at the
freed image.  canvas-diagram carries the same lesson."
  (let ((frames (seq-filter #'display-graphic-p
                            (delete-dups (mapcar #'window-frame
                                                 (canvas-browser--showing-windows
                                                  (current-buffer)))))))
    (if frames
        (progn (image-flush canvas-browser--canvas (car frames))
               (mapc #'redraw-frame (cdr frames)))
      (image-flush canvas-browser--canvas t))))

(defun canvas-browser--put-spots (boxes)
  "Make the hot spots of BOXES the map of this buffer's canvas.
An equal map stays, since a new map is a new image-cache key, and its
flush redraws the frame."
  (let ((spots (canvas-browser--hot-spots boxes)))
    (unless (equal spots (plist-get (cdr canvas-browser--canvas) :map))
      (canvas-browser--flush-image)
      (plist-put (cdr canvas-browser--canvas) :map spots))))


(defun canvas-browser--sync-spots (buffer)
  "Read the boxes of BUFFER's page, and put their hot spots on its canvas."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq canvas-browser--spots-timer nil)
      (when canvas-browser--session
        (canvas-browser--boxes
         (lambda (boxes)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (canvas-browser--put-spots boxes)))))))))

(defun canvas-browser--schedule-spots ()
  "Read the pointer areas of this page once it has been quiet for a moment."
  (when canvas-browser--spots-timer
    (cancel-timer canvas-browser--spots-timer))
  (setq canvas-browser--spots-timer
        (run-with-idle-timer canvas-browser-spots-delay nil
                             #'canvas-browser--sync-spots (current-buffer))))

;;;; The menu

(canvas-keys-define-menu canvas-browser-menu
    "The menu of a page buffer.
Its own columns stand in the first row, a word for each entry, and the
row every canvas menu carries comes below them: the zoom, the picture,
and the settings.  The widths line the columns of the two rows up."
  :column-widths '(13 13 24)
  ["Go"
   ("o" "open" canvas-browser-open-url)
   ("O" "new" canvas-browser)
   ("g" "reload" canvas-browser-refresh)
   ("b" "back" canvas-browser-back)
   ("F" "forward" canvas-browser-forward)
   ("B" "bookmark" canvas-browser-bookmark)
   ("J" "bookmarks" canvas-browser-open-bookmark)
   ("H" "history" canvas-browser-history)]
  ["Page"
   ("f" "hints" canvas-browser-hints)
   ("a" "links" canvas-browser-links)
   ("M-j" "jump" canvas-browser-caret-jump)
   ("TAB" "field" canvas-browser-next-field)
   ("E" "edit field" canvas-browser-edit-field)
   ("s" "find" canvas-browser-find)
   ("t" "text" canvas-browser-text)
   ("L" "lines" canvas-browser-search-text)
   ("e" "eww" canvas-browser-open-in-eww)
   ("y" "copy URL" canvas-browser-copy-url)
   (">" "send to" canvas-browser-send-to)]
  ["Modes"
   ("d" canvas-browser-toggle-dark :transient t
    :description (lambda () (canvas-keys-setting "dark" 'canvas-browser-dark)))
   ("i" canvas-browser-insert-mode
    :description (lambda () (canvas-keys-setting "typing" 'canvas-browser--insert)))
   ("v" canvas-browser-caret-mode
    :description (lambda () (canvas-keys-setting "caret" 'canvas-browser--caret)))
   ("S" canvas-browser-pick-part
    :description (lambda ()
                   (canvas-keys-describe "keys" (if canvas-browser--part "part" "page"))))
   ("N" canvas-browser-toggle-notifications :transient t
    :description canvas-browser--notify-description)])

(defvar canvas-browser-mode-map
  (define-keymap :parent (make-composed-keymap canvas-keys-mode-map special-mode-map))
  "Keymap of a page buffer in normal state.
The common canvas keys come from canvas-keys: `SPC\=' opens the menu,
`q\=' quits, `W\=' writes the picture, `C\=' customizes, and the zoom
keys zoom the page.  `g\=' is `revert-buffer\=', which reads it again.")

;; The keys are bound here and not where the map is made.  A variable
;; keeps its value when its file is loaded again, so keys bound there
;; would never reach a running Emacs.
(define-keymap :keymap canvas-browser-mode-map
  "M-p" #'canvas-browser-back
  "M-n" #'canvas-browser-forward
  "o" #'canvas-browser-open-url
  "O" #'canvas-browser
  "y" #'canvas-browser-copy-url
  ">" #'canvas-browser-send-to
  "B" #'canvas-browser-bookmark
  "J" #'canvas-browser-open-bookmark
  "H" #'canvas-browser-history
  "v" #'canvas-browser-caret-mode
  "M-j" #'canvas-browser-caret-jump
  ;; The key of avy jumps in the page, wherever it is bound, and even
  ;; from a map that beats this one, as `bind-key*' puts it.
  "<remap> <avy-goto-char-timer>" #'canvas-browser-caret-jump
  "i" #'canvas-browser-insert-mode
  "f" #'canvas-browser-hints
  "a" #'canvas-browser-links
  "S" #'canvas-browser-pick-part
  "C-g" #'canvas-browser-quit
  "TAB" #'canvas-browser-next-field
  "<backtab>" #'canvas-browser-previous-field
  "C-s" #'canvas-browser-find
  "C-r" #'canvas-browser-find-previous
  "d" #'canvas-browser-toggle-dark
  "n" #'canvas-browser-scroll-line-up
  "p" #'canvas-browser-scroll-line-down
  "t" #'canvas-browser-text
  "e" #'canvas-browser-open-in-eww
  "M-s M-l" #'canvas-browser-search-text
  "C-c '" #'canvas-browser-edit-field
  "<remap> <set-mark-command>" #'canvas-browser-set-mark
  "<remap> <exchange-point-and-mark>" #'canvas-browser-exchange-position-and-mark
  "<remap> <point-to-register>" #'canvas-browser-position-to-register
  "<remap> <consult-history>" #'canvas-browser-history
  "<remap> <imenu>" #'canvas-browser-imenu
  "<remap> <consult-imenu>" #'canvas-browser-imenu
  "z" #'canvas-keys-zoom-fit
  "<remap> <scroll-up-command>" #'canvas-browser-scroll-up
  "<remap> <scroll-down-command>" #'canvas-browser-scroll-down
  ;; A line key scrolls a line, as it moves a line in any other buffer.
  "<remap> <next-line>" #'canvas-browser-scroll-line-up
  "<remap> <previous-line>" #'canvas-browser-scroll-line-down
  "<remap> <beginning-of-buffer>" #'canvas-browser-beginning-of-page
  "<remap> <end-of-buffer>" #'canvas-browser-end-of-page
  ;; `pixel-scroll-precision-mode' takes the page keys for itself, and
  ;; its commands scroll a buffer, which a page is not.
  "<remap> <pixel-scroll-interpolate-down>" #'canvas-browser-scroll-up
  "<remap> <pixel-scroll-interpolate-up>" #'canvas-browser-scroll-down
  "<next>" #'canvas-browser-scroll-up
  "<prior>" #'canvas-browser-scroll-down
  "<home>" #'canvas-browser-beginning-of-page
  "<end>" #'canvas-browser-end-of-page)

;;;; The caret of the page

(defvar-local canvas-browser--caret nil
  "Whether the caret of the page has the keys.")

(defvar-local canvas-browser--caret-mark nil
  "Whether the caret of the page marks a region.")

(defvar-local canvas-browser--caret-box nil
  "Where the caret of the page stands, as a box, or nil.")

(defvar-local canvas-browser--caret-text nil
  "The text that the caret of the page marks, as the page last told it.")

(defconst canvas-browser--caret-js
  (concat "(function () {
     const version = 3;
     if ((window.__canvasBrowserCaret || {}).version === version) return;" canvas-browser--reach-js "
     const barId = '__canvas-browser-caret';
     let colour = '#ff8c00', edge = '#000000';
     let places = [];
     const paint = (c, e) => { if (c) colour = c; if (e) edge = e; };
     const leaveFocus = () => {
       const focus = document.activeElement;
       if (!focus || focus === document.body || focus === document.documentElement) return null;
       if (focus.blur) focus.blur();
       return focus;
     };
     const bar = () => {
       let b = document.getElementById(barId);
       if (!b) {
         b = document.createElement('div');
         b.id = barId;
         b.style.cssText = 'position:absolute;width:2px;pointer-events:none;' +
                           'z-index:2147483647;display:none';
         document.documentElement.appendChild(b);
       }
       b.style.background = colour;
       b.style.boxShadow = '0 0 0 1px ' + edge;
       return b;
     };
     const caretRect = s => {
       if (!s.focusNode) return null;
       const r = document.createRange();
       try { r.setStart(s.focusNode, s.focusOffset); } catch (other) { return null; }
       r.collapse(true);
       const rects = r.getClientRects();
       let rect = rects.length ? rects[0] : null;
       if (!rect || rect.height === 0) {
         const el = s.focusNode.nodeType === 1 ? s.focusNode : s.focusNode.parentElement;
         const e = el && el.getBoundingClientRect();
         rect = e && {left: e.left, top: e.top, height: Math.min(e.height, 24),
                      bottom: e.top + Math.min(e.height, 24)};
       }
       return rect;
     };
     const report = () => {
       const s = getSelection();
       let rect = caretRect(s);
       const b = bar();
       if (rect && rect.height > 0) {
         if (rect.top < 0 || rect.bottom > innerHeight) {
           window.scrollBy(0, rect.top < 0 ? rect.top - innerHeight / 3
                                           : rect.bottom - innerHeight * 2 / 3);
           rect = caretRect(s);
         }
         b.style.left = (rect.left + scrollX) + 'px';
         b.style.top = (rect.top + scrollY) + 'px';
         b.style.height = rect.height + 'px';
         b.style.display = 'block';
       } else {
         b.style.display = 'none';
       }
       let region = null;
       if (s.rangeCount && !s.isCollapsed) {
         const r = s.getRangeAt(0).getBoundingClientRect();
         region = [Math.round(r.left), Math.round(r.top), Math.round(r.width), Math.round(r.height)];
       }
       return {box: rect && rect.height > 0
                    ? [Math.round(rect.left), Math.round(rect.top), 2, Math.round(rect.height)]
                    : null,
               text: s.toString(), region: region};
     };
     const firstTextInView = () => {
       for (let y = 8; y < innerHeight; y += 16)
         for (const x of [8, innerWidth / 4, innerWidth / 2, innerWidth * 3 / 4]) {
           const r = document.caretRangeFromPoint(x, y);
           if (r && r.startContainer.nodeType === 3 && r.startContainer.textContent.trim()) {
             r.setStart(r.startContainer, 0);
             r.collapse(true);
             return r;
           }
         }
       return null;
     };
     const inView = s => { const r = caretRect(s); return r && r.top >= 0 && r.bottom <= innerHeight; };
     const textRoots = () => {
       const roots = [document];
       walk(document, '*', e => { if (e.shadowRoot) roots.push(e.shadowRoot); });
       return roots;
     };
     const matchBox = (node, offset, length) => {
       const r = document.createRange();
       r.setStart(node, offset);
       r.setEnd(node, offset + length);
       const rect = r.getClientRects()[0];
       if (!rect || rect.width === 0 || rect.height === 0 || rect.left < 0 || rect.top < 0 ||
           rect.right > innerWidth || rect.bottom > innerHeight) return null;
       return {x: Math.round(rect.left), y: Math.round(rect.top),
               w: Math.round(rect.width), h: Math.round(rect.height)};
     };
     const inside = (outer, e) => { for (let n = e; n; n = up(n)) if (n === outer) return true; return false; };
     const alpha = colour => {
       if (colour === 'transparent') return 0;
       const m = colour.match(/^rgba?\\(([^)]*)\\)$/);
       if (!m) return 1;
       const parts = m[1].split(/[\\s,\\/]+/).filter(Boolean);
       return parts.length > 3 ? parseFloat(parts[3]) : 1;
     };
     const paints = e => {
       if (/^(IMG|VIDEO|CANVAS|IFRAME)$/.test(e.tagName)) return true;
       const s = getComputedStyle(e);
       return s.backgroundImage !== 'none' || alpha(s.backgroundColor) > 0.5;
     };
     const stack = (root, x, y) => root.elementsFromPoint(x, y)
       .filter(e => e.getRootNode() === root)
       .flatMap(e => e.shadowRoot ? [...stack(e.shadowRoot, x, y), e] : [e]);
     const uncovered = (parent, box) => {
       const [x, y] = middle(box);
       for (const e of stack(document, x, y)) {
         if (inside(e, parent) || inside(parent, e)) return true;
         if (paints(e)) return false;
       }
       return false;
     };
     const hidesAll = s => s.clip === 'rect(0px, 0px, 0px, 0px)' || /^inset\\(50%/.test(s.clipPath);
     const clipsAll = s => s.clip !== 'auto' || s.clipPath !== 'none';
     const shownArea = e => {
       let left = 0, top = 0, right = innerWidth, bottom = innerHeight, escape = 'static';
       for (let n = e; n; n = up(n)) {
         if (n.nodeType !== 1 || n === document.body || n === document.documentElement) continue;
         const s = getComputedStyle(n);
         if (hidesAll(s)) return null;
         const positioned = s.position !== 'static';
         const overflows = s.overflow !== 'visible' &&
                           (escape === 'static' || (escape === 'absolute' && positioned));
         if (overflows || clipsAll(s)) {
           const r = n.getBoundingClientRect();
           left = Math.max(left, r.left); top = Math.max(top, r.top);
           right = Math.min(right, r.right); bottom = Math.min(bottom, r.bottom);
         }
         if (escape === 'fixed' || s.position === 'fixed') escape = 'fixed';
         else if (s.position === 'absolute') escape = 'absolute';
         else if (positioned) escape = 'static';
       }
       return left < right && top < bottom ? {left, top, right, bottom} : null;
     };
     const within = (box, area) => {
       const [x, y] = middle(box);
       return x >= area.left && x <= area.right && y >= area.top && y <= area.bottom;
     };
     const seen = e => e.checkVisibility({opacityProperty: true, visibilityProperty: true,
                                          checkOpacity: true, checkVisibilityCSS: true});
     const nearView = e => {
       const r = e.getBoundingClientRect();
       return r.bottom >= 0 && r.top <= innerHeight && r.right >= 0 && r.left <= innerWidth;
     };
     const matchesIn = (node, text, fold) => {
       const lower = node.data.toLowerCase();
       const haystack = fold && lower.length === node.data.length ? lower : node.data;
       if (haystack.indexOf(text) < 0) return [];
       const parent = node.parentElement || (node.parentNode && node.parentNode.host);
       if (!parent || !nearView(parent) || !seen(parent)) return [];
       const area = shownArea(parent);
       if (!area) return [];
       const matches = [];
       for (let i = haystack.indexOf(text); i >= 0; i = haystack.indexOf(text, i + text.length)) {
         const box = matchBox(node, i, text.length);
         if (box && within(box, area) && uncovered(parent, box)) matches.push({node, offset: i, box});
       }
       return matches;
     };
     window.__canvasBrowserCaret = {
       version,
       colours(c, e) { paint(c, e); return this; },
       start(c, e) {
         paint(c, e);
         const s = getSelection();
         const focus = leaveFocus();
         if (focus) {
           const r = document.createRange();
           r.setStartAfter(focus);
           r.collapse(true);
           s.removeAllRanges();
           s.addRange(r);
         } else if (!(s.rangeCount && inView(s))) {
           const r = firstTextInView();
           if (r) { s.removeAllRanges(); s.addRange(r); }
         }
         return report();
       },
       move(alter, direction, granularity) {
         getSelection().modify(alter, direction, granularity);
         return report();
       },
       collapse() {
         const s = getSelection();
         if (s.focusNode) s.collapse(s.focusNode, s.focusOffset);
         return report();
       },
       find(text) {
         if (!text) throw new Error('canvas-browser: a jump needs text to look for');
         const fold = text === text.toLowerCase();
         places = [];
         for (const root of textRoots()) {
           const walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT);
           for (let node = walker.nextNode(); node; node = walker.nextNode())
             places.push(...matchesIn(node, text, fold));
         }
         return places.map(place => place.box);
       },
       jump(i, extend) {
         const place = places[i];
         if (!place) throw new Error('canvas-browser: no place ' + i + ' to jump to');
         const s = getSelection();
         if (extend && s.rangeCount) s.extend(place.node, place.offset);
         else { leaveFocus(); s.collapse(place.node, place.offset); }
         return report();
       },
       report,
       stop() {
         const b = document.getElementById(barId);
         if (b) b.remove();
         return {box: null, text: ''};
       }
     };
   })();")
  "The JavaScript of the caret of a page, `window.__canvasBrowserCaret\\='.
The caret is the selection of the page: a collapsed one is the caret, and
one that is not is the region, which chromium draws itself.  A page that
takes no typing draws no caret, so a bar of the colour of the cursor of
Emacs stands where it is.  Every call answers where the caret stands,
what the region holds, and the box of the region.  It is put in every
time it is used, since a new page has none, and replaces a caret of
another version that a page holds from before.
`find\\=' gives the boxes of the places in view that show a text, and
keeps them for `jump\\=', which puts the caret on one, or marks to it.
A place counts where its text shows: it is not invisible, no box that
clips it cuts it off, as a heading for screen readers only is cut, and
nothing that paints lies over its middle.  A clear link laid over a
card, as over a post of Reddit, paints nothing, while the veil of a
dialog does.  A text that runs across two elements, into a bold word
say, is not found.")

(defun canvas-browser--caret-script (call)
  "The script that runs CALL, a method of the caret of the page."
  (concat canvas-browser--caret-js "window.__canvasBrowserCaret." call))

(defun canvas-browser--caret-call (call &optional then)
  "Run CALL, a method of the caret of the page, and follow where it moves.
THEN, when given, is called in this buffer with the answer."
  (canvas-browser--evaluate-here
   (canvas-browser--caret-script call)
   (lambda (report)
     (let ((to (canvas-browser--box-of (plist-get report :box))))
       (canvas-browser--fly canvas-browser--caret-box to)
       (setq canvas-browser--caret-box to
             canvas-browser--caret-text (plist-get report :text)))
     (when then (funcall then report)))))

(defun canvas-browser--caret-colours ()
  "The colours of the caret of the page, as (FILL EDGE) in CSS.
The fill is the colour of the cursor of Emacs, and the edge black or
white, whichever stands out from it: a white bar alone vanishes on a
white page, and a black one on a black page."
  (let* ((rgb (or (color-name-to-rgb (or (face-background 'cursor nil t) "orange"))
                  '(1.0 0.55 0.0)))
         (light (> (+ (* 0.299 (nth 0 rgb)) (* 0.587 (nth 1 rgb)) (* 0.114 (nth 2 rgb))) 0.5)))
    (list (apply #'color-rgb-to-hex (append rgb '(2)))
          (if light "#000000" "#ffffff"))))

(defun canvas-browser--with-colours (method)
  "A call of METHOD of the caret of the page, given the colours of the caret."
  (apply #'format (concat method "(%S, %S)") (canvas-browser--caret-colours)))

(defconst canvas-browser--caret-motions
  '(("C-f" "forward" "character") ("<right>" "forward" "character")
    ("C-b" "backward" "character") ("<left>" "backward" "character")
    ("M-f" "forward" "word") ("M-b" "backward" "word")
    ("C-n" "forward" "line") ("<down>" "forward" "line")
    ("C-p" "backward" "line") ("<up>" "backward" "line")
    ("C-a" "backward" "lineboundary") ("<home>" "backward" "lineboundary")
    ("C-e" "forward" "lineboundary") ("<end>" "forward" "lineboundary")
    ("M-<" "backward" "documentboundary") ("M->" "forward" "documentboundary"))
  "The motions of Emacs, as the selection of a page makes them.
Each is the key, the direction, and how far a step goes.")

(defvar canvas-browser-caret-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map canvas-browser-mode-map)
    (dolist (motion canvas-browser--caret-motions)
      (define-key map (kbd (car motion)) #'canvas-browser-caret-move))
    (define-key map (kbd "C-SPC") #'canvas-browser-caret-set-mark)
    (define-key map (kbd "C-@") #'canvas-browser-caret-set-mark)
    (define-key map (kbd "M-w") #'canvas-browser-caret-copy)
    (define-key map (kbd "C-g") #'canvas-browser-caret-quit)
    (define-key map (kbd "<escape>") #'canvas-browser-caret-leave)
    (define-key map (kbd "v") #'canvas-browser-caret-leave)
    map)
  "Keymap of a page buffer while the caret of the page has the keys.
The motions of Emacs move the caret, and every other key is the page's.")

(defun canvas-browser-caret-mode ()
  "Move a caret through the text of the page with the motions of Emacs.
The caret starts after what has the focus, a field you typed in say, or
at the first text in view.  `C-SPC\\=' marks a region, `M-w\\=' copies it,
and `C-g\\=' drops the mark, and then leaves, as `ESC\\=' does."
  (interactive)
  (canvas-browser--caret-enter)
  (canvas-browser--caret-call (canvas-browser--with-colours "start")))

(defun canvas-browser--caret-enter ()
  "Give the keys to the caret of the page, which has no mark and no place yet."
  (setq canvas-browser--caret t
        canvas-browser--caret-mark nil
        canvas-browser--caret-box nil)
  (use-local-map canvas-browser-caret-map)
  (canvas-browser--keep-modal-state)
  (force-mode-line-update)
  (message "canvas-browser: the caret moves with the keys of Emacs; C-SPC marks, M-w copies"))

(defun canvas-browser--caret-take-region (text)
  "Give the keys to the caret, with TEXT as its region.
TEXT is what a drag of the mouse marked on the page.  Nothing is copied,
as nothing is after a drag in a buffer: `M-w\=' copies the region and
`C-g\=' drops it.  One who set `mouse-drag-copy-region\=' gets the copy
here as well.  In a field the keys are the page's, and the field copies
its own mark."
  (when (and (stringp text) (not (string-empty-p text)) (not canvas-browser--insert))
    (unless canvas-browser--caret (canvas-browser--caret-enter))
    (setq canvas-browser--caret-mark t)
    (canvas-browser--caret-call
     (concat (canvas-browser--with-colours "colours") ".report()"))
    (when mouse-drag-copy-region (kill-new text))))

(defun canvas-browser-caret-move ()
  "Move the caret as the key that called this command moves point.
With the mark set it marks, as a motion after `C-SPC\\=' does."
  (interactive)
  (let* ((keys (key-description (vector last-command-event)))
         (motion (or (cdr (assoc keys canvas-browser--caret-motions))
                     (error "canvas-browser: %s is no motion of the caret" keys))))
    (canvas-browser--caret-call
     (format "move('%s', '%s', '%s')"
             (if canvas-browser--caret-mark "extend" "move") (car motion) (cadr motion)))))

(defun canvas-browser-caret-set-mark ()
  "Set the mark at the caret, or drop it when it is set."
  (interactive)
  (if canvas-browser--caret-mark
      (canvas-browser--caret-drop-mark)
    (setq canvas-browser--caret-mark t)
    (message "Mark set")))

(defun canvas-browser--caret-drop-mark ()
  "Drop the mark of the caret, and the region it marks."
  (setq canvas-browser--caret-mark nil)
  (canvas-browser--caret-call "collapse()"))

(defun canvas-browser-caret-copy ()
  "Copy the region of the caret to the kill ring, pulse it, and drop the mark."
  (interactive)
  (let ((buffer (current-buffer)))
    (canvas-browser--caret-call
     "report()"
     (lambda (report)
       (let ((text (plist-get report :text)))
         (if (and (stringp text) (not (string-empty-p text)))
             (progn (kill-new text)
                    (when-let* ((region (canvas-browser--box-of (plist-get report :region))))
                      (canvas-browser--pulse buffer region))
                    (message "canvas-browser: copied %d characters" (length text)))
           (message "canvas-browser: nothing is marked")))
       (canvas-browser--caret-drop-mark)))))

(defun canvas-browser-caret-quit ()
  "Drop the mark of the caret when it is set, else leave the caret."
  (interactive)
  (if canvas-browser--caret-mark
      (canvas-browser--caret-drop-mark)
    (canvas-browser-caret-leave)))

(defun canvas-browser-caret-leave ()
  "Give the keys back to Emacs, and take the caret off the page."
  (interactive)
  (canvas-browser--caret-call "stop()")
  (setq canvas-browser--caret nil
        canvas-browser--caret-mark nil)
  (use-local-map canvas-browser-mode-map)
  (canvas-browser--keep-modal-state)
  (force-mode-line-update))

;;;; Jumping to text, as avy does

(defvar avy-timeout-seconds)
(defvar avy-single-candidate-jump)

(defun canvas-browser--jump-timeout ()
  "How long a pause ends the text of a jump: avy\='s pause, once avy is loaded."
  (if (boundp 'avy-timeout-seconds) avy-timeout-seconds 0.5))

(defun canvas-browser--read-jump-text ()
  "Read the text of a jump until a pause, as `avy-goto-char-timer\=' reads it.
`DEL\=' takes the last character back, `RET\=' ends at once, and `ESC\=' gives
up.  The text, or nil when given up or empty."
  (let ((text (catch 'done
                (let ((text ""))
                  (while t
                    (setq text (canvas-browser--jump-text-key text)))))))
    (and text (not (string-empty-p text)) text)))

(defun canvas-browser--jump-text-key (text)
  "Read one key of the text of a jump, which holds TEXT so far; the new text.
The first key is waited for; after it, a pause ends the text."
  (let ((key (read-char (format "Jump to: %s" text) t
                        (and (not (string-empty-p text)) (canvas-browser--jump-timeout)))))
    (cond ((memq key '(nil ?\r)) (throw 'done text))
          ((eq key ?\e) (throw 'done nil))
          ((memq key '(?\d ?\b)) (substring text 0 (max 0 (1- (length text)))))
          (t (concat text (char-to-string key))))))

(defun canvas-browser-caret-jump ()
  "Put the caret on text in view that you type, as `avy-goto-char-timer\=' does.
Type until you pause; every place in view that shows the text takes a
hint, and naming one puts the caret there, or marks to it when the mark
is set.  From normal state the caret starts there.  Text in lower case
matches either case, and a single place takes no hint, as in avy."
  (interactive)
  (when-let* ((text (canvas-browser--read-jump-text)))
    (canvas-browser--evaluate-here
     (canvas-browser--caret-script (format "find(%s)" (json-encode text)))
     (lambda (boxes) (canvas-browser--jump-to-one text boxes)))))

(defun canvas-browser--jump-to-one (text boxes)
  "Put the caret on the place named of BOXES, the places of TEXT in view."
  (cond ((null boxes) (message "canvas-browser: no %S in view" text))
        ((and (null (cdr boxes)) (canvas-browser--single-jump-p))
         (canvas-browser--caret-jump-to 0))
        (t (when-let* ((chosen (canvas-browser--choose-box boxes)))
             (canvas-browser--caret-jump-to (car chosen))))))

(defun canvas-browser--single-jump-p ()
  "Whether a single place is jumped to without a hint, as avy has it."
  (or (not (boundp 'avy-single-candidate-jump)) avy-single-candidate-jump))

(defun canvas-browser--caret-jump-to (place)
  "Put the caret on PLACE of the last jump, or mark to it with the mark set."
  (let ((extend canvas-browser--caret-mark))
    (unless canvas-browser--caret
      (canvas-browser--caret-enter))
    (canvas-browser--caret-call
     (format "%s.jump(%d, %s)" (canvas-browser--with-colours "colours")
             place (if extend "true" "false")))))

;;;; The window and the header line

(defun canvas-browser--window-resized (width height)
  "Lay the page out for WIDTH by HEIGHT, and ask for frames of that size."
  (unless (equal canvas-browser--size (cons width height))
    (canvas-browser--adopt width height)
    (canvas-browser--start-screencast)
    (canvas-browser--resize width height #'canvas-browser--fill-window)))

(defcustom canvas-browser-second-try 0.5
  "Seconds before a window still empty is asked for its picture again."
  :type 'number
  :group 'canvas-browser)

(defun canvas-browser--fill-window ()
  "Ask for a picture of the window, now and once more if it stays empty.
The canvas of a window that has just changed size holds nothing, and a
page that has settled sends no frame of its own; the first picture may
also come before chromium has laid the page out for the new window."
  (canvas-browser--paint-window)
  (let ((buffer (current-buffer))
        (frames canvas-browser--frames))
    (run-with-timer canvas-browser-second-try nil
                    #'canvas-browser--paint-if-quiet buffer frames)))

(defun canvas-browser--watch-freshness ()
  "Look every now and then whether this window is still being painted."
  (unless canvas-browser--fresh-timer
    (setq canvas-browser--fresh-timer
          (run-with-timer canvas-browser-fresh-interval canvas-browser-fresh-interval
                          #'canvas-browser--keep-fresh (current-buffer)))))

(defun canvas-browser--may-ask-p (buffer)
  "Whether BUFFER\='s page can be asked for a picture of itself now.
No session, no chromium, hints waiting to be named, or no window showing
the buffer: each of them means no."
  (and canvas-browser--session
       (canvas-browser-cdp-running-p)
       (not canvas-browser--hinting)
       (canvas-browser--shown-p buffer)))

(defun canvas-browser--keep-fresh (buffer)
  "Ask BUFFER's window for a picture of itself if it painted nothing.
An embedded page whose host no longer holds its picture goes instead."
  (cond
   ((not (buffer-live-p buffer)) nil)
   ((canvas-browser--embed-gone-p buffer) (kill-buffer buffer))
   (t
    (with-current-buffer buffer
      (when (get-buffer-window buffer t)
        (canvas-browser--keep-modal-state))
      (when (canvas-browser--may-ask-p buffer)
        ;; A canvas that holds the still picture of this page holds the
        ;; page as it is, and a page whose moving parts are known has a
        ;; clock of its own: asking for another picture here would have
        ;; chromium draw for nobody.
        (if (or canvas-browser--crisp
                canvas-browser--live-boxes
                (/= canvas-browser--frames canvas-browser--fresh-frames))
            (setq canvas-browser--fresh-frames canvas-browser--frames
                  canvas-browser--quiet 0)
          (canvas-browser--paint-window)
          (cl-incf canvas-browser--quiet)
          (when (= canvas-browser--quiet canvas-browser--quiet-looks)
            (message (concat "canvas-browser: the page has stopped answering;"
                             " g reads it again")))))))))

(defun canvas-browser--paint-if-quiet (buffer frames)
  "Ask BUFFER for a picture of its window if it has painted none since FRAMES."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (= canvas-browser--frames frames)
        (canvas-browser--paint-window)))))

(defun canvas-browser--window-change (window)
  "Fit the page of WINDOW's buffer to WINDOW."
  (when (and canvas-browser--session (window-live-p window))
    (canvas-browser--window-resized (window-body-width window t)
                                    (window-body-height window t))))

(defun canvas-browser--header ()
  "The header line: the title, the URL, the state of the keyboard, and
whether a part of the page is picked."
  (string-join (delq nil (list (or canvas-browser--title "…")
                               canvas-browser--url
                               (when canvas-browser--insert "insert")
                               (when canvas-browser--caret "caret")
                               (when canvas-browser--part "part")))
               " · "))

(defvar canvas-browser-insert-map (make-sparse-keymap)
  "Keymap of a page buffer in insert state: every key goes to the page.")

(defun canvas-browser--bind-undo-keys (map)
  "Bind the keys and the commands that undo in Emacs to the undo of the field.
A page buffer holds the picture of a page and no text of its own, so
the only thing to undo there is what was typed into the page.  The
commands are bound as well as the keys: a key that the reader gave to
undo then reaches the field, and a package that takes the undo keys of
every buffer leaves alone a mode that has an undo of its own, as
undo-tree does."
  (dolist (key '("C-/" "C-_"))
    (define-key map (kbd key) #'canvas-browser-field-undo))
  (dolist (key '("C-?" "C-M-_"))
    (define-key map (kbd key) #'canvas-browser-field-redo))
  ;; The key that shows the undo history where a package gives it that.
  (define-key map (kbd "C-x u") #'canvas-browser-field-history)
  (define-key map [remap undo] #'canvas-browser-field-undo)
  (define-key map [remap undo-redo] #'canvas-browser-field-redo))

(defun canvas-browser--bind-insert-keys (map)
  "Bind the keys of insert state in MAP.
A variable keeps its value when its file is loaded again, so keys bound
where the map is made would never reach a running Emacs.  They are bound
here, on every load."
  (define-key map [remap self-insert-command] #'canvas-browser-self-insert)
  (dolist (key canvas-browser--insert-keys)
    (define-key map (kbd (car key)) #'canvas-browser-send-key))
  (define-key map (kbd "C-k") #'canvas-browser-kill-line)
  (define-key map (kbd "C-y") #'canvas-browser-yank)
  (define-key map (kbd "S-<insert>") #'canvas-browser-yank)
  (define-key map [mouse-2] #'canvas-browser-yank)
  (canvas-browser--bind-undo-keys map)
  (define-key map (kbd "TAB") #'canvas-browser-next-field)
  (define-key map (kbd "<backtab>") #'canvas-browser-previous-field)
  (define-key map (kbd "<escape>") #'canvas-browser-normal-mode)
  ;; Another package may have taken `ESC' for itself, as meow does.
  (define-key map (kbd "C-g") #'canvas-browser-insert-quit)
  (define-key map (kbd "C-SPC") #'canvas-browser-field-set-mark)
  (define-key map (kbd "C-@") #'canvas-browser-field-set-mark)
  (define-key map (kbd "C-x h") #'canvas-browser-field-mark-whole)
  (define-key map (kbd "M-w") #'canvas-browser-field-copy)
  (define-key map (kbd "C-w") #'canvas-browser-field-cut)
  ;; A page takes the focus for a field of its own, as an application
  ;; does when it opens a talk.  Without this key here, the key of
  ;; `consult-line' would then search the page buffer, which holds no
  ;; text.
  (define-key map (kbd "M-s M-l") #'canvas-browser-search-text)
  (define-key map (kbd "C-c '") #'canvas-browser-edit-field)
  (define-key map [remap point-to-register] #'canvas-browser-position-to-register)
  (define-key map [remap consult-history] #'canvas-browser-history)
  (define-key map [remap imenu] #'canvas-browser-imenu)
  (define-key map [remap consult-imenu] #'canvas-browser-imenu))

(canvas-browser--bind-insert-keys canvas-browser-insert-map)
(canvas-browser--bind-undo-keys canvas-browser-mode-map)

(declare-function eww "eww" (url &optional arg))

(defun canvas-browser-open-in-eww ()
  "Open the URL of this page in eww, which reads it as text."
  (interactive)
  (unless canvas-browser--url
    (user-error "canvas-browser: this buffer holds no URL"))
  (eww canvas-browser--url))

(defun canvas-browser-insert-mode ()
  "Send every key to the page until `ESC'."
  (interactive)
  (setq canvas-browser--insert t)
  (use-local-map canvas-browser-insert-map)
  (canvas-browser--keep-modal-state)
  (message "canvas-browser: what you type goes to the page; ESC or C-g stops"))

(defvar meow--current-state)
(defvar meow-mode-state-list)
(declare-function meow--switch-state "meow-core" (state))

(defun canvas-browser--keep-modal-state ()
  "Ask for the modal state this mode was given, if something changed it.
`ESC\=' leaves the typing state of a page, and meow takes that key for
its own state, which would leave the buffer in a state where `SPC\=' is
meow's keypad rather than the menu of the page."
  (when (and (fboundp 'meow--switch-state)
             (boundp 'meow-mode-state-list)
             (boundp 'meow--current-state))
    (when-let* ((state (alist-get 'canvas-browser-mode meow-mode-state-list)))
      (unless (eq meow--current-state state)
        (meow--switch-state state)))))

(defun canvas-browser-normal-mode ()
  "Keep the keys of Emacs again."
  (interactive)
  (setq canvas-browser--insert nil
        canvas-browser--field-mark nil)
  (use-local-map canvas-browser-mode-map)
  (canvas-browser--keep-modal-state)
  (message "canvas-browser: the keys are Emacs's again"))

(defun canvas-browser--bind-clicks (map click)
  "Bind the clicks of the mouse in MAP to CLICK, a command.
An area of the image map puts `canvas-browser-link\=' before the event,
so that id takes a map of its own, whose parent is MAP: a click over a
link runs the same command, and an event that nothing binds is ignored,
rather than telling the reader that it is undefined."
  (define-key map [down-mouse-1] #'ignore)
  (dolist (event '([mouse-1] [double-mouse-1] [triple-mouse-1]
                   [double-down-mouse-1] [triple-down-mouse-1]))
    (define-key map event click))
  (let ((link (make-sparse-keymap)))
    (set-keymap-parent link map)
    (define-key link [t] #'ignore)
    (define-key map [canvas-browser-link] link))
  map)

(defun canvas-browser--bind-mouse (map)
  "Bind the mouse in MAP.
A click reaches the page, a drag marks in it, and the wheel scrolls it.
Emacs reports a fast wheel turn as a double or triple event."
  (dolist (turn '("" "double-" "triple-"))
    (dolist (direction '("down" "up"))
      (define-key map (vector (intern (format "%swheel-%s" turn direction)))
                  #'canvas-browser-wheel)))
  (define-key map [drag-mouse-1] #'canvas-browser-drag)
  (canvas-browser--bind-clicks map #'canvas-browser-click))

(canvas-browser--bind-mouse canvas-browser-mode-map)
;; The mouse works while you type in the page as well: the state is about
;; the keys, and a click over a link would else say it is undefined.
(canvas-browser--bind-mouse canvas-browser-insert-map)

(defvar canvas-minimap-exclude-modes)

(defun canvas-browser--leave-out-of-map ()
  "Tell canvas-minimap to leave a page buffer alone.
A page is a picture, not lines of text, and a map of it told the reader
nothing the window does not.  Take the mode out of
`canvas-minimap-exclude-modes\=' to have the strip back."
  (add-to-list 'canvas-minimap-exclude-modes 'canvas-browser-mode))

(with-eval-after-load 'canvas-minimap (canvas-browser--leave-out-of-map))

(define-derived-mode canvas-browser-mode special-mode "Browser"
  "Major mode of a buffer that shows a web page on a canvas."
  (setq cursor-type nil
        truncate-lines t)
  (setq header-line-format '(:eval (canvas-browser--header)))
  ;; The text of the buffer is the picture of the page.  A new picture
  ;; is no step that the reader can take back.
  (buffer-disable-undo)
  ;; `revert-buffer-function' is not buffer-local by itself: a plain setq
  ;; would make every other buffer read a page again.
  (setq-local revert-buffer-function #'canvas-browser-refresh)
  (setq-local bookmark-make-record-function #'canvas-browser-bookmark-make-record)
  (setq-local imenu-create-index-function #'canvas-browser-imenu-index)
  (setq canvas-keys-menu-command #'canvas-browser-menu
        canvas-keys-write-command #'canvas-browser-write-picture
        canvas-keys-copy-function #'canvas-browser-copy-block
        canvas-keys-group 'canvas-browser
        canvas-keys-zoom-function #'canvas-browser--zoom-by-key
        canvas-keys-redraw-function #'canvas-browser-refresh)
  (add-hook 'kill-buffer-hook #'canvas-browser--release nil t)
  ;; Any command may change the page anywhere, so what was moving before
  ;; it says nothing about the page after it.
  (add-hook 'pre-command-hook #'canvas-browser--forget-live nil t)
  (add-hook 'post-command-hook #'canvas-browser--field-read-later nil t)
  (canvas-browser--embark-here)
  (canvas-browser--watch-freshness))

(defun canvas-browser--buffer-name (url)
  "The name of the page buffer that shows URL."
  (format "*canvas-browser: %s*" url))

(defun canvas-browser--page-buffer (url)
  "A new page buffer for URL."
  (let ((buffer (generate-new-buffer (canvas-browser--buffer-name url))))
    (with-current-buffer buffer (canvas-browser-mode))
    buffer))

;;;###autoload
(defun canvas-browser (url)
  "Open URL in a page buffer of its own."
  (interactive "sURL: ")
  (let* ((url (canvas-browser--reachable-url url))
         (buffer (canvas-browser--page-buffer url)))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (let ((window (get-buffer-window buffer)))
        (canvas-browser--open url
                              (window-body-width window t)
                              (window-body-height window t))))
    buffer))

;;;; As the browser of Emacs

(defcustom canvas-browser-fallback-browser #'eww-browse-url
  "The browser `canvas-browser-browse-url' hands a URL to where no canvas shows.
It is called as `browse-url' calls a browser: with the URL and the
arguments `browse-url' passed on."
  :type 'function
  :group 'canvas-browser)

(defun canvas-browser-can-show-p ()
  "Return non-nil when the selected frame can show a page on a canvas.
A terminal frame cannot, and nor can an Emacs without canvas images."
  (and (display-graphic-p) (image-type-available-p 'canvas)))

;;;###autoload
(defun canvas-browser-browse-url (url &rest args)
  "Open URL in a page buffer of its own, as `browse-url' asks a browser to.
Set `browse-url-browser-function' to this, and every link Emacs opens
opens here.  ARGS, such as the new-window flag, do not matter: every
page is a buffer of its own.  Where no canvas can show the page, the URL
and ARGS go to `canvas-browser-fallback-browser' instead.  Return the
page buffer, or what the fallback returns."
  (if (canvas-browser-can-show-p)
      (canvas-browser url)
    (apply canvas-browser-fallback-browser url args)))

;;;; A file for a page, picked in dired

(declare-function dired-get-marked-files "dired"
                  (&optional localp arg filter distinguish-one-marked error))
(declare-function dired-other-window "dired" (dirname &optional switches))
(declare-function dired-get-file-for-visit "dired" ())
(declare-function dired-find-file "dired" ())

(defvar canvas-browser--attach-directory nil
  "The directory that a file was last given to a page from, or nil.")

(defvar canvas-browser--chooser nil
  "The choice of files that a page waits for, or nil.
A plist: :buffer is the page buffer, :node the field of the page that
asked, and :multiple whether the field takes several files.")

(defvar canvas-browser-attach-mode-map (make-sparse-keymap)
  "Keymap of a dired buffer while a page waits for files.")

;; Bound here and not where the map is made, so that a second load of
;; this file brings a new key to a running Emacs.
(keymap-set canvas-browser-attach-mode-map "C-c C-c" #'canvas-browser-attach-send)
(keymap-set canvas-browser-attach-mode-map "C-c C-k" #'canvas-browser-attach-cancel)
(keymap-set canvas-browser-attach-mode-map "<remap> <dired-find-file>"
            #'canvas-browser-attach-open)

(defun canvas-browser--attach-header ()
  "The line over a dired buffer that says what a page waits for."
  (format "%s wants %s: C-c C-c sends what is marked, or the file at point; C-c C-k cancels"
          (buffer-name (plist-get canvas-browser--chooser :buffer))
          (if (plist-get canvas-browser--chooser :multiple) "files" "a file")))

(define-minor-mode canvas-browser-attach-mode
  "Pick in this dired buffer the files that a page asked for.
Mark them and press `C-c C-c\=', or press it on one file.  `C-c C-k\='
tells the page that nothing was chosen."
  :lighter " Attach"
  :keymap canvas-browser-attach-mode-map
  (if canvas-browser-attach-mode
      (setq-local header-line-format '(:eval (canvas-browser--attach-header)))
    (kill-local-variable 'header-line-format)))

(defun canvas-browser--attach-watch ()
  "Give this new dired buffer the keys of the choice that a page waits for."
  (when canvas-browser--chooser
    (canvas-browser-attach-mode 1)))

(defun canvas-browser--file-chooser-opened (params)
  "Take PARAMS of the event that says this page asks for files.
A dired buffer opens, in the directory a file was last taken from, and
the files are picked there."
  (canvas-browser--attach-finish)
  (setq canvas-browser--chooser
        (list :buffer (current-buffer)
              :node (plist-get params :backendNodeId)
              :multiple (equal (plist-get params :mode) "selectMultiple")))
  (add-hook 'dired-mode-hook #'canvas-browser--attach-watch)
  (dired-other-window (or canvas-browser--attach-directory "~/"))
  (canvas-browser-attach-mode 1))

(defun canvas-browser--attach-finish ()
  "Forget the choice of files that a page waits for, in every dired buffer."
  (setq canvas-browser--chooser nil)
  (remove-hook 'dired-mode-hook #'canvas-browser--attach-watch)
  (dolist (buffer (buffer-list))
    (when (buffer-local-value 'canvas-browser-attach-mode buffer)
      (with-current-buffer buffer (canvas-browser-attach-mode -1)))))

(defun canvas-browser--attach-files ()
  "The files chosen in this dired buffer for the page that waits.
They are the marked files, or the file at point when none is marked."
  (let ((files (dired-get-marked-files nil nil nil nil t)))
    (when-let* ((directory (seq-find #'file-directory-p files)))
      (user-error "canvas-browser: %s is a directory" (file-name-nondirectory directory)))
    (when (and (cdr files) (not (plist-get canvas-browser--chooser :multiple)))
      (user-error "canvas-browser: the page takes one file, and %d are marked"
                  (length files)))
    files))

(defun canvas-browser--file-for-chromium (file)
  "FILE, or a copy of it where the chromium in use can read it.
A snap chromium reads no hidden directory of the home and no /tmp but
its own."
  (if (and (canvas-browser-cdp-snap-p) (not (canvas-browser--snap-can-read-p file)))
      (canvas-browser--copy-for-snap file)
    file))

(defun canvas-browser--attach-leave (page)
  "Bury this dired buffer, and go back to the window of PAGE if it has one."
  (quit-window)
  (when-let* ((window (get-buffer-window page t)))
    (select-window window)))

(defun canvas-browser--attach-give (files)
  "Give FILES to the page that waits, and end the choice."
  (let ((page (plist-get canvas-browser--chooser :buffer))
        (node (plist-get canvas-browser--chooser :node)))
    (setq canvas-browser--attach-directory default-directory)
    (with-current-buffer page
      (canvas-browser--tell
       "DOM.setFileInputFiles"
       (list :files (vconcat (mapcar #'canvas-browser--file-for-chromium files))
             :backendNodeId node)))
    (canvas-browser--attach-finish)
    (canvas-browser--attach-leave page)
    (message "canvas-browser: gave the page %d file%s"
             (length files) (if (cdr files) "s" ""))))

(defun canvas-browser-attach-send ()
  "Give the page that waits the marked files, or the file at point."
  (interactive)
  (unless canvas-browser--chooser
    (user-error "canvas-browser: no page waits for a file"))
  (canvas-browser--attach-give (canvas-browser--attach-files)))

(defun canvas-browser-attach-open ()
  "Go into the directory at point, or give the file at point to the page.
This is `RET\=' while a page waits for a file.  Without it, `RET\=' on a
file opens the file in a buffer, where the keys of the choice are not."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (dired-find-file)
      (unless canvas-browser--chooser
        (user-error "canvas-browser: no page waits for a file"))
      (canvas-browser--attach-give (list file)))))

(defun canvas-browser-attach-cancel ()
  "Tell the page that waits that no file was chosen.
The field that asked gets a cancel event, as it does when a file dialog
is closed."
  (interactive)
  (unless canvas-browser--chooser
    (user-error "canvas-browser: no page waits for a file"))
  (let ((page (plist-get canvas-browser--chooser :buffer))
        (node (plist-get canvas-browser--chooser :node)))
    (with-current-buffer page
      (canvas-browser--tell
       "DOM.resolveNode" (list :backendNodeId node)
       (canvas-browser--here
        (lambda (result)
          (canvas-browser--tell
           "Runtime.callFunctionOn"
           (list :objectId (plist-get (plist-get result :object) :objectId)
                 :functionDeclaration
                 "function () { this.dispatchEvent(new Event('cancel', {bubbles: true})); }"))))))
    (canvas-browser--attach-finish)
    (canvas-browser--attach-leave page)
    (message "canvas-browser: no file for the page")))

;;;; A field of the page in a buffer

(defcustom canvas-browser-field-history-delay 1
  "How many seconds after a key the text of the field of a page is read.
The text goes into the buffer of the field as a step to undo, so that
the field has a history that Emacs can show: chromium keeps its own to
itself.  With nil no field is read, and a field has a history only of
what was written in its buffer."
  :type '(choice (number :tag "Seconds")
                 (const :tag "Keep no history of what is typed in a page" nil))
  :group 'canvas-browser)

(defconst canvas-browser--field-js
  (concat canvas-browser--takes-typing-js "
  const activeField = () => {
    let e = document.activeElement;
    while (e) {
      if (e.shadowRoot && e.shadowRoot.activeElement) { e = e.shadowRoot.activeElement; continue; }
      if (e.tagName === 'IFRAME') {
        let inner = null;
        try { inner = e.contentDocument && e.contentDocument.activeElement; }
        catch (other) { inner = null; }
        if (inner && inner !== e.contentDocument.body) { e = inner; continue; }
      }
      break;
    }
    return takesTyping(e) ? e : null;
  };
  const keptFields = () => window.__canvasBrowserFields || (window.__canvasBrowserFields =
    {key: Math.random().toString(36).slice(2), ids: new WeakMap(), fields: new Map(), last: 0});
  const fieldId = e => {
    const kept = keptFields();
    if (!kept.ids.has(e)) {
      kept.ids.set(e, kept.key + '-' + (++kept.last));
      kept.fields.set(kept.ids.get(e), new WeakRef(e));
    }
    return kept.ids.get(e);
  };
  const fieldOf = id => {
    const known = keptFields().fields.get(id);
    const e = known && known.deref();
    return e && e.isConnected ? e : null;
  };
  const fieldText = e => {
    if (typeof e.value === 'string') return e.value;
    let text = '', loose = false;
    const breakLine = () => {
      if (!text || text.endsWith('\\n')) return;
      if (loose) text = text.replace(/ $/, '');
      text += '\\n';
    };
    const visit = node => {
      if (node.nodeType === 3) {
        const kept = /^pre/.test(getComputedStyle(node.parentElement || node.parentNode.host).whiteSpace);
        let piece = kept ? node.textContent : node.textContent.replace(/\\s+/g, ' ');
        if (!kept && (!text || /[\\n ]$/.test(text))) piece = piece.replace(/^ /, '');
        if (piece) { text += piece; loose = !kept; }
        return;
      }
      if (node.nodeType !== 1) return;
      if (node.tagName === 'BR') { text += '\\n'; return; }
      const style = getComputedStyle(node);
      if (style.display === 'none') return;
      const block = !/^(inline|contents)/.test(style.display);
      if (block) breakLine();
      node.childNodes.forEach(visit);
      if (block) breakLine();
    };
    e.childNodes.forEach(visit);
    return text.replace(/\\n$/, '');
  };
  const markAll = e => {
    e.focus();
    if (typeof e.select === 'function') { e.select(); return; }
    const range = e.ownerDocument.createRange();
    range.selectNodeContents(e);
    const root = e.getRootNode();
    const selection = root.getSelection ? root.getSelection() : e.ownerDocument.getSelection();
    selection.removeAllRanges();
    selection.addRange(range);
  };")
  "The JavaScript that reads a field of a page and marks all of it.
`activeField\\=' gives the field that has the focus, also in a web
component and in a frame of the same site, or null when the focus is in
nothing that takes typing.
`fieldId\\=' gives a field a name that no other field has, in this page
or in the one that was at its address before, and `fieldOf\\=' gives the
field of a name, or null once the page has let go of it.
`fieldText\\=' gives the text of a field.  An editor that a page builds
itself has no value: its text is a line for each block it shows and for
each line break.  The innerText of the page is not that text, because
it has an empty line between two paragraphs, and each such line would
come back as a paragraph of its own.
`markAll\\=' gives the field the focus and marks all that it holds.")

(defconst canvas-browser--field-read-script
  (concat "(function () {" canvas-browser--field-js "
  const e = activeField();
  if (!e) return null;
  if (e.type === 'password') return {password: true};
  return {id: fieldId(e), text: fieldText(e)};
})()")
  "The JavaScript that gives the field with the focus: its name and its text.
The answer is null without such a field, and it says so when the field
is one for a password, without the password.")

(defconst canvas-browser--field-mark-script
  (concat "(function (id) {" canvas-browser--field-js "
  const e = fieldOf(id);
  if (!e) return null;
  const told = " canvas-browser--selection-change-js ";
  markAll(e);
  return told.then(() => true);
})(%s)")
  "The JavaScript that marks all of a field; %s is the name of the field.
It gives a promise that is kept once the page has told of the new
selection, as an editor must know of it before a text takes its place.
The answer is null when the page no longer has the field.")

(defconst canvas-browser--field-push-delay 0.2
  "How many seconds a change in the history of a field waits to be typed.
Steps that follow one another fast are typed into the field as one.")

(defvar-local canvas-browser--field-buffers nil
  "In a page buffer: the buffers of its fields, as an alist of (ID . BUFFER).")

(defvar-local canvas-browser--field-read-timer nil
  "In a page buffer: the timer that reads the field once the keys pause.")

(defvar-local canvas-browser--field-page nil
  "The page buffer that has the field whose text this buffer holds.")

(defvar-local canvas-browser--field-id nil
  "The name that the page has for the field whose text this buffer holds.")

(defvar-local canvas-browser--field-state nil
  "What a change of the text of this buffer means for its field.
With `edit\\=' the buffer is open to be written in: its text waits for
`C-c C-c\\=', and the field is not read into it.  With `live\\=' the
history of the buffer is looked at: each change is typed into the field
at once.  With nil the buffer follows the field.")

(defvar-local canvas-browser--field-push-timer nil
  "The timer that types the text of this buffer into its field.")

(defvar canvas-browser--field-reading nil
  "Whether the text of a field is being put in its buffer.
That change comes from the page, and is not typed back into it.")

(defvar canvas-browser-edit-field-mode-map (make-sparse-keymap)
  "Keymap of the buffer that holds the text of a field of a page.")

;; The keys are bound here and not where the map is made, so that they
;; reach a running Emacs when the file is loaded again.
(keymap-set canvas-browser-edit-field-mode-map "C-c C-c" #'canvas-browser-edit-field-finish)
(keymap-set canvas-browser-edit-field-mode-map "C-c C-k" #'canvas-browser-edit-field-abort)

(define-minor-mode canvas-browser-edit-field-mode
  "The buffer holds the text of a field of a page of canvas-browser.
`C-c C-c\\=' types the text into the field, in place of what the field
holds, and sends nothing.  `C-c C-k\\=' leaves the field as it is."
  :lighter " Field"
  :keymap canvas-browser-edit-field-mode-map
  (if canvas-browser-edit-field-mode
      (setq-local header-line-format
                  (substitute-command-keys
                   (concat "\\<canvas-browser-edit-field-mode-map>"
                           "The text of a field: \\[canvas-browser-edit-field-finish]"
                           " puts it in the field, \\[canvas-browser-edit-field-abort]"
                           " leaves the field as it is")))
    (kill-local-variable 'header-line-format)))

(defun canvas-browser--field-buffer-of (page id)
  "The buffer of the field that PAGE names ID, or nil when it has none."
  (let ((buffer (cdr (assoc id (buffer-local-value 'canvas-browser--field-buffers page)))))
    (and (buffer-live-p buffer) buffer)))

(defun canvas-browser--new-field-buffer (page id text)
  "A new buffer for the field that PAGE names ID, which holds TEXT."
  (let ((buffer (generate-new-buffer (format "*field of %s*" (buffer-name page)))))
    (with-current-buffer buffer
      (text-mode)
      (insert text)
      ;; The text of the field is where its history starts, not a change
      ;; that can be taken back.
      (setq buffer-undo-list nil)
      (canvas-browser-edit-field-mode 1)
      (setq canvas-browser--field-page page
            canvas-browser--field-id id)
      (add-hook 'after-change-functions #'canvas-browser--field-changed nil t)
      (add-hook 'window-buffer-change-functions #'canvas-browser--field-shown nil t))
    (with-current-buffer page
      (setf (alist-get id canvas-browser--field-buffers nil nil #'equal) buffer))
    buffer))

(defun canvas-browser--field-follow (text)
  "Make TEXT, which the field holds now, the text of this buffer.
Only what differs is changed, and the change is one step to undo.  A
buffer that is open to be written in keeps its text."
  (unless (or (eq canvas-browser--field-state 'edit)
              (equal text (buffer-substring-no-properties (point-min) (point-max))))
    (let ((canvas-browser--field-reading t))
      (undo-boundary)
      (replace-region-contents (point-min) (point-max) text 0.5)
      (undo-boundary))))

(defun canvas-browser--field-buffer (page field)
  "The buffer of FIELD of PAGE, with the text that the field holds now.
FIELD is what the page answered about a field: a plist of its name and
its text.  A field that has no buffer yet gets one."
  (let* ((id (plist-get field :id))
         (text (plist-get field :text))
         (buffer (canvas-browser--field-buffer-of page id)))
    (if (null buffer)
        (canvas-browser--new-field-buffer page id text)
      (with-current-buffer buffer (canvas-browser--field-follow text))
      buffer)))

(defun canvas-browser--field-send (then)
  "Type the text of this buffer into its field, in place of what it holds.
THEN is called in the page buffer with whether the page still has the
field.  The text is typed without a key, as a yank is: the page is told
of no Enter, so a chat does not send what it gets."
  (let ((text (buffer-substring-no-properties (point-min) (point-max)))
        (id canvas-browser--field-id))
    (with-current-buffer canvas-browser--field-page
      (canvas-browser--ask-page
       (format canvas-browser--field-mark-script (json-encode id))
       (lambda (marked)
         (when marked
           (canvas-browser--tell "Input.insertText" (list :text text)))
         (funcall then (and marked t)))))))

(defun canvas-browser--field-push (buffer)
  "Type the text of BUFFER into its field, while both are still there."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq canvas-browser--field-push-timer nil)
      (when (buffer-live-p canvas-browser--field-page)
        (canvas-browser--field-send #'ignore)))))

(defun canvas-browser--field-changed (&rest _)
  "Have the text of this buffer typed into its field soon.
This is for a buffer whose history is looked at: a step that is taken
there shows in the page.  A change that came from the page is left
out."
  (when (and (eq canvas-browser--field-state 'live)
             (not canvas-browser--field-reading))
    (when canvas-browser--field-push-timer
      (cancel-timer canvas-browser--field-push-timer))
    (setq canvas-browser--field-push-timer
          (run-with-timer canvas-browser--field-push-delay nil
                          #'canvas-browser--field-push (current-buffer)))))

(defun canvas-browser--field-hide (window buffer)
  "Show the page of BUFFER in WINDOW, in place of BUFFER, the buffer of a field.
The buffer follows its field again."
  (when (and (window-live-p window) (buffer-live-p buffer)
             (eq (window-buffer window) buffer))
    (let ((page (buffer-local-value 'canvas-browser--field-page buffer)))
      (with-current-buffer buffer
        (setq canvas-browser--field-state nil))
      (when (buffer-live-p page)
        (set-window-buffer window page)
        (unrecord-window-buffer window buffer)))))

(defun canvas-browser--field-shown (window)
  "Have the page take the place of the buffer of its field in WINDOW.
This is for a buffer whose history was asked for in the page, where it
stands for the field.  A view of the undo history goes back to the
buffer whose history it showed, as the visualizer of undo-tree does,
and shows that buffer when no window has it.  The reader came from the
page, so the page is what the window shows again."
  (let ((buffer (window-buffer window)))
    (when (eq (buffer-local-value 'canvas-browser--field-state buffer) 'live)
      ;; The windows are told of a change while they change, which is no
      ;; time to change them again.
      (run-with-timer 0 nil #'canvas-browser--field-hide window buffer))))

(defun canvas-browser--edit-field-open (field)
  "Open FIELD in its buffer: what this page answered about its field."
  (cond ((null field)
         (message "canvas-browser: no field of the page has the focus"))
        ((plist-get field :password)
         (message "canvas-browser: a password is not shown in a buffer"))
        (t (let ((buffer (canvas-browser--field-buffer (current-buffer) field)))
             (with-current-buffer buffer
               (setq canvas-browser--field-state 'edit))
             (pop-to-buffer buffer)))))

(defun canvas-browser-edit-field ()
  "Edit the text of the field with the focus in a buffer of its own.
The buffer is an ordinary buffer for text, with all that Emacs has for
one.  There `C-c C-c\\=' types the text into the field, in place of what
the field holds, and `C-c C-k\\=' leaves the field as it is.  Nothing is
sent either way.  What the field holds that is no text, as a picture,
does not come back.

The buffer stays when the edit ends, and it is the buffer of that field
from then on: its undo history is the history of the field."
  (interactive)
  (canvas-browser--ask-page canvas-browser--field-read-script
                            #'canvas-browser--edit-field-open))

(defun canvas-browser--edit-field-leave (buffer)
  "Put BUFFER out of sight, which was open to write the text of a field in.
It follows its field again."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq canvas-browser--field-state nil))
    (when-let* ((window (get-buffer-window buffer t)))
      (quit-window nil window))))

(defun canvas-browser-edit-field-finish ()
  "Type the text of this buffer into its field, in place of what it holds.
The text goes in as a yank does, without a key, so nothing is sent."
  (interactive)
  (unless (buffer-live-p canvas-browser--field-page)
    (user-error "canvas-browser: the page of this field is gone"))
  (let ((buffer (current-buffer)))
    (canvas-browser--field-send
     (lambda (marked)
       (if marked
           (canvas-browser--edit-field-leave buffer)
         (message "canvas-browser: the page no longer has this field; its text stays in the buffer"))))))

(defun canvas-browser-edit-field-abort ()
  "Put this buffer out of sight, and leave the field of the page as it is.
What was written here stays in the undo history of the buffer."
  (interactive)
  (canvas-browser--edit-field-leave (current-buffer)))

(defun canvas-browser--history-keys ()
  "The keys that ask for the history of a field: those just pressed, or C-x u.
A command that was not called by a key of its own has no keys to hand
on."
  (let ((keys (this-command-keys-vector)))
    (if (eq (key-binding keys) #'canvas-browser-field-history)
        keys
      (kbd "C-x u"))))

(defun canvas-browser--field-history-of (field keys)
  "Run in the buffer of FIELD what KEYS run there, or undo in the page.
FIELD is what this page answered about its field.  Plain undo on KEYS
means that nothing shows the history of a buffer there.  The page then
takes a step back itself, which it also does when it has no field: it
knows each key that was typed."
  (let* ((buffer (and (plist-get field :id)
                      (canvas-browser--field-buffer (current-buffer) field)))
         (command (and buffer (with-current-buffer buffer (key-binding keys)))))
    (if (or (not (commandp command)) (eq command 'undo))
        (canvas-browser-field-undo)
      (with-current-buffer buffer
        (setq canvas-browser--field-state 'live)
        (call-interactively command)))))

(defun canvas-browser-field-history ()
  "Do with the history of the field what this key does in a buffer for text.
The key is `C-x u\\='.  A package that shows the undo history of a
buffer on that key then shows the history of the field: the steps of
its buffer, which are what was written there and what the field held at
each pause in typing.  A step that is taken in that view is typed into
the field.  Without such a package the key is undo, and the page takes
back a step, as on `C-/\\='."
  (interactive)
  (let ((keys (canvas-browser--history-keys)))
    (canvas-browser--ask-page
     canvas-browser--field-read-script
     (lambda (field) (canvas-browser--field-history-of field keys)))))

(defun canvas-browser--field-read (page)
  "Put what the field of PAGE holds now in the buffer of the field."
  (when (buffer-live-p page)
    (with-current-buffer page
      (setq canvas-browser--field-read-timer nil)
      (canvas-browser--ask-page
       canvas-browser--field-read-script
       (lambda (field)
         (when (plist-get field :id)
           (canvas-browser--field-buffer (current-buffer) field)))))))

(defun canvas-browser--field-read-later ()
  "Read the field of this page once the keys pause, while they go to the page.
Each pause is then a step in the history of the field."
  (when canvas-browser--field-read-timer
    (cancel-timer canvas-browser--field-read-timer)
    (setq canvas-browser--field-read-timer nil))
  (when (and canvas-browser--insert canvas-browser-field-history-delay)
    (setq canvas-browser--field-read-timer
          (run-with-idle-timer canvas-browser-field-history-delay nil
                               #'canvas-browser--field-read (current-buffer)))))

(defun canvas-browser--forget-fields ()
  "Kill the buffers of the fields of this page, but one that is written in.
The page that had the fields is gone.  A buffer that is open to be
written in holds text that is nowhere else."
  (when canvas-browser--field-read-timer
    (cancel-timer canvas-browser--field-read-timer)
    (setq canvas-browser--field-read-timer nil))
  (dolist (entry canvas-browser--field-buffers)
    (when (and (buffer-live-p (cdr entry))
               (not (eq (buffer-local-value 'canvas-browser--field-state (cdr entry)) 'edit)))
      (kill-buffer (cdr entry))))
  (setq canvas-browser--field-buffers nil))

;;;; Sending the page to a receiver

(defcustom canvas-browser-receivers
  '(("buffer" . canvas-browser-send-to-buffer)
    ("org capture" . canvas-browser-send-to-org-capture)
    ("org-roam node" . canvas-browser-send-to-org-roam))
  "Where `canvas-browser-send-to\=' can send a page, each (NAME . FUNCTION).
FUNCTION is called in the page buffer with one argument, a plist.  Its
`:kind' is `text' for the text that the caret marks, `page' for the
text of the whole page, and `part' for the text of the part of it that
is picked.  The text is all that the page shows there, and nothing is
read into it on the way: what is made of it is the business of
FUNCTION, which gets the HTML from `canvas-browser-content-html\=' when
it wants what the text has lost.  Only a translator of
`canvas-browser-translators\=' comes between: when the address of the
page has one, the text is what it made of the page, and `:thread' is
the thread as Lisp data, when the translator gave one.  Its `:text' is
that text, and its `:url' and `:title' are the address and the title of
the page.  Another package adds a receiver of its own with
`add-to-list\='."
  :type '(alist :key-type string :value-type function)
  :group 'canvas-browser)

(defun canvas-browser--marked-text ()
  "The text that the caret of this page marks, or nil when it marks none."
  (and canvas-browser--caret
       (stringp canvas-browser--caret-text)
       (not (string-empty-p canvas-browser--caret-text))
       canvas-browser--caret-text))

(defun canvas-browser--answer-now (method params &optional refused)
  "The answer of chromium to METHOD with PARAMS in this page.
It is nil for a command that chromium refused; REFUSED, when given, is
called with what chromium says, and else the reader is told.  Emacs
waits here for the answer, so that the command that asks has it before
it goes on, and its errors reach the user."
  (let ((result 'waiting)
        (deadline (+ (float-time) canvas-browser-cdp-timeout)))
    (canvas-browser-cdp-send method params (lambda (answer) (setq result answer))
                             canvas-browser--session refused)
    (while (and (eq result 'waiting) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (when (eq result 'waiting)
      (error "canvas-browser: the page did not answer in %s seconds"
             canvas-browser-cdp-timeout))
    result))

(defun canvas-browser--ask-page (script then)
  "Run SCRIPT in the page, and call THEN in this buffer with its value.
When the value is a promise, THEN gets what the promise is kept with.
The null of JSON is nil."
  (canvas-browser--tell
   "Runtime.evaluate"
   (list :expression script :returnByValue t :awaitPromise t)
   (canvas-browser--here
    (lambda (result) (funcall then (canvas-browser--value-of result))))))

(defun canvas-browser--value-now (script &optional await)
  "The value that SCRIPT gives in this page; nil for the null of JSON.
With AWAIT, the value of the promise that SCRIPT gives.  See
`canvas-browser--answer-now\='."
  (canvas-browser--value-of
   (canvas-browser--answer-now
    "Runtime.evaluate"
    (append (list :expression script :returnByValue t) (and await (list :awaitPromise t))))))

(defun canvas-browser--main-frame ()
  "The id of the frame of this page itself, or nil."
  (plist-get (plist-get (plist-get (canvas-browser--answer-now "Page.getFrameTree" nil)
                                   :frameTree)
                        :frame)
             :id))

(defun canvas-browser--loaded-bytes (url)
  "The bytes of URL as this page loaded it, or nil when it has not.
Chromium keeps what a page loaded, so nothing is fetched again, and a
picture that needs a sign-in comes all the same."
  (when-let* ((frame (canvas-browser--main-frame))
              (answer (canvas-browser--answer-now
                       "Page.getResourceContent" (list :frameId frame :url url) #'ignore))
              (content (plist-get answer :content)))
    (if (eq (plist-get answer :base64Encoded) t)
        (base64-decode-string content)
      (encode-coding-string content 'utf-8))))

(defconst canvas-browser--fetch-script
  "fetch(%s).then(r => r.blob()).then(b => new Promise(done => {
     const reader = new FileReader();
     reader.onload = () => done(reader.result);
     reader.readAsDataURL(b);
   }))"
  "The JavaScript that fetches an address in the page, as a data URL.")

(defun canvas-browser--fetched-bytes (url)
  "The bytes of URL as the page fetches it, or nil when it cannot."
  (when-let* ((data (ignore-errors
                      (canvas-browser--value-now
                       (format canvas-browser--fetch-script (json-encode url)) t)))
              ((string-match "\\`data:[^,]*;base64,\\(.*\\)\\'" data)))
    (base64-decode-string (match-string 1 data))))

(defun canvas-browser-picture-bytes (url)
  "The bytes of the picture at URL in this page, or nil when it has none.
It is what the page loaded, or else what the page fetches now: an
address that a page makes for a picture of its own works only there."
  (or (canvas-browser--loaded-bytes url) (canvas-browser--fetched-bytes url)))

(defun canvas-browser--page-text ()
  "The text of this page."
  (or (canvas-browser--value-now canvas-browser--text-script) ""))

(defun canvas-browser--part-script (function)
  "The JavaScript that gives what FUNCTION gives of the part that is picked.
FUNCTION is JavaScript, a function of one element.  The script gives
null for a part that the page has thrown away."
  (concat "(part => part && part.isConnected ? " function "(part) : null)"
          (canvas-browser--part-js)))

(defun canvas-browser--part-text ()
  "The text of the part of this page that is picked.
A part that the page has thrown away is forgotten, with an error."
  (or (canvas-browser--value-now (canvas-browser--part-script canvas-browser--text-js))
      (progn (canvas-browser--forget-part)
             (user-error "canvas-browser: that part has gone; pick one again with `S'"))))

(defconst canvas-browser--selection-html-script
  "(() => {
     const selection = getSelection();
     if (!selection.rangeCount) return null;
     const box = document.createElement('div');
     box.append(selection.getRangeAt(0).cloneContents());
     return box.innerHTML || null;
   })()"
  "The JavaScript that gives the HTML that the selection of a page covers.
It gives null when the selection covers no HTML, as in a field.")

(defun canvas-browser-content-html (content)
  "The HTML of CONTENT, which a receiver was given, as the page has it.
It is nil when the page has no HTML for it, as for text marked in a
field.  A receiver calls this, in the page buffer that it runs in, when
it wants more than the text: the HTML still holds what lies inside
what, which the text does not.  Nothing is taken out of it or read into
it here."
  (canvas-browser--value-now
   (pcase-exhaustive (plist-get content :kind)
     ('page (concat canvas-browser--html-js "(document.body)"))
     ('part (canvas-browser--part-script canvas-browser--html-js))
     ('text canvas-browser--selection-html-script))))

(defun canvas-browser--part-box ()
  "The box of the part that is picked, as far as the window shows it.
It is nil for a part that has gone, or that is out of the window."
  (let ((box (canvas-browser--value-now
              (concat "(part => {" canvas-browser--reach-js
                      " return part && part.isConnected ? shown(part) : null; })"
                      (canvas-browser--part-js)))))
    (and box (> (plist-get box :w) 0) (> (plist-get box :h) 0) box)))

(defun canvas-browser--translation (kind)
  "What the translator of this page makes of KIND of it, or nil.
It is a plist of `canvas-browser-translate\='.
It is nil when the address has no translator, and when the translator
finds nothing to translate, which a message says.  The page is asked
for its HTML only when its address has a translator."
  (when-let* ((entry (and canvas-browser--url
                          (canvas-browser-translator-for canvas-browser--url))))
    (or (when-let* ((html (canvas-browser-content-html (list :kind kind))))
          (canvas-browser-translate canvas-browser--url html))
        ;; A snippet that does not fit the page would go unseen else.
        (progn (message "canvas-browser: the translator for %s found nothing here"
                        (car entry))
               nil))))

(defun canvas-browser--reading (kind plain)
  "KIND of this page for a reader, as a plist with :text, and :thread.
It is the translation, when the address of the page has a translator
that gives one.  Else :text is what PLAIN, a function, gives: the text
as the page shows it, and there is no :thread."
  (or (canvas-browser--translation kind) (list :text (funcall plain))))

(defun canvas-browser--text-to-read (kind plain)
  "The text of KIND of this page, for a reader; see `canvas-browser--reading\='."
  (plist-get (canvas-browser--reading kind plain) :text))

(defun canvas-browser--content (kind reading)
  "What a receiver gets of this page: READING of KIND, its address and title.
KIND is `text' for the text that the caret marks, `page' for the text
of the whole page, and `part' for that of one part of it.  READING is
a plist of `canvas-browser--reading\='.  A page without a title is
named by its address."
  (append (list :kind kind :text (plist-get reading :text))
          (and (plist-get reading :thread) (list :thread (plist-get reading :thread)))
          (list :url canvas-browser--url
                :title (or canvas-browser--title canvas-browser--url))))

(defun canvas-browser--read-receiver ()
  "Ask which receiver gets the page, and return its function."
  (unless canvas-browser-receivers
    (user-error "canvas-browser: no receiver in `canvas-browser-receivers'"))
  (let ((name (completing-read "Send to: " canvas-browser-receivers nil t)))
    (or (cdr (assoc name canvas-browser-receivers))
        (user-error "canvas-browser: no receiver is named %S" name))))

(defun canvas-browser--send (kind text)
  "Ask for a receiver, and give it the content of KIND of this page.
TEXT is a function that gives the text.  The receiver is asked for
first, so that a page is not read for nothing."
  (let ((receiver (canvas-browser--read-receiver)))
    (funcall receiver (canvas-browser--content kind (canvas-browser--reading kind text)))))

(defun canvas-browser-send-to ()
  "Send the text that the caret marks to a receiver.
With nothing marked, send the part of the page that is picked, and with
no part picked, the whole page.  `S\=' picks a part.  The receivers are
those of `canvas-browser-receivers\='."
  (interactive)
  (cond ((canvas-browser--marked-text)
         (canvas-browser--send 'text #'canvas-browser--marked-text))
        (canvas-browser--part (canvas-browser-send-part-to))
        (t (canvas-browser-send-page-to))))

(defun canvas-browser-send-page-to ()
  "Send the text of the whole page to a receiver.
It does so whatever the caret marks, and whatever part is picked."
  (interactive)
  (canvas-browser--send 'page #'canvas-browser--page-text))

(defconst canvas-browser--tint-colour #x40FFD633
  "The colour laid over the part of a page that is sent, as ARGB32.
It is the colour of a hint, and the page shows through it.")

(defun canvas-browser--draw-tint (box)
  "Lay the colour of a hint over BOX on the canvas, and show it."
  (canvas-browser--set-colour canvas-browser--context canvas-browser--tint-colour)
  (canvas-browser--box-path box)
  (canvas-cairo-fill canvas-browser--context)
  (canvas-refresh canvas-browser--canvas)
  (force-window-update (current-buffer)))

(defun canvas-browser--read-receiver-over (box)
  "Ask which receiver gets the part of the page in BOX, with BOX tinted.
The tint shows what goes to the receiver.  A new picture of the page
takes it off again."
  (setq canvas-browser--hinting t)
  (unwind-protect
      (progn (canvas-browser--draw-tint box)
             (canvas-browser--read-receiver))
    (canvas-browser--take-marks-off)))

(defun canvas-browser-send-part-to ()
  "Send the text of the part of the page that is picked to a receiver.
`S\=' picks a part.  The part is tinted while the receiver is asked
for, when the window shows it.  All of its text is sent, also what is
out of the window."
  (interactive)
  (unless canvas-browser--part
    (user-error "canvas-browser: no part of the page is picked; `S' picks one"))
  (let* ((box (canvas-browser--part-box))
         (receiver (if box
                       (canvas-browser--read-receiver-over box)
                     (canvas-browser--read-receiver))))
    (funcall receiver (canvas-browser--content
                       'part (canvas-browser--reading 'part #'canvas-browser--part-text)))))

(defun canvas-browser--paste (text)
  "Paste TEXT into this buffer with the command it has for `yank\='.
The buffer of a terminal is read-only and takes text through its
process, which its own yank command does.  The kill ring stays as it
is."
  (let* ((kill-ring (list text))
         (kill-ring-yank-pointer kill-ring)
         (interprogram-paste-function nil))
    (call-interactively (or (command-remapping 'yank) #'yank))))

(defun canvas-browser-send-to-buffer (content)
  "Paste the text of CONTENT into a buffer that is asked for.
The text goes where the cursor of that buffer shows: at the point of
the window that shows it, or at its own point when no window does."
  (let* ((buffer (get-buffer (read-buffer "Paste into buffer: "
                                          (other-buffer (current-buffer) t) t)))
         (window (get-buffer-window buffer t)))
    (if window
        (with-selected-window window (canvas-browser--paste (plist-get content :text)))
      (with-current-buffer buffer (canvas-browser--paste (plist-get content :text))))))

(declare-function org-link-store-props "ol" (&rest plist))
(declare-function org-link-make-string "ol" (link &optional description))
(declare-function org-capture "org-capture" (&optional goto keys))
(declare-function org-roam-capture- "org-roam-capture")
(defvar org-capture-link-is-already-stored)

(defun canvas-browser--store-org-link (content text)
  "Tell org of the page of CONTENT, as the link and TEXT of a capture.
A template then has the link in %a, the address in %:link, the title in
%:description and TEXT in %i."
  (let ((url (plist-get content :url))
        (title (plist-get content :title)))
    (org-link-store-props :type (url-type (url-generic-parse-url url))
                          :link url
                          :description title
                          :annotation (org-link-make-string url title)
                          :initial text)))

(defun canvas-browser--org-text (content &optional quote)
  "The text of CONTENT for a note of org.
A thread that a translator gave is an org list.  Other text is as it
is, or in a quote block with QUOTE."
  (cond ((plist-get content :thread) (canvas-browser-thread-org (plist-get content :thread)))
        (quote (format "#+begin_quote\n%s\n#+end_quote" (plist-get content :text)))
        (t (plist-get content :text))))

(defun canvas-browser-send-to-org-capture (content)
  "Capture CONTENT with a template of `org-capture\=', which is asked for."
  (require 'org-capture)
  (canvas-browser--store-org-link content (canvas-browser--org-text content))
  ;; Without this, org-capture stores a link to the page buffer, and
  ;; that one replaces the link to the page.
  (let ((org-capture-link-is-already-stored t))
    (org-capture)))

(defcustom canvas-browser-org-roam-templates
  '(("p" "page" plain "%i\n%?"
     :target (file+head "${slug}.org" "#+title: ${title}")
     :unnarrowed t))
  "The templates of the org-roam receiver, as org-roam writes its templates.
See `org-roam-capture-templates\=' for what one holds.  %i stands for
what is sent, with a heading of its own that links to the page: a
thread that a translator gave as an org list, and other text as a
quote.  %a stands for a link to the page, named by the title of the
page, and ${title} for the title of the node.

The text is not ${body}, as org-roam has it for a page sent from a
browser.  For ${NAME} org-roam first calls a function called NAME, when
there is one, and a package that is loaded may have a function `body\=':
the web-server package has."
  :type '(repeat sexp)
  :group 'canvas-browser)

(declare-function org-roam-node-read "org-roam-node"
                  (&optional initial-input filter-fn sort-fn require-match prompt))

(defun canvas-browser--picture-name (bytes)
  "A name for the file of a picture of BYTES: the same bytes, the same name."
  (format "picture-%s.%s" (substring (md5 bytes) 0 10)
          (cond ((string-prefix-p "\x89PNG" bytes) "png")
                ((string-prefix-p "\xff\xd8" bytes) "jpg")
                ((string-prefix-p "GIF8" bytes) "gif")
                ((and (string-prefix-p "RIFF" bytes) (string-search "WEBP" (substring bytes 0 16))) "webp")
                ((string-match-p "\\`[ \t\n]*<\\(?:\\?xml\\|svg\\)" bytes) "svg")
                (t "bin"))))

(defun canvas-browser--attach-picture (address directory)
  "Put the picture at ADDRESS of this page in DIRECTORY; the name of its file.
It is nil when the page cannot give the picture."
  (when-let* ((bytes (canvas-browser-picture-bytes
                      (url-expand-file-name address canvas-browser--url))))
    (let ((name (canvas-browser--picture-name bytes)))
      (make-directory directory t)
      (let ((coding-system-for-write 'binary))
        (with-temp-file (expand-file-name name directory)
          (set-buffer-multibyte nil)
          (insert bytes)))
      name)))

(defun canvas-browser--attach-pictures-in (text directory)
  "TEXT with each picture in it put in DIRECTORY and linked as an attachment.
A picture that the page cannot give stays as its name in brackets."
  (let ((pieces nil) (from 0))
    (pcase-dolist (`(,start ,end ,address) (canvas-browser-text-pictures text))
      (push (substring text from start) pieces)
      (push (if-let* ((name (canvas-browser--attach-picture address directory)))
                (format "[[attachment:%s]]" name)
              (substring text start end))
            pieces)
      (setq from end))
    (apply #'concat (nreverse (cons (substring text from) pieces)))))

(declare-function org-attach-dir-from-id "org-attach" (id &optional existing))
(declare-function org-roam-node-id "org-roam-node" (node))
(declare-function org-roam-node-title "org-roam-node" (node))
(declare-function org-roam-node-create "org-roam-node")
(declare-function org-id-new "org-id" (&optional prefix))
(declare-function org-roam-node-file "org-roam-node" (node))
(defvar org-roam-directory)

(defun canvas-browser--node-with-id (node)
  "NODE, an org-roam node, with an id: a new node is given one here.
org-roam keeps that id when it makes the note."
  (if (org-roam-node-id node)
      node
    (org-roam-node-create :title (org-roam-node-title node) :id (org-id-new))))

(defun canvas-browser--node-directory (node)
  "The directory of the note of NODE, which a new node gets from org-roam."
  (file-name-as-directory (if (org-roam-node-file node)
                              (file-name-directory (org-roam-node-file node))
                            org-roam-directory)))

(defun canvas-browser--node-attach-dir (node)
  "The directory that org-attach keeps the attachments of NODE in.
NODE has an id."
  (let ((default-directory (canvas-browser--node-directory node)))
    (org-attach-dir-from-id (org-roam-node-id node))))

(defun canvas-browser--attach-property (node)
  "The directory of the attachments of NODE, as a heading of its note names it.
It is nil while NODE has no attachment.  org-attach finds the
attachments of a note by the id of the note, but it does not look above
a heading for that id.  So a heading that holds links to attachments
names their directory itself, in its property DIR."
  (let ((directory (canvas-browser--node-attach-dir node)))
    (and (file-directory-p directory)
         (file-relative-name directory (canvas-browser--node-directory node)))))

(defun canvas-browser--org-section (content)
  "What a send of CONTENT writes into a note: a heading of its own.
The heading links to the page.  Under it comes a thread as an org list,
or other text as a quote.

The heading does not name the directory of the attachments here.  org
fills a capture template in a buffer without a file, and org-attach
takes no relative directory in such a buffer: where org shows the
pictures of a note at startup, the capture stopped with an error.
`canvas-browser--name-attachments\=' names the directory when the text
is in the note."
  (concat "* From "
          (org-link-make-string (plist-get content :url) (plist-get content :title))
          "\n\n"
          (canvas-browser--org-text content t)))

(defun canvas-browser--with-attached-pictures (content node)
  "CONTENT with the pictures of its thread attached to NODE, an org-roam node.
The text of each item links to its pictures as attachments.  Content
without a thread stays as it is."
  (if-let* ((thread (plist-get content :thread)))
      (let ((directory (canvas-browser--node-attach-dir node)))
        (plist-put (copy-sequence content) :thread
                   (canvas-browser-thread-map-text
                    (lambda (text) (canvas-browser--attach-pictures-in text directory))
                    thread)))
    content))

(declare-function org-roam-db-update-file "org-roam-db" (&optional file-path no-require))

(declare-function org-before-first-heading-p "org" ())
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-entry-get "org" (epom property &optional inherit literal-nil))
(declare-function org-entry-put "org" (epom property value))

(defun canvas-browser--name-attachments (thread node)
  "Have the heading that holds THREAD in this note name the attachments of NODE.
See `canvas-browser--attach-property\='.  A thread above the first
heading needs nothing, and a heading that names a directory keeps it."
  (when-let* ((directory (canvas-browser--attach-property node))
              (start (canvas-browser-org-thread-start thread)))
    (save-excursion
      (goto-char start)
      (unless (org-before-first-heading-p)
        (org-back-to-heading t)
        (unless (org-entry-get nil "DIR")
          (org-entry-put nil "DIR" directory))))))

(defun canvas-browser--merge-into-node (node content)
  "Merge the thread of CONTENT into the note of NODE, if it holds the thread.
Return non-nil when it did; a message says what was added.  A node
without a note, content without a thread, and a note that holds none of
its messages are left to the capture."
  (when-let* ((thread (plist-get content :thread))
              (file (org-roam-node-file node)))
    (with-current-buffer (find-file-noselect file)
      (when-let* ((counts (canvas-browser-org-merge thread)))
        (canvas-browser--name-attachments thread node)
        (save-buffer)
        (org-roam-db-update-file file)
        (message "canvas-browser: %s: %s"
                 (org-roam-node-title node)
                 (if (equal counts '(0 . 0))
                     "nothing new"
                   (format "%d new, %d edited" (car counts) (cdr counts))))
        t))))

(defvar org-capture-mode-hook)

(defun canvas-browser--capture-in-node (node content)
  "Capture CONTENT in the note of NODE, under a heading of its own.
The heading names the attachments of NODE as soon as org has put the
text in the note: see `canvas-browser--org-section\='."
  (canvas-browser--store-org-link content (canvas-browser--org-section content))
  (let ((org-capture-link-is-already-stored t)
        (org-capture-mode-hook
         (cons (lambda ()
                 (canvas-browser--name-attachments (plist-get content :thread) node))
               org-capture-mode-hook)))
    (org-roam-capture- :node node :templates canvas-browser-org-roam-templates)))

(defun canvas-browser-send-to-org-roam (content)
  "Capture CONTENT in an org-roam node, which is asked for.
The node is one that exists, or a new one with the title that you type.
What is sent gets a heading of its own in the note, which links to
the page: a thread as an org list, and other text as a quote.  The
templates are those of `canvas-browser-org-roam-templates\='.

The node is not found by the address of the page, and not named by its
title.  Many a page has one address and one title for all that it
shows, so every note from it would land in one node with a title that
says nothing.

The pictures of a thread are fetched from the page and kept among the
attachments of the node, as org-attach keeps them, and the note links
to them where they stood.

A node that holds the thread already is not given it a second time:
the new messages are merged into it, as `canvas-browser-org-merge\='
does."
  (require 'org-roam)
  (require 'org-attach)
  (require 'org-id)
  (let* ((node (canvas-browser--node-with-id
                (org-roam-node-read nil nil nil nil "Capture in node: ")))
         (content (canvas-browser--with-attached-pictures content node)))
    (unless (canvas-browser--merge-into-node node content)
      (canvas-browser--capture-in-node node content))))

;;;; The targets of embark

(defun canvas-browser-embark-target ()
  "The targets of embark in a page buffer.
The text that the caret marks comes first, then the part of the page
that is picked, and then the page.  The last two are named by the
address.  Embark acts on the first, and cycles to the others."
  (when (derived-mode-p 'canvas-browser-mode)
    (append (when-let* ((text (canvas-browser--marked-text)))
              (list (cons 'canvas-browser-text text)))
            (when (and canvas-browser--url canvas-browser--part)
              (list (cons 'canvas-browser-part canvas-browser--url)))
            (when canvas-browser--url
              (list (cons 'canvas-browser-page canvas-browser--url))))))

(defvar canvas-browser-embark-text-map (make-sparse-keymap)
  "The actions of embark on the text that the caret of a page marks.
Its parent holds the actions that embark has for any target.")

;; Bound here and not where the map is made, so that a second load of
;; this file brings a new key to a running Emacs.
(keymap-set canvas-browser-embark-text-map "s" #'canvas-browser)
(keymap-set canvas-browser-embark-text-map ">" #'canvas-browser-send-to)

(defvar canvas-browser-embark-page-map (make-sparse-keymap)
  "The actions of embark on a page, beside those embark has for any URL.")

(keymap-set canvas-browser-embark-page-map ">" #'canvas-browser-send-page-to)

(defvar canvas-browser-embark-part-map (make-sparse-keymap)
  "The actions of embark on the part of a page that is picked.
Those embark has for any URL come beside them, as for the page.")

(keymap-set canvas-browser-embark-part-map ">" #'canvas-browser-send-part-to)

(defvar embark-target-finders)
(defvar embark-keymap-alist)
(defvar embark-general-map)
(defvar embark-target-injection-hooks)
(defvar embark-transformer-alist)

(defun canvas-browser--embark-link (_type candidate)
  "CANDIDATE, a link as `canvas-browser-links\=' offers it, as a URL for embark.
The address is the last word of what is offered."
  (cons 'url (car (last (split-string candidate " " t)))))

(defun canvas-browser--embark-here ()
  "Have embark find its targets in this page buffer with one finder alone.
That is `canvas-browser-embark-target\='.  The buffer holds one character,
which shows the picture of the page.  A finder that reads text takes
that character as its target, and embark shows the picture in its
prompt when it cycles to it."
  (setq-local embark-target-finders (list #'canvas-browser-embark-target)))

(defun canvas-browser--embark-setup ()
  "Tell embark of the targets of a page buffer, also of those open already.
`s\=' on marked text opens a page buffer that searches for it, and `>\='
sends the target to a receiver.  A page has the actions of a URL too."
  (set-keymap-parent canvas-browser-embark-text-map embark-general-map)
  (setf (alist-get 'canvas-browser-text embark-keymap-alist)
        '(canvas-browser-embark-text-map))
  (setf (alist-get 'canvas-browser-page embark-keymap-alist)
        '(canvas-browser-embark-page-map embark-url-map))
  (setf (alist-get 'canvas-browser-part embark-keymap-alist)
        '(canvas-browser-embark-part-map embark-url-map))
  (setf (alist-get 'canvas-browser-link embark-transformer-alist)
        #'canvas-browser--embark-link)
  ;; Embark answers the first question of an action with the target and
  ;; presses RET.  These actions ask which receiver, not for the target.
  (dolist (command '(canvas-browser-send-to canvas-browser-send-page-to
                     canvas-browser-send-part-to))
    (setf (alist-get command embark-target-injection-hooks) '(embark--ignore-target)))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'canvas-browser-mode)
        (canvas-browser--embark-here)))))

(with-eval-after-load 'embark
  (canvas-browser--embark-setup))

;;;; Bookmarks
;;
;; A page is an Emacs bookmark, so `bookmark-set' keeps it and
;; `bookmark-jump', `consult-bookmark' and the bookmark list open it.

(defvar consult-bookmark-narrow)
(declare-function bookmark-prop-get "bookmark" (bookmark prop))
(declare-function bookmark-get-handler "bookmark" (bookmark))
(declare-function bookmark-jump "bookmark" (bookmark &optional display-func))
(declare-function bookmark-maybe-load-default-file "bookmark")
(defvar bookmark-alist)

(defun canvas-browser-bookmark-make-record ()
  "A bookmark record of this buffer's page, named by its title.
It is the `bookmark-make-record-function' of a page buffer."
  (unless canvas-browser--url
    (error "canvas-browser: this page has no address to keep"))
  `(,(or canvas-browser--title canvas-browser--url)
    (location . ,canvas-browser--url)
    (handler . canvas-browser-bookmark-jump)
    (defaults . ,(delete-dups (delq nil (list canvas-browser--title canvas-browser--url))))))

(defun canvas-browser--buffer-showing (url)
  "The page buffer that shows URL, or nil."
  (seq-find (lambda (buffer)
              (and (eq (buffer-local-value 'major-mode buffer) 'canvas-browser-mode)
                   (equal (buffer-local-value 'canvas-browser--url buffer) url)))
            (buffer-list)))

;;;###autoload
(defun canvas-browser-bookmark-jump (bookmark)
  "Open the page of BOOKMARK, in the page buffer that shows it if there is one."
  (let ((url (bookmark-prop-get bookmark 'location)))
    (set-buffer (or (canvas-browser--buffer-showing url)
                    (canvas-browser url)))))

(put 'canvas-browser-bookmark-jump 'bookmark-handler-type "Web")

(defun canvas-browser-bookmark ()
  "Keep this page as a bookmark, named by its title unless you name it."
  (interactive)
  (require 'bookmark)
  (call-interactively #'bookmark-set))

(defun canvas-browser--bookmark-names ()
  "The names of the bookmarks of pages, in the order of the bookmark list."
  (mapcar #'car (seq-filter (lambda (bookmark)
                              (eq (bookmark-get-handler bookmark) 'canvas-browser-bookmark-jump))
                            bookmark-alist)))

(defun canvas-browser-open-bookmark ()
  "Pick a bookmark of a page, and open it.
The names are offered as bookmarks, so that marginalia, consult and
embark treat them as they treat any bookmark."
  (interactive)
  (require 'bookmark)
  (bookmark-maybe-load-default-file)
  (let ((names (or (canvas-browser--bookmark-names)
                   (user-error "canvas-browser: no page is bookmarked yet; B keeps this one"))))
    (bookmark-jump
     (completing-read "Page: "
                      (lambda (string predicate action)
                        (if (eq action 'metadata)
                            '(metadata (category . bookmark))
                          (complete-with-action action names string predicate)))
                      nil t))))

(defun canvas-browser--join-consult-web-group ()
  "Put the bookmarks of pages in the Web group of `consult-bookmark'."
  (setq consult-bookmark-narrow
        (mapcar (lambda (group)
                  (if (and (eq (car group) ?w)
                           (not (memq 'canvas-browser-bookmark-jump (cddr group))))
                      (append group '(canvas-browser-bookmark-jump))
                    group))
                consult-bookmark-narrow)))

(with-eval-after-load 'consult
  (canvas-browser--join-consult-web-group))

;;;; Extensions, and uBlock Origin Lite

(defconst canvas-browser--ublock-release
  "https://api.github.com/repos/uBlockOrigin/uBOL-home/releases/latest"
  "Where GitHub says which release of uBlock Origin Lite is the newest.")

(defconst canvas-browser--ublock-name "ublock-origin-lite"
  "The directory of uBlock Origin Lite, among the extensions.")

(defun canvas-browser--ublock-asset (release)
  "The (NAME . URL) of the chromium zip of RELEASE, as GitHub describes it."
  (let ((asset (seq-find (lambda (asset)
                           (string-match-p "\\.chromium\\.zip\\'" (alist-get 'name asset)))
                         (alist-get 'assets release))))
    (unless asset
      (error "canvas-browser: release %s of uBlock Origin Lite has no chromium zip"
             (alist-get 'tag_name release)))
    (cons (alist-get 'name asset) (alist-get 'browser_download_url asset))))

(defvar url-automatic-caching)

(defmacro canvas-browser--fresh (&rest body)
  "Run BODY with url.el asking for a fresh answer, and keeping no copy.
url.el sends If-Modified-Since for any URL it has a cached copy of, as
long as the request has no Pragma header that says no-cache, whatever
`url-automatic-caching' says.  The server then answers 304 with no body,
and url.el does not put the cached copy in its place.  Another package
may turn the caching on for all of Emacs."
  `(let ((url-automatic-caching nil)
         (url-request-extra-headers '(("Pragma" . "no-cache"))))
     ,@body))

(defun canvas-browser--read-json-url (url)
  "The JSON that URL answers with, its objects as alists."
  (let ((buffer (or (canvas-browser--fresh (url-retrieve-synchronously url t t 30))
                    (error "canvas-browser: no answer from %s" url))))
    (with-current-buffer buffer
      (unwind-protect
          (progn
            (unless (equal 200 (bound-and-true-p url-http-response-status))
              (error "canvas-browser: %s answered %s" url
                     (bound-and-true-p url-http-response-status)))
            (goto-char (point-min))
            (re-search-forward "\r?\n\r?\n")
            (json-parse-buffer :object-type 'alist :array-type 'list))
        (kill-buffer buffer)))))

(defun canvas-browser--unpack (zip directory)
  "Unpack ZIP, an extension, into DIRECTORY."
  (unless (executable-find "unzip")
    (user-error "canvas-browser: no unzip; run `sudo apt install unzip'"))
  (make-directory directory t)
  (unless (zerop (call-process "unzip" nil nil nil "-q" zip "-d" directory))
    (error "canvas-browser: unzip failed on %s" zip))
  (unless (file-exists-p (expand-file-name "manifest.json" directory))
    (error "canvas-browser: %s holds no manifest.json" zip)))

(defun canvas-browser--put-extension (unpacked name)
  "Make UNPACKED the extension NAME, in place of an older one; its directory."
  (let ((target (expand-file-name name (canvas-browser-cdp-extension-directory))))
    (when (file-exists-p target)
      (delete-directory target t))
    (rename-file unpacked target)
    target))

(defun canvas-browser--install-extension (url name)
  "Fetch the zip of an extension from URL and install it as NAME.
It is unpacked into a hidden directory beside the extensions, which
chromium leaves alone, and takes its place only once it is whole."
  (let* ((home (canvas-browser-cdp-extension-directory))
         (zip (make-temp-file "canvas-browser-extension" nil ".zip"))
         (unpacked (progn (make-directory home t)
                          (make-temp-name (expand-file-name ".unpacking-" home)))))
    (unwind-protect
        (progn
          (canvas-browser--fresh (url-copy-file url zip t))
          (canvas-browser--unpack zip unpacked)
          (canvas-browser--put-extension unpacked name))
      (delete-file zip)
      (when (file-exists-p unpacked)
        (delete-directory unpacked t)))))

;;;###autoload
(defun canvas-browser-install-ublock ()
  "Install the newest uBlock Origin Lite for canvas-browser, or update it.
It blocks ads and trackers with the lists of uBlock Origin, and chromium
loads it from `canvas-browser-extension-directory' when it starts.  A
chromium that runs is offered a restart, which opens the pages again."
  (interactive)
  (let* ((asset (canvas-browser--ublock-asset
                 (canvas-browser--read-json-url canvas-browser--ublock-release))))
    (canvas-browser--install-extension (cdr asset) canvas-browser--ublock-name)
    (message "canvas-browser: installed %s" (car asset))
    (when (and (canvas-browser-cdp-running-p)
               (y-or-n-p "Restart chromium, so that it loads uBlock Origin Lite? "))
      (canvas-browser-restart-chromium))))

(defun canvas-browser-restart-chromium ()
  "Stop chromium, and open the pages that a window shows in a new one.
Chromium reads its extensions only when it starts.  A page no window
shows opens again when it is next shown."
  (interactive)
  (let ((pages (seq-filter (lambda (buffer)
                             (with-current-buffer buffer
                               (and (derived-mode-p 'canvas-browser-mode) canvas-browser--url)))
                           (buffer-list))))
    (canvas-browser-cdp-stop)
    (dolist (page pages)
      (with-current-buffer page
        (canvas-browser--forget-page)))
    (dolist (page pages)
      (when (canvas-browser--shown-p page)
        (with-current-buffer page
          (canvas-browser--reopen))))))

;;;; A page that calls Emacs

(defvar-local canvas-browser--bindings nil
  "What the page of this buffer can call in Emacs, each (NAME . FUNCTION).
The page calls window.NAME with a string.  FUNCTION is then called in
this buffer with that string, and with the context of the page that
called: a page has one for each of its frames.")

(defun canvas-browser--binding-called (params)
  "Call the function that the page named, with what PARAMS say it gave."
  (when-let* ((function (cdr (assoc (plist-get params :name) canvas-browser--bindings))))
    (funcall function (plist-get params :payload) (plist-get params :executionContextId))))

(defun canvas-browser--on-call (name function)
  "Have FUNCTION called when the page of this buffer calls window.NAME.
See `canvas-browser--bindings\=' for how it is called.  The page has the
name only once `canvas-browser--give-name\=' gave it."
  (setf (alist-get name canvas-browser--bindings nil nil #'equal) function)
  (canvas-browser-cdp-listen canvas-browser--session "Runtime.bindingCalled"
                             (canvas-browser--here #'canvas-browser--binding-called)))

(defun canvas-browser--give-name (name)
  "Give the page of this buffer the function window.NAME, in all its frames.
The frames that are there now get it.  A page that the tab loads later
gets it only while chromium reports on the scripts of the tab, which
`Runtime.enable' turns on, or when the name is given again."
  (canvas-browser--tell "Runtime.addBinding" (list :name name)))

(defun canvas-browser--evaluate-in (context script)
  "Run SCRIPT in the page of this buffer, in CONTEXT, one of its frames."
  (canvas-browser--tell "Runtime.evaluate" (list :expression script :contextId context)))

;;;; What a page copies

(defun canvas-browser--set-permission (name origin setting &optional then)
  "Tell chromium that the permission NAME of the site ORIGIN is SETTING.
SETTING is \"granted\", \"denied\" or \"prompt\".  THEN is called once
chromium has taken it."
  (canvas-browser-cdp-send "Browser.setPermission"
                           (list :permission (list :name name)
                                 :setting setting
                                 :origin origin)
                           (and then (lambda (_result) (funcall then)))))

(defvar canvas-browser--clipboard nil
  "The text that the clipboard of chromium held when Emacs last read it.
Chromium has one clipboard for all its pages.")

(defconst canvas-browser--clipboard-delay 0.5
  "The seconds between a click and the look at the clipboard.
A page copies a moment after the click that asks it to.")

(defconst canvas-browser--clipboard-script
  "navigator.clipboard ? navigator.clipboard.readText().then(text => text, () => null) : null"
  "The JavaScript that gives the text of the clipboard, or null.
A page that is not served over https has no clipboard to read.")

(defun canvas-browser--first-line (text)
  "The first line of TEXT, cut to a length that the echo area shows."
  (truncate-string-to-width (car (split-string text "\n")) 70 nil nil t))

(defun canvas-browser--take-copy (text)
  "Make TEXT the newest kill, when it is new on the clipboard of chromium."
  (when (and (stringp text)
             (not (string-empty-p text))
             (not (equal text canvas-browser--clipboard)))
    (setq canvas-browser--clipboard text)
    (kill-new text)
    (message "canvas-browser: copied %s" (canvas-browser--first-line text))))

(defun canvas-browser--read-clipboard (origin)
  "Read the clipboard of chromium in this page, of the site ORIGIN.
A page may read the clipboard only when its site is allowed to.
Chromium asks for that in its own window, where nobody sees the
question, and a read that waits for the answer never ends.  So the site
is allowed for this one read, and not after it."
  (let ((forbid (lambda (&rest _)
                  (canvas-browser--set-permission "clipboard-read" origin "prompt"))))
    (canvas-browser--set-permission
     "clipboard-read" origin "granted"
     (canvas-browser--here
      (lambda ()
        (canvas-browser--tell
         "Runtime.evaluate"
         (list :expression canvas-browser--clipboard-script
               :awaitPromise t :returnByValue t)
         (lambda (result)
           (funcall forbid)
           (canvas-browser--take-copy (plist-get (plist-get result :result) :value)))
         forbid))))))

(defun canvas-browser--look-at-clipboard (buffer)
  "Take what the page of BUFFER copied, if the buffer still holds a page."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and canvas-browser--session canvas-browser--url)
        (canvas-browser--read-clipboard (canvas-browser--site-of canvas-browser--url))))))

(defun canvas-browser--look-at-clipboard-later ()
  "Look at the clipboard of chromium a moment from now.
A button of a page that copies, as one that copies a link does, puts
its text on the clipboard of chromium, which stands on a display that
nobody sees.  Chromium tells nobody of it.  So Emacs looks after each
click, and what is new there becomes the newest kill."
  (run-at-time canvas-browser--clipboard-delay nil
               #'canvas-browser--look-at-clipboard (current-buffer)))

;;;; The notifications of a page

(defcustom canvas-browser-notification-functions '(canvas-browser-notification-message)
  "Functions that are called with each notification that a page shows.
Each gets one argument, a plist.  `:title' and `:body' are the text of
the notification, and `:tag' and `:icon' what the page gave for those,
each a string that is empty when the page gave none.  `:source' names
where it comes from, as \"example.org\".  `:origin' is the site, as
\"https://example.org\", `:url' and `:page-title' are the address and
the title of the page, and `:buffer' is its page buffer.
`canvas-browser-notification-open\=' shows the page of a notification,
and tells the page that the notification was clicked.

A notification that came through `canvas-browser-notification-service\='
has the last four only when a page of its site is open.  Its `:source'
is the name of the program when no browser showed it.

canvas-browser shows nothing itself: what is made of a notification is
the business of these functions.  As the package comes, one function
says the notification in the echo area.

Only a site that `canvas-browser-notifications\=' allowed shows
notifications."
  :type 'hook
  :group 'canvas-browser)

(defcustom canvas-browser-notification-sites nil
  "The sites that may show notifications, and those that may not.
Each is (ORIGIN . ALLOWED).  ORIGIN is the site as a page names it,
such as \"https://example.org\".  ALLOWED is non-nil for a site that
may show notifications, and nil for one that may not.
`canvas-browser-notifications\=' adds the site of a page, and changes
its answer."
  :type '(alist :key-type string :value-type boolean)
  :group 'canvas-browser)

(defconst canvas-browser--notify-binding "canvasBrowserNotify"
  "The function a page calls to tell Emacs of its notifications.")

(defconst canvas-browser--notify-script
  (format "(() => {
  if (!window.Notification || window.__canvasBrowserNotifications) return;
  if (Notification.permission !== 'granted') return;
  const real = window.Notification;
  const tell = (message, tries) => {
    if (typeof window.%1$s === 'function')
      window.%1$s(JSON.stringify({origin: location.origin, ...message}));
    else if ((tries || 0) < 20) setTimeout(() => tell(message, (tries || 0) + 1), 250);
  };
  const shown = new Map();
  let next = 0;
  const fire = (target, type) => {
    const event = new Event(type);
    target.dispatchEvent(event);
    if (typeof target['on' + type] === 'function') target['on' + type](event);
  };
  const show = (title, options, target) => {
    const id = ++next;
    shown.set(id, target);
    tell({event: 'show', id, title: String(title), body: String(options.body || ''),
          tag: String(options.tag || ''), icon: String(options.icon || '')});
    return id;
  };
  class Shown extends EventTarget {
    constructor(title, options) {
      super();
      options = options || {};
      Object.assign(this, {title: String(title), body: options.body || '', tag: options.tag || '',
                           icon: options.icon || '',
                           data: options.data === undefined ? null : options.data,
                           onclick: null, onclose: null, onerror: null, onshow: null});
      const may = real.permission === 'granted';
      if (may) this.shownAs = show(title, options, this);
      queueMicrotask(() => fire(this, may ? 'show' : 'error'));
    }
    close() { if (shown.delete(this.shownAs)) fire(this, 'close'); }
    static get permission() { return real.permission; }
    static get maxActions() { return real.maxActions; }
    static requestPermission(then) {
      const answer = Promise.resolve(real.permission);
      if (typeof then === 'function') answer.then(then);
      return answer;
    }
  }
  window.Notification = Shown;
  if (window.ServiceWorkerRegistration)
    ServiceWorkerRegistration.prototype.showNotification = function (title, options) {
      if (real.permission !== 'granted')
        return Promise.reject(new TypeError('No notification permission has been granted.'));
      show(title, options || {}, null);
      return Promise.resolve();
    };
  window.__canvasBrowserNotifications = {
    click: id => {
      const target = shown.get(id);
      if (target) fire(target, 'click');
      return shown.has(id);
    }
  };
})();" canvas-browser--notify-binding)
  "The script that hands the notifications of a page to Emacs.
Chromium shows a notification in a window of its own, on a display that
nobody sees.  So on a site that may show notifications, the page gets a
`Notification\=' that shows nothing and tells Emacs.  A notification that
the page asks its service worker to show is taken the same way.  One
that a service worker shows on its own, while no page asks, is not: the
script is in pages only.

The script goes into every page, and does nothing on a site that may
not show notifications.  Such a site finds its browser as chromium made
it, with nothing put in.  A site that looks for a browser that a
program drives finds none here: the reader drives this one.

The page may call Emacs before chromium has given it the name to call,
in the first moment after it loads, so it tries again for a while.")

(defun canvas-browser--site-of (url)
  "The site of URL as a page names its own.
That is the scheme and the host, and the port when it is not the usual
one for the scheme."
  (let* ((parsed (url-generic-parse-url url))
         (port (url-port-if-non-default parsed)))
    (concat (url-type parsed) "://" (url-host parsed) (if port (format ":%d" port) ""))))

(defun canvas-browser--site-host (site)
  "SITE without its scheme, which is how chromium names a site to a reader."
  (replace-regexp-in-string "\\`[a-z]+://" "" site))

(defun canvas-browser--may-notify-p (url)
  "Whether the site of URL is kept as one that may show notifications."
  (cdr (assoc (canvas-browser--site-of url) canvas-browser-notification-sites)))

(defun canvas-browser--notification (said context)
  "The notification that a frame of this page told of in SAID, from CONTEXT.
It is the plist that `canvas-browser-notification-functions\=' get."
  (list :title (plist-get said :title)
        :body (plist-get said :body)
        :tag (plist-get said :tag)
        :icon (plist-get said :icon)
        :source (canvas-browser--site-host (plist-get said :origin))
        :origin (plist-get said :origin)
        :url canvas-browser--url
        :page-title (or canvas-browser--title canvas-browser--url)
        :buffer (current-buffer)
        :id (plist-get said :id)
        :context context))

(defun canvas-browser-notification-message (notification)
  "Say NOTIFICATION, which a page showed, in the echo area."
  (message "%s: %s"
           (plist-get notification :source)
           (string-join (delete "" (list (plist-get notification :title)
                                         (plist-get notification :body)))
                        ": ")))

(defun canvas-browser--click-in-page (notification)
  "Tell the page of NOTIFICATION, which it showed itself, that it was clicked."
  (let ((buffer (plist-get notification :buffer)))
    (unless (buffer-live-p buffer)
      (user-error "canvas-browser: the page of that notification is gone"))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (canvas-browser--evaluate-in
       (plist-get notification :context)
       (format "window.__canvasBrowserNotifications.click(%d)" (plist-get notification :id))))))

(defconst canvas-browser--click-wait 2
  "Seconds a site has to open a page of its own after a click on its notification.")

(defun canvas-browser--show-site (origin)
  "Show a page of the site ORIGIN: one that is open, or else a new one."
  (let ((page (canvas-browser--page-of (canvas-browser--site-host origin))))
    (if page
        (pop-to-buffer page)
      (canvas-browser origin))))

(defun canvas-browser--click-on-bus (notification)
  "Tell the program that showed NOTIFICATION through the bus that it was clicked.
The page of its site is shown.  A site with no page open may open one
itself when it hears of the click, as a mail site opens the mail: that
page gets a buffer as any tab that chromium opens.  When the site has
opened none a moment later, the site is opened in a page buffer."
  (let ((buffer (plist-get notification :buffer))
        (origin (plist-get notification :origin)))
    (cond ((buffer-live-p buffer) (pop-to-buffer buffer))
          (origin (run-at-time canvas-browser--click-wait nil
                               #'canvas-browser--show-site origin))))
  (canvas-browser--dbus-signal "ActionInvoked"
                               :uint32 (plist-get notification :service-id) "default"))

(defun canvas-browser-notification-open (notification)
  "Show the page of NOTIFICATION, and say that the notification was clicked.
NOTIFICATION is what `canvas-browser-notification-functions\=' got.  A
site acts on the click as it likes: a mail site opens the mail."
  (if (plist-get notification :service-id)
      (canvas-browser--click-on-bus notification)
    (canvas-browser--click-in-page notification)))

(defun canvas-browser--notify-called (payload context)
  "Take PAYLOAD, what a frame of this page said of a notification, from CONTEXT."
  (run-hook-with-args 'canvas-browser-notification-functions
                      (canvas-browser--notification
                       (json-parse-string payload :object-type 'plist) context)))

(defun canvas-browser--set-notification-permission (origin allowed &optional then)
  "Tell chromium that the site ORIGIN may show notifications, or may not.
THEN is called once chromium has taken it."
  (canvas-browser--set-permission "notifications" origin (if allowed "granted" "denied")
                                  then))

(defun canvas-browser--frame-may-notify (url)
  "Give a frame of this page that went to URL the name to call Emacs by.
It gets the name only when its site may show notifications.  Chromium
keeps such a name for the frames that are there, so it is given again
each time a frame goes to a new page."
  (when (and url (canvas-browser--may-notify-p url))
    (canvas-browser--give-name canvas-browser--notify-binding)))

(defun canvas-browser--take-notifications-now ()
  "Take the notifications of the page that this buffer holds now.
A page that was there before its site was allowed is not loaded again
for it."
  (canvas-browser--give-name canvas-browser--notify-binding)
  (canvas-browser--tell "Runtime.evaluate" (list :expression canvas-browser--notify-script)))

(defun canvas-browser--decide-notifications (origin allowed)
  "Keep that the site ORIGIN may show notifications, or may not, and act on it."
  (setf (alist-get origin canvas-browser-notification-sites nil nil #'equal) allowed)
  (customize-save-variable 'canvas-browser-notification-sites canvas-browser-notification-sites)
  (canvas-browser--set-notification-permission
   origin allowed
   (and allowed (canvas-browser--here #'canvas-browser--take-notifications-now))))

(defun canvas-browser-notifications ()
  "Say whether the site of this page may show notifications.
The answer is kept in `canvas-browser-notification-sites\=' for the
sessions to come.  Run the command again to change it.

Chromium asks this in its own window when a page wants to show
notifications, where nobody sees the question.  So a page waits for an
answer that does not come until this command gives one."
  (interactive nil canvas-browser-mode)
  (let ((origin (canvas-browser--this-site)))
    (canvas-browser--decide-notifications
     origin (y-or-n-p (format "Let %s show notifications? " origin)))))

(defun canvas-browser--this-site ()
  "The site of the page that this buffer holds.  An error when it holds none."
  (unless canvas-browser--url
    (user-error "canvas-browser: no page here"))
  (canvas-browser--site-of canvas-browser--url))

(defun canvas-browser--notify-description ()
  "The entry of the menu that says whether the site of this page may notify."
  (canvas-keys-describe
   "notify"
   (if (and canvas-browser--url (canvas-browser--may-notify-p canvas-browser--url))
       "on"
     "off")))

(defun canvas-browser-toggle-notifications ()
  "Let the site of this page show notifications, or stop it.
The menu of the page says which of the two holds.  This is the answer
that `canvas-browser-notifications\=' asks for, given without a question."
  (interactive nil canvas-browser-mode)
  (let ((origin (canvas-browser--this-site)))
    (canvas-browser--decide-notifications
     origin (not (canvas-browser--may-notify-p canvas-browser--url)))))

(defun canvas-browser--watch-notifications ()
  "Take the notifications of the pages of this buffer.
See `canvas-browser--notify-script\='.  Chromium is told of the sites
that are kept, so that a page of one finds its answer there."
  (pcase-dolist (`(,origin . ,allowed) canvas-browser-notification-sites)
    (canvas-browser--set-notification-permission origin allowed))
  (canvas-browser--on-call canvas-browser--notify-binding #'canvas-browser--notify-called)
  (canvas-browser--tell "Page.addScriptToEvaluateOnNewDocument"
                        (list :source canvas-browser--notify-script)))

;;;; Emacs as the notification service of the session

(defcustom canvas-browser-notification-service nil
  "Whether canvas-browser is the notification service of the session.
A service worker of a site shows notifications without a page: a mail
site does so for mail that it is told of by web push.  Chromium hands
those to the notification service of the desktop, over D-Bus.  With
this non-nil, and when no other program is that service, canvas-browser
is it, and the notifications go to
`canvas-browser-notification-functions\='.

Emacs is then the service for every program of the session, not for
chromium alone.  A desktop with a notification service of its own keeps
it, and shows the notifications itself.

Chromium looks for the service when it starts, so the option counts for
a chromium that starts after it was set."
  :type 'boolean
  :group 'canvas-browser)

(declare-function dbus-get-name-owner "dbus" (bus service))
(declare-function dbus-register-service "dbusbind.c" (bus service &rest flags))
(declare-function dbus-register-method "dbus"
                  (bus service path interface method handler &optional dont-register-service))
(declare-function dbus-send-signal "dbus" (bus service path interface signal &rest args))

(defconst canvas-browser--dbus-service "org.freedesktop.Notifications"
  "The name of the notification service on the session bus, and of its interface.")

(defconst canvas-browser--dbus-path "/org/freedesktop/Notifications"
  "The path of the notification service on the session bus.")

(defvar canvas-browser--dbus-served nil
  "Non-nil once Emacs is the notification service of the session.")

(defvar canvas-browser--dbus-count 0
  "The id of the newest notification that came through the bus.")

(defun canvas-browser--dbus-free-p ()
  "Whether Emacs can be the notification service of the session.
It can when it reaches the session bus, and no program is the service."
  (and (featurep 'dbusbind)
       (require 'dbus nil t)
       (condition-case nil
           (not (dbus-get-name-owner :session canvas-browser--dbus-service))
         (error nil))))

(defun canvas-browser--dbus-signal (name &rest arguments)
  "Tell every program on the session bus NAME with ARGUMENTS.
NAME is a signal of the notification service."
  (apply #'dbus-send-signal :session nil canvas-browser--dbus-path
         canvas-browser--dbus-service name arguments))

(defun canvas-browser--dbus-capabilities ()
  "What the notification service can do, as a program asks for it.
It takes a text, and actions: chromium hands over a click as an action."
  '(("body" "actions")))

(defun canvas-browser--dbus-information ()
  "Who the notification service is: its name, vendor, version and standard."
  (list "canvas-browser" "canvas-browser" "1" "1.2"))

(defun canvas-browser--page-of (host)
  "A page buffer whose page is of the site that chromium names HOST, or nil."
  (seq-find (lambda (buffer)
              (with-current-buffer buffer
                (and (derived-mode-p 'canvas-browser-mode)
                     canvas-browser--url
                     (equal host (canvas-browser--site-host
                                  (canvas-browser--site-of canvas-browser--url))))))
            (buffer-list)))

(defun canvas-browser--dbus-parts (app body)
  "(SOURCE TEXT SITE) of a notification that the program APP showed with BODY.
Chromium starts the body of the notification of a site with the site
and an empty line.  The source is then the site, the text is what
follows, and SITE is non-nil.  For another program the source is its
name, and the text is the body."
  (if (and (string-match-p "chrom" (downcase app))
           (string-match "\\`\\([^\n]+\\)\\(?:\n\n\\(\\(?:.\\|\n\\)*\\)\\)?\\'" body))
      (list (match-string 1 body) (or (match-string 2 body) "") t)
    (list app body nil)))

(defun canvas-browser--dbus-notification (id app summary body)
  "The notification that the program APP showed through the bus, for the hook.
ID is its id on the bus, SUMMARY its title and BODY its text.  It has a
page when a page of its site is open.  A site without a page has its
address alone, which is taken to be an https one: chromium names a site
without its scheme, and a site that a service worker shows
notifications for is such a site."
  (pcase-let* ((`(,source ,text ,site) (canvas-browser--dbus-parts app body))
               (page (canvas-browser--page-of source)))
    (append (list :title summary :body text :tag "" :icon "" :source source :service-id id)
            (cond (page
                   (with-current-buffer page
                     (list :origin (canvas-browser--site-of canvas-browser--url)
                           :url canvas-browser--url
                           :page-title (or canvas-browser--title canvas-browser--url)
                           :buffer page)))
                  (site (list :origin (concat "https://" source)))))))

(defun canvas-browser--dbus-notify (app replaces _icon summary body _actions _hints _timeout)
  "Take a notification that the program APP shows through the session bus.
REPLACES is the id of the one that it takes the place of, or 0.  SUMMARY
and BODY are its title and its text.  Return its id, as the bus has it."
  (let ((id (if (> replaces 0) replaces (cl-incf canvas-browser--dbus-count))))
    (run-hook-with-args 'canvas-browser-notification-functions
                        (canvas-browser--dbus-notification id app summary body))
    (list :uint32 id)))

(defun canvas-browser--dbus-close (id)
  "Take the notification ID back, as its program asks.
Emacs keeps no notification, so the programs are only told that this
one is closed."
  (canvas-browser--dbus-signal "NotificationClosed" :uint32 id :uint32 3)
  :ignore)

(defun canvas-browser--serve-notifications ()
  "Be the notification service of the session, when asked for and free.
See `canvas-browser-notification-service\='."
  (when (and canvas-browser-notification-service
             (not canvas-browser--dbus-served)
             (canvas-browser--dbus-free-p))
    (dbus-register-service :session canvas-browser--dbus-service)
    (pcase-dolist (`(,method . ,handler)
                   '(("GetCapabilities" . canvas-browser--dbus-capabilities)
                     ("GetServerInformation" . canvas-browser--dbus-information)
                     ("Notify" . canvas-browser--dbus-notify)
                     ("CloseNotification" . canvas-browser--dbus-close)))
      (dbus-register-method :session canvas-browser--dbus-service canvas-browser--dbus-path
                            canvas-browser--dbus-service method handler))
    (setq canvas-browser--dbus-served t)))

;;;; A page embedded in another buffer

(defun canvas-browser--embed-at (position)
  "The page embedded at POSITION of this buffer, or a user error."
  (or (get-text-property position 'canvas-browser-embed)
      (user-error "canvas-browser: no page here")))

(defun canvas-browser-embed-click (event)
  "Click the embedded page where EVENT, a click on its picture, points."
  (interactive "e")
  (let* ((start (event-start event))
         (page (with-current-buffer (window-buffer (posn-window start))
                 (canvas-browser--embed-at (posn-point start)))))
    (with-current-buffer page
      (canvas-browser-click event))))

(defun canvas-browser-embed-click-middle ()
  "Click the middle of the page embedded at point.
A video starts or stops at a click on it."
  (interactive)
  (with-current-buffer (canvas-browser--embed-at (point))
    (canvas-browser--click (/ (car canvas-browser--size) 2)
                           (/ (cdr canvas-browser--size) 2))))

(defvar canvas-browser-embed-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'canvas-browser-embed-click-middle)
    (define-key map [return] #'canvas-browser-embed-click-middle)
    (canvas-browser--bind-clicks map #'canvas-browser-embed-click))
  "The keys of the picture of an embedded page, in the buffer it is in.
The wheel is left to that buffer, which it scrolls.")

(defun canvas-browser--embed-gone-p (buffer)
  "Whether BUFFER holds an embedded page its host has let go of.
The host has, once it is killed or holds the page's picture no more, as
after it is drawn again."
  (let ((host (buffer-local-value 'canvas-browser--host buffer)))
    (and host
         (or (not (buffer-live-p host))
             (with-current-buffer host
               (not (text-property-any (point-min) (point-max)
                                       'canvas-browser-embed buffer)))))))

(declare-function x-window-property "xfns.c")

(defconst canvas-browser--fullscreen-binding "canvasBrowserFullscreen"
  "The function an embedded page calls to say it went fullscreen or back.")

(defconst canvas-browser--fullscreen-script
  (format "document.addEventListener('fullscreenchange', () =>
  window.%s(document.fullscreenElement ? 'on' : 'off'));"
          canvas-browser--fullscreen-binding)
  "The script that tells Emacs when a page goes fullscreen, and back.")

(defvar-local canvas-browser--fullscreen-frame nil
  "The frame this embedded page fills while it is fullscreen, or nil.")

(defvar-local canvas-browser--embed-size nil
  "The size this embedded page has in its host, kept while it is fullscreen.")

(defun canvas-browser--watch-fullscreen ()
  "Hear when this embedded page goes fullscreen, and when it comes back.
Fullscreen fills the page, which is only as large as its picture in the
host, so Emacs gives the page a frame of its own to fill instead.
The script goes in before the page loads, and the binding is kept for
every page the tab loads later."
  (canvas-browser--on-call canvas-browser--fullscreen-binding
                           (lambda (payload _context)
                             (canvas-browser--fullscreen-changed (equal payload "on"))))
  (canvas-browser--tell "Runtime.enable" nil)
  (canvas-browser--give-name canvas-browser--fullscreen-binding)
  (canvas-browser--tell "Page.addScriptToEvaluateOnNewDocument"
                        (list :source canvas-browser--fullscreen-script)))

(defun canvas-browser--fullscreen-changed (on)
  "Fill a frame with this embedded page if ON, else put it back in its host."
  (if on
      (canvas-browser--enter-fullscreen)
    (canvas-browser--leave-fullscreen)))

(defun canvas-browser--wm-fullscreen-p (frame)
  "Whether the window manager of FRAME can make a frame fullscreen.
On X, Emacs asks the window manager for it, and without one that says it
can, Emacs stretches the frame over the whole X screen, which spans every
monitor.  These are the two things Emacs reads on the root window."
  (or (not (eq (window-system frame) 'x))
      (and (x-window-property "_NET_SUPPORTING_WM_CHECK" frame "WINDOW" 0 nil t)
           (seq-contains-p (x-window-property "_NET_SUPPORTED" frame "ATOM" 0 nil t)
                           '_NET_WM_STATE_FULLSCREEN)
           t)))

(defun canvas-browser--frame-offset (position)
  "POSITION as a frame parameter counts it: a negative one from the left too.
A plain negative number counts from the right or bottom edge instead."
  (if (< position 0) (list '+ position) position))

(defun canvas-browser--frame-place (frame)
  "The frame parameters that open a new frame where FRAME is, at its size."
  (let ((position (frame-position frame)))
    `((left . ,(canvas-browser--frame-offset (car position)))
      (top . ,(canvas-browser--frame-offset (cdr position)))
      (width . (text-pixels . ,(frame-text-width frame)))
      (height . (text-pixels . ,(frame-text-height frame)))
      (user-position . t)
      (user-size . t))))

(defun canvas-browser--fullscreen-parameters (frame)
  "The parameters of the frame a fullscreen page fills, over FRAME.
It opens where FRAME is, so a window manager makes it fullscreen on the
monitor you look at.  Where it cannot go fullscreen it keeps FRAME\\='s
place and size, because the monitors may not be known: an X server can
report a single screen for two monitors."
  (append (when (canvas-browser--wm-fullscreen-p frame)
            '((fullscreen . fullboth)))
          (canvas-browser--frame-place frame)))

(defun canvas-browser--host-frame ()
  "The frame that shows this page\\='s host, or else the selected frame."
  (let ((window (get-buffer-window canvas-browser--host 'visible)))
    (if window (window-frame window) (selected-frame))))

(defun canvas-browser--enter-fullscreen ()
  "Show this embedded page in a fullscreen frame of its own.
The window changes that follow lay the page out for that frame."
  (unless (frame-live-p canvas-browser--fullscreen-frame)
    (setq canvas-browser--embed-size canvas-browser--size)
    (let ((window (display-buffer
                   (current-buffer)
                   `(display-buffer-pop-up-frame
                     (pop-up-frame-parameters
                      . ,(canvas-browser--fullscreen-parameters (canvas-browser--host-frame)))))))
      (setq canvas-browser--fullscreen-frame (window-frame window))
      (select-frame-set-input-focus canvas-browser--fullscreen-frame))))

(defun canvas-browser--leave-fullscreen ()
  "Delete the fullscreen frame of this embedded page, and put it back in its host."
  (let ((frame canvas-browser--fullscreen-frame))
    (setq canvas-browser--fullscreen-frame nil)
    (when (frame-live-p frame)
      (delete-frame frame)))
  (canvas-browser--back-in-host))

(defun canvas-browser--back-in-host ()
  "Give this page its size in its host again, and show it there.
Nothing is done for a page that is not fullscreen, so the page saying it
left fullscreen after Emacs made it leave does no harm."
  (when-let* ((size canvas-browser--embed-size))
    (setq canvas-browser--embed-size nil)
    (canvas-browser--window-resized (car size) (cdr size))
    (canvas-browser--show-in-host)))

(defun canvas-browser--show-in-host ()
  "Show this page's canvas where its host holds the picture of it.
A page of a new size has a new canvas, and the host still shows the old one."
  (let ((page (current-buffer))
        (canvas canvas-browser--canvas))
    (with-current-buffer canvas-browser--host
      (with-silent-modifications
        (let ((start (point-min)))
          (while (setq start (text-property-any start (point-max) 'canvas-browser-embed page))
            (let ((end (next-single-property-change start 'canvas-browser-embed nil (point-max))))
              (put-text-property start end 'display canvas)
              (setq start end))))))))

(defun canvas-browser--fullscreen-frame-deleted (frame)
  "Take the embedded page that fills FRAME, which is going, out of fullscreen.
Quitting the window of that page deletes the frame, and the page is then
told to leave fullscreen too."
  (dolist (buffer (buffer-list))
    (when (eq (buffer-local-value 'canvas-browser--fullscreen-frame buffer) frame)
      (with-current-buffer buffer
        (setq canvas-browser--fullscreen-frame nil)
        (canvas-browser--tell "Runtime.evaluate" (list :expression "document.exitFullscreen()"))
        (canvas-browser--back-in-host)))))

(add-hook 'delete-frame-functions #'canvas-browser--fullscreen-frame-deleted)

(defun canvas-browser--kill-embeds ()
  "Kill the pages embedded in this buffer, which is going."
  (let ((host (current-buffer)))
    (dolist (buffer (buffer-list))
      (when (eq (buffer-local-value 'canvas-browser--host buffer) host)
        (kill-buffer buffer)))))

;;;###autoload
(defun canvas-browser-embed (url width height host)
  "Open URL in a page WIDTH by HEIGHT whose picture shows in HOST, a buffer.
Each side is at least `canvas-browser--least-size'.  Return the text to
insert in HOST, or nil where no canvas can show it.
The page lives in a hidden buffer of its own, and is drawn while a
window shows HOST.  A click on the picture goes to the page, and RET
clicks its middle.  The page goes when HOST is killed, and once HOST
holds the text no more."
  (when (canvas-browser-can-show-p)
    (let* ((url (canvas-browser--reachable-url url))
           (page (generate-new-buffer (format " *canvas-browser embed: %s*" url))))
      (with-current-buffer page
        (canvas-browser-mode)
        (setq canvas-browser--host host)
        (canvas-browser--open url width height))
      (with-current-buffer host
        (add-hook 'kill-buffer-hook #'canvas-browser--kill-embeds nil t))
      (propertize "#" 'display (buffer-local-value 'canvas-browser--canvas page)
                  'canvas-browser-embed page
                  'keymap canvas-browser-embed-map
                  'pointer 'hand
                  'help-echo "A click reaches the page; RET clicks its middle"))))

(provide 'canvas-browser)
;;; canvas-browser.el ends here
