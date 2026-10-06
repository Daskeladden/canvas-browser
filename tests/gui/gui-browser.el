;;; gui-browser.el --- The mouse on a page in a real frame -*- lexical-binding: t -*-

;; `make gui' loads this in an Emacs on a virtual display.  It shows a
;; page buffer in a window beside a window of text, which is the
;; selected one, with `pixel-scroll-precision-mode' on.  No chromium
;; runs: the functions that talk to it are replaced, and each message
;; that a gesture sends to the page is a note.  The notes go to the file
;; `result' once drive.py made the file `done'.

;; Whatever goes wrong, this Emacs ends: the runner waits for it.
(run-with-timer 60 nil (lambda () (kill-emacs 3)))

(require 'cl-lib)
(require 'canvas-browser)

(defvar gui-notes nil)
(defvar gui-dir (file-name-directory load-file-name))
(defvar gui-buffer nil)
(defvar gui-noting nil
  "Non-nil once the page is set up: from then on a message is a note.")

(defconst gui-link '(:x 300 :y 200 :w 200 :h 60)
  "The box of the one link of the page, in pixels of the picture.")

(defun gui-where ()
  "Whether the keys of this command came through the hot spot of a link."
  (if (eq (aref (this-command-keys-vector) 0) 'canvas-browser-link)
      'over-a-link
    'beside-a-link))

(defun gui-selected ()
  "Which window is selected now."
  (if (eq (window-buffer (selected-window)) gui-buffer)
      'the-page-is-selected
    'another-window-is-selected))

(defvar gui-counted 0
  "How many commands the page counted at the last note.")

(defun gui-counted ()
  "Whether the page counted a command since the last note.
The page counts a command to draw its whole window from the next frame.
Its hook for that runs in the buffer of the selected window alone."
  (let ((now (buffer-local-value 'canvas-browser--commands gui-buffer)))
    (prog1 (if (> now gui-counted) 'the-page-counts-it 'THE-PAGE-COUNTS-NOTHING)
      (setq gui-counted now))))

(defun gui-note (method params)
  "Keep a note of METHOD with PARAMS, if it is a message of a gesture."
  (pcase method
    ("Input.dispatchMouseEvent"
     (push (list (intern (plist-get params :type)) (plist-get params :x) (plist-get params :y)
                 'by (plist-get params :deltaX) (plist-get params :deltaY)
                 (gui-where) (gui-counted) (gui-selected))
           gui-notes))
    ("Emulation.setDeviceMetricsOverride"
     (push (list 'zoom (plist-get params :deviceScaleFactor) (gui-where) (gui-counted)
                 (gui-selected))
           gui-notes))))

(defun gui-send (method params &optional answer &rest _)
  "Take METHOD with PARAMS in the place of chromium, and answer as a
chromium does that opened a page."
  (when gui-noting
    (gui-note method params))
  (when answer
    (funcall answer '(:targetId "T1" :sessionId "S1"))))

(advice-add 'canvas-browser-cdp-start :override #'ignore)
(advice-add 'canvas-browser-cdp-running-p :override (lambda () t))
(advice-add 'canvas-browser-cdp-listen :override #'ignore)
(advice-add 'canvas-browser-cdp-forget :override #'ignore)
(advice-add 'canvas-browser-cdp-send :override #'gui-send)
;; The page of this check has one link, which this file puts there.
(advice-add 'canvas-browser--sync-spots :override #'ignore)

(defun gui-finish ()
  (when (file-exists-p (expand-file-name "done" gui-dir))
    (with-temp-file (expand-file-name "result" gui-dir)
      (dolist (note (reverse gui-notes)) (insert (format "%S\n" note))))
    (kill-emacs 0)))

(defun gui-ready (window)
  "Put the link on the page of WINDOW, and tell drive.py where the page is."
  (with-current-buffer gui-buffer
    (canvas-browser--put-spots (list gui-link)))
  (redisplay t)
  (setq gui-counted (buffer-local-value 'canvas-browser--commands gui-buffer)
        gui-noting t)
  ;; The picture starts where the body of the window starts.
  ;; `window-absolute-pixel-position' is no help here: it counts the
  ;; header line two times, so it names a place that many pixels too low.
  (let ((edges (window-absolute-body-pixel-edges window)))
    (with-temp-file (expand-file-name "ready" gui-dir)
      (insert (format "%d %d\n" (nth 0 edges) (nth 1 edges))))))

(defun gui-start ()
  ;; This minor mode binds the plain wheel in every buffer.
  (pixel-scroll-precision-mode 1)
  (set-frame-size (selected-frame) 1300 700 t)
  (switch-to-buffer (get-buffer-create "text"))
  (insert (make-string 3000 ?x))
  (delete-other-windows)
  (let ((right (split-window-right)))
    (setq gui-buffer (get-buffer-create "page"))
    (set-window-buffer right gui-buffer)
    (with-current-buffer gui-buffer
      (canvas-browser-mode)
      ;; The mode gives the window a header line.  The body has its size
      ;; only once Emacs drew that line.
      (redisplay t)
      (canvas-browser--open "https://example.org"
                            (window-body-width right t) (window-body-height right t)))
    (run-with-timer 1.5 nil #'gui-ready right)
    (run-with-timer 0.2 0.2 #'gui-finish)))

(add-hook 'window-setup-hook (lambda () (run-with-timer 0.5 nil #'gui-start)))
