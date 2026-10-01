;;; canvas-browser-cdp.el --- The DevTools protocol of a headless chromium -*- lexical-binding: t -*-

;; Copyright (C) 2026 canvas-browser contributors

;; Author: Daskeladden
;; Version: 0.1.0
;; Keywords: hypermedia, tools
;; URL: https://github.com/Daskeladden/canvas-browser

;;; Commentary:
;; Emacs speaks the DevTools protocol itself: it starts one headless
;; chromium, connects to its websocket, sends commands with an id each,
;; and hands the answers and the events to the page buffers.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'websocket)

(defgroup canvas-browser nil
  "A web browser in an Emacs buffer, drawn on a canvas."
  :group 'applications
  :prefix "canvas-browser-")

(defcustom canvas-browser-chromium '("chromium" "chromium-browser" "google-chrome")
  "The names of the chromium to run, in the order they are looked for."
  :type '(repeat string)
  :group 'canvas-browser)

(defun canvas-browser-cdp--snap-executable-p (executable)
  "Whether the chromium EXECUTABLE is a snap."
  (string-prefix-p "/snap/" executable))

(defun canvas-browser-cdp-snap-home ()
  "The directory of the home that a snap chromium has for its own."
  (expand-file-name "snap/chromium/common" "~"))

(defun canvas-browser-cdp-snap-p ()
  "Whether the chromium in use is a snap."
  (canvas-browser-cdp--snap-executable-p (canvas-browser-cdp--executable)))

(defun canvas-browser-cdp--profile-for (executable)
  "Where the chromium EXECUTABLE keeps its profile.
A snap may write only outside the hidden directories of the home, so its
profile goes under ~/snap, where the snap has a place of its own."
  (if (canvas-browser-cdp--snap-executable-p executable)
      (expand-file-name "canvas-browser-profile" (canvas-browser-cdp-snap-home))
    (expand-file-name "canvas-browser/profile"
                      (or (getenv "XDG_CACHE_HOME") (expand-file-name "~/.cache")))))

(defcustom canvas-browser-profile-directory nil
  "Where chromium keeps its profile, so that a site stays logged in.
Nil lets `canvas-browser-cdp--profile-for' choose by the chromium found."
  :type '(choice (const :tag "By the chromium found" nil) directory)
  :group 'canvas-browser)

(defun canvas-browser-cdp--extensions-for (executable)
  "Where the chromium EXECUTABLE finds the extensions it loads.
A snap reads nothing in the hidden directories of the home, so its
extensions go under ~/snap, as its profile does."
  (if (canvas-browser-cdp--snap-executable-p executable)
      (expand-file-name "canvas-browser-extensions" (canvas-browser-cdp-snap-home))
    (expand-file-name "canvas-browser/extensions"
                      (or (getenv "XDG_DATA_HOME") (expand-file-name "~/.local/share")))))

(defcustom canvas-browser-extension-directory nil
  "The directory of the unpacked extensions chromium loads.
Each directory in it that holds a manifest.json is an extension, and
chromium loads every one when it starts.  Nil lets
`canvas-browser-cdp--extensions-for' choose by the chromium found.
`canvas-browser-install-ublock' puts uBlock Origin Lite there."
  :type '(choice (const :tag "By the chromium found" nil) directory)
  :group 'canvas-browser)

(defcustom canvas-browser-cdp-timeout 10
  "Seconds to wait for chromium to open its port."
  :type 'number
  :group 'canvas-browser)

(defvar canvas-browser-cdp--process nil
  "The chromium process, or nil.")

(defvar canvas-browser-cdp--socket nil
  "The websocket to chromium, or nil.")

(defun canvas-browser-cdp--executable ()
  "The chromium to run, or a user error that says what to install."
  (or (cl-some #'executable-find canvas-browser-chromium)
      (user-error "canvas-browser: no chromium; run `sudo snap install chromium'")))

(defun canvas-browser-cdp--profile ()
  "The profile directory in use."
  (or canvas-browser-profile-directory
      (canvas-browser-cdp--profile-for (canvas-browser-cdp--executable))))

(defun canvas-browser-cdp-extension-directory ()
  "The extension directory in use."
  (or canvas-browser-extension-directory
      (canvas-browser-cdp--extensions-for (canvas-browser-cdp--executable))))

(defun canvas-browser-cdp--extensions ()
  "The directories of the extensions chromium loads, in the order of their names.
A hidden directory is left out: an extension is unpacked into one
before it takes its place."
  (let ((directory (canvas-browser-cdp-extension-directory)))
    (and (file-directory-p directory)
         (seq-filter (lambda (extension)
                       (file-exists-p (expand-file-name "manifest.json" extension)))
                     (directory-files directory t "\\`[^.]")))))

(defun canvas-browser-cdp--extension-flags ()
  "The flags that load the extensions, or nil when there are none.
The flag lists the directories with commas, so a directory whose name
holds one is an error rather than two wrong ones."
  (when-let* ((extensions (canvas-browser-cdp--extensions)))
    (dolist (extension extensions)
      (when (string-search "," extension)
        (error "canvas-browser: an extension's directory holds a comma: %s" extension)))
    (list (concat "--load-extension=" (string-join extensions ",")))))

(defun canvas-browser-cdp--port-file ()
  "The file in which chromium writes its port."
  (expand-file-name "DevToolsActivePort" (canvas-browser-cdp--profile)))

(defun canvas-browser-cdp--no-port-error ()
  "Say why no port file came, and signal it.
A chromium that exits at once handed its work to a chromium that already
uses the profile, and that one alone listens."
  (if (and canvas-browser-cdp--process (not (process-live-p canvas-browser-cdp--process)))
      (error "canvas-browser: chromium exited; another chromium uses the profile %s"
             (canvas-browser-cdp--profile))
    (error "canvas-browser: chromium did not open a port in %s seconds"
           canvas-browser-cdp-timeout)))

(defun canvas-browser-cdp--wait-for-port ()
  "Wait until chromium has written its port file, or signal an error."
  (let ((deadline (+ (float-time) canvas-browser-cdp-timeout)))
    (while (and (not (file-exists-p (canvas-browser-cdp--port-file)))
                (< (float-time) deadline))
      (sleep-for 0.05))
    (unless (file-exists-p (canvas-browser-cdp--port-file))
      (canvas-browser-cdp--no-port-error))))

(defun canvas-browser-cdp--address ()
  "The websocket address of the browser, once chromium has opened its port."
  (canvas-browser-cdp--wait-for-port)
  (with-temp-buffer
    (insert-file-contents (canvas-browser-cdp--port-file))
    (let ((port (string-trim (buffer-substring (point-min) (line-end-position))))
          (path (string-trim (buffer-substring (line-beginning-position 2) (point-max)))))
      (format "ws://127.0.0.1:%s%s" port path))))

;;;; The commands, their answers and the events

(defvar canvas-browser-cdp--id 0
  "The id of the last command sent.")

(defvar canvas-browser-cdp--waiting (make-hash-table :test #'eql)
  "The function that waits for the answer of each command id.")

(defvar canvas-browser-cdp--listeners (make-hash-table :test #'equal)
  "The functions that wait for an event, keyed by (SESSION . METHOD).")

(defun canvas-browser-cdp-send (method params &optional answer session)
  "Send METHOD with PARAMS to chromium, in SESSION when there is one.
ANSWER, a function of one plist, has the result when it arrives."
  (unless (canvas-browser-cdp-running-p)
    (user-error "canvas-browser: chromium is not connected"))
  (let* ((id (cl-incf canvas-browser-cdp--id))
         (command (append (list :id id :method method)
                          (when params (list :params params))
                          (when session (list :sessionId session)))))
    (when answer (puthash id answer canvas-browser-cdp--waiting))
    (websocket-send-text canvas-browser-cdp--socket (json-encode command))
    id))

(defun canvas-browser-cdp-listen (session method function)
  "Have FUNCTION called with the parameters of METHOD in SESSION."
  (puthash (cons session method) function canvas-browser-cdp--listeners))

(defun canvas-browser-cdp-forget (session)
  "Drop every listener of SESSION."
  (maphash (lambda (key _function)
             (when (equal (car key) session)
               (remhash key canvas-browser-cdp--listeners)))
           canvas-browser-cdp--listeners))

(defun canvas-browser-cdp--receive (text)
  "Take TEXT, one message of chromium, to whoever waits for it."
  (let* ((message (json-parse-string text :object-type 'plist :array-type 'list))
         (id (plist-get message :id)))
    (if id
        (canvas-browser-cdp--answer id message)
      (canvas-browser-cdp--event message))))

(defun canvas-browser-cdp--answer (id message)
  "Give MESSAGE, the answer of the command ID, to the function that waits."
  (let ((answer (gethash id canvas-browser-cdp--waiting))
        (failure (plist-get message :error)))
    (remhash id canvas-browser-cdp--waiting)
    (when failure
      (message "canvas-browser: chromium says: %s" (plist-get failure :message)))
    (when answer
      ;; A refused command answers with nothing, so that a caller which
      ;; put something aside while it waited can put it back.
      (funcall answer (and (not failure) (plist-get message :result))))))

(defun canvas-browser-cdp--event (message)
  "Give MESSAGE, an event, to the listener of its session and method."
  (when-let* ((function (gethash (cons (plist-get message :sessionId)
                                       (plist-get message :method))
                                 canvas-browser-cdp--listeners)))
    (funcall function (plist-get message :params))))

;;;; Starting and stopping

(defun canvas-browser-cdp-running-p ()
  "Whether chromium runs and its websocket is open.
A chromium that has gone leaves a websocket whose connection is no
process, and the websocket library answers such a socket with a type
error rather than with no."
  (and canvas-browser-cdp--socket
       (canvas-browser-cdp--socket-live-p canvas-browser-cdp--socket)))

(defun canvas-browser-cdp--socket-live-p (socket)
  "Whether the connection of SOCKET is a process that lives."
  (let ((process (websocket-conn socket)))
    (and (processp process) (process-live-p process) t)))

(defcustom canvas-browser-headless nil
  "Whether chromium runs without a window of its own.
A headless chromium says so in its user agent, keeps `navigator.webdriver\='
true and has no WebGL at all, and a site behind a bot check reads all
three: it then asks the reader to pick out traffic lights rather than to
tick a box.  A chromium with a window reads as the browser it is.  Set
this when there is no X server to give it one."
  :type 'boolean
  :group 'canvas-browser)

(defcustom canvas-browser-display ":98"
  "The X display chromium draws its window on.
A display of its own is where a window nobody looks at belongs.  The page
is read through the screencast, so nothing of it travels to your own
display, which matters when that display is forwarded over ssh.  `Xvfb\='
is started on this one when nothing listens there yet."
  :type 'string
  :group 'canvas-browser)

(defconst canvas-browser-cdp--screen "2560x1440x24"
  "The screen `Xvfb\=' is given, wide enough for any window of a page.")

(defconst canvas-browser-cdp--window-size "1920,1200"
  "The window chromium opens, which the page layout then overrides.")

(defun canvas-browser-cdp--display-socket (display)
  "The file an X server listens on for DISPLAY.
One socket serves every screen of a display, so the screen after the dot
is left off."
  (let ((number (car (split-string (string-remove-prefix ":" display) "\\."))))
    (format "/tmp/.X11-unix/X%s" number)))

(defun canvas-browser-cdp--display-live-p (display)
  "Whether an X server listens on DISPLAY."
  (file-exists-p (canvas-browser-cdp--display-socket display)))

(defun canvas-browser-cdp--wait-for-display ()
  "Wait until `canvas-browser-display\=' answers, or signal an error."
  (let ((deadline (+ (float-time) canvas-browser-cdp-timeout)))
    (while (and (not (canvas-browser-cdp--display-live-p canvas-browser-display))
                (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (unless (canvas-browser-cdp--display-live-p canvas-browser-display)
      (error "canvas-browser: Xvfb did not open %s in %s seconds"
             canvas-browser-display canvas-browser-cdp-timeout))))

(defun canvas-browser-cdp--ensure-display ()
  "Have an X server on `canvas-browser-display\=', starting `Xvfb\=' if need be.
The display listens on its socket alone, never on the network."
  (unless (canvas-browser-cdp--display-live-p canvas-browser-display)
    (unless (executable-find "Xvfb")
      (user-error (concat "canvas-browser: no Xvfb; run `sudo apt install xvfb',"
                          " or set `canvas-browser-headless'")))
    (make-process :name "canvas-browser-xvfb"
                  :buffer (get-buffer-create " *canvas-browser-xvfb*")
                  :noquery t
                  :command (list "Xvfb" canvas-browser-display
                                 "-screen" "0" canvas-browser-cdp--screen
                                 "-nolisten" "tcp" "-ac"))
    (canvas-browser-cdp--wait-for-display)))

(defun canvas-browser-cdp--environment ()
  "The environment chromium is started in."
  (if canvas-browser-headless
      process-environment
    (cons (format "DISPLAY=%s" canvas-browser-display) process-environment)))

(defun canvas-browser-cdp--command-line ()
  "The command line of chromium, with a window of its own or without one."
  (append (list (canvas-browser-cdp--executable)
                "--remote-debugging-port=0"
                "--remote-allow-origins=*"
                (format "--user-data-dir=%s" (canvas-browser-cdp--profile)))
          (canvas-browser-cdp--extension-flags)
          (if canvas-browser-headless
              (list "--headless=new")
            ;; A window nobody looks at is a window chromium would throttle
            ;; or stop drawing, and a page that stops drawing sends no frames.
            (list "--no-first-run"
                  "--no-default-browser-check"
                  "--disable-background-timer-throttling"
                  "--disable-backgrounding-occluded-windows"
                  "--disable-renderer-backgrounding"
                  ;; The reader drives this browser by hand, one key at a
                  ;; time, and the flag that tells a page a robot does is
                  ;; wrong about it.
                  "--disable-blink-features=AutomationControlled"
                  ;; An X server of its own has no graphics card, so
                  ;; WebGL is drawn in software rather than not at all,
                  ;; which is also what a page asks it about.  Only WebGL:
                  ;; drawing the whole window that way costs two cores.
                  "--enable-unsafe-swiftshader"
                  (format "--window-size=%s" canvas-browser-cdp--window-size)))))

(defun canvas-browser-cdp--open-p (socket)
  "Whether SOCKET has finished shaking hands."
  (eq (websocket-ready-state socket) 'open))

(defun canvas-browser-cdp--wait-until-open (socket)
  "Wait until SOCKET has finished shaking hands, or signal an error.
A command sent while it connects reaches nobody."
  (let ((deadline (+ (float-time) canvas-browser-cdp-timeout)))
    (while (and (not (canvas-browser-cdp--open-p socket))
                (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (unless (canvas-browser-cdp--open-p socket)
      (error "canvas-browser: chromium did not answer the websocket in %s seconds"
             canvas-browser-cdp-timeout))
    socket))

(defun canvas-browser-cdp--connect ()
  "Connect to the browser of the chromium that runs, once it answers."
  (canvas-browser-cdp--wait-until-open
   (websocket-open (canvas-browser-cdp--address)
                   :on-message (lambda (_socket frame)
                                 (canvas-browser-cdp--receive (websocket-frame-text frame)))
                   :on-close (lambda (_socket) (setq canvas-browser-cdp--socket nil)))))

(defun canvas-browser-cdp-start ()
  "Start chromium and connect to it, unless that has happened already."
  (unless (canvas-browser-cdp-running-p)
    ;; The command line names the chromium, and a machine without one
    ;; says so before an X server is started for it.
    (let ((command (canvas-browser-cdp--command-line)))
      (unless canvas-browser-headless
        (canvas-browser-cdp--ensure-display))
      (let ((process-environment (canvas-browser-cdp--environment)))
        (make-directory (canvas-browser-cdp--profile) t)
        (ignore-errors (delete-file (canvas-browser-cdp--port-file)))
        (setq canvas-browser-cdp--process
              (make-process :name "canvas-browser-chromium"
                            :buffer (get-buffer-create " *canvas-browser-chromium*")
                            :noquery t
                            :command command)))
      (setq canvas-browser-cdp--socket (canvas-browser-cdp--connect)))))

(defun canvas-browser-cdp-stop ()
  "Close the websocket and stop chromium."
  (when canvas-browser-cdp--socket
    ;; A socket whose chromium has gone has no process to close, and
    ;; closing it reaches for one.
    (when (canvas-browser-cdp--socket-live-p canvas-browser-cdp--socket)
      (websocket-close canvas-browser-cdp--socket))
    (setq canvas-browser-cdp--socket nil))
  (when canvas-browser-cdp--process
    (delete-process canvas-browser-cdp--process)
    (setq canvas-browser-cdp--process nil))
  (clrhash canvas-browser-cdp--waiting)
  (clrhash canvas-browser-cdp--listeners))

(provide 'canvas-browser-cdp)
;;; canvas-browser-cdp.el ends here
