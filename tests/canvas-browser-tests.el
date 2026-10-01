;;; canvas-browser-tests.el --- tests of the page buffer -*- lexical-binding: t -*-
(require 'ert)
(require 'cl-lib)
(require 'canvas-browser)

(defvar smear-cursor-mode)

(defvar canvas-browser-test--commands nil
  "The commands the page buffer sent, as (METHOD . PARAMS), newest first.")

(defvar canvas-browser-test--listeners nil
  "The listeners the page buffer left with chromium, as ((SESSION . METHOD) . FUNCTION).")

(defun canvas-browser-test--event (method params)
  "Have chromium send the event METHOD with PARAMS to whoever listens."
  (funcall (cdr (cl-find method canvas-browser-test--listeners
                         :key #'cdar :test #'equal))
           params))

(defmacro canvas-browser-test--with-chromium (&rest body)
  "Run BODY with chromium stubbed; what it is sent goes to
`canvas-browser-test--commands', and who listens to
`canvas-browser-test--listeners'."
  (declare (indent 0))
  `(let ((canvas-browser-test--commands nil)
         (canvas-browser-test--listeners nil))
     (cl-letf (((symbol-function 'canvas-browser-cdp-start) #'ignore)
               ((symbol-function 'canvas-browser-cdp-running-p) (lambda () t))
               ((symbol-function 'canvas-browser-cdp-listen)
                (lambda (session method function)
                  (push (cons (cons session method) function) canvas-browser-test--listeners)))
               ((symbol-function 'canvas-browser-cdp-forget) #'ignore)
               ((symbol-function 'canvas-browser-cdp-send)
                (lambda (method params &optional answer _session)
                  (push (cons method params) canvas-browser-test--commands)
                  (when answer (funcall answer '(:targetId "T1" :sessionId "S1"))))))
       ,@body)))

(defmacro canvas-browser-test--in-page (&rest body)
  "Run BODY in a page buffer whose chromium is stubbed."
  (declare (indent 0))
  `(canvas-browser-test--with-chromium
     (with-temp-buffer
       (canvas-browser-mode)
       (canvas-browser--open "https://example.org" 800 600)
       ,@body)))

(defun canvas-browser-test--frame (&rest params)
  "Take a frame of PARAMS in this page buffer, and paint it at once.
A page paints the newest frame when Emacs next has a moment; a test has
that moment now."
  (canvas-browser--frame params)
  (canvas-browser--paint-pending (current-buffer)))

(defun canvas-browser-test--params (method)
  "The parameters of the last METHOD that was sent."
  (cdr (cl-find method canvas-browser-test--commands :key #'car :test #'equal)))

;;;; The page and its frames

(ert-deftest canvas-browser-opening-a-page-sizes-it-and-starts-the-screencast ()
  ;; GIVEN a page buffer of 800 by 600 pixels
  ;; WHEN it opens a URL
  ;; THEN chromium lays the page out at that size, the page is enabled,
  ;;      the URL is navigated to, AND the screencast starts as jpeg
  (canvas-browser-test--in-page
    (let ((metrics (canvas-browser-test--params "Emulation.setDeviceMetricsOverride"))
          (screencast (canvas-browser-test--params "Page.startScreencast")))
      (should (equal (plist-get metrics :width) 800))
      (should (equal (plist-get metrics :height) 600))
      (should (assoc "Page.enable" canvas-browser-test--commands))
      (should (equal (plist-get (canvas-browser-test--params "Page.navigate") :url)
                     "https://example.org"))
      (should (equal (plist-get screencast :format) "jpeg"))
      (should (equal (plist-get screencast :maxWidth) 800)))))

(ert-deftest canvas-browser-a-frame-is-painted-and-acknowledged ()
  ;; GIVEN a page buffer
  ;; WHEN a screencast frame arrives
  ;; THEN its picture is painted on the canvas, AND the frame is
  ;;      acknowledged, so that chromium sends the next one
  (canvas-browser-test--in-page
    (let ((painted nil))
      (cl-letf (((symbol-function 'canvas-cairo-image)
                 (lambda (_ctx file _x _y _w _h) (setq painted file)))
                ((symbol-function 'canvas-refresh) #'ignore))
        (canvas-browser-test--frame :data (base64-encode-string "not a picture")
                                    :sessionId "S1")
        (should painted)
        (should (file-exists-p painted))
        (should (equal (plist-get (canvas-browser-test--params "Page.screencastFrameAck")
                                  :sessionId)
                       "S1"))))))


(ert-deftest canvas-browser-counts-the-frames-it-painted ()
  ;; GIVEN a page buffer that has painted nothing
  ;; WHEN two frames arrive
  ;; THEN the count of frames follows them, so that a test or a reader can
  ;;      tell whether a page has drawn anything
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'canvas-cairo-image) #'ignore)
              ((symbol-function 'canvas-refresh) #'ignore))
      (should (= canvas-browser--frames 0))
      (canvas-browser-test--frame :data (base64-encode-string "one") :sessionId "S1")
      (canvas-browser-test--frame :data (base64-encode-string "two") :sessionId "S1")
      (should (= canvas-browser--frames 2)))))

(ert-deftest canvas-browser-killing-the-buffer-closes-its-target ()
  ;; GIVEN a page buffer
  ;; WHEN the buffer goes away
  ;; THEN its session is forgotten and its target closed
  (canvas-browser-test--in-page
    (let ((forgotten nil))
      (cl-letf (((symbol-function 'canvas-browser-cdp-forget)
                 (lambda (_session) (setq forgotten t))))
        (canvas-browser--release))
      (should forgotten)
      (should (equal (plist-get (canvas-browser-test--params "Target.closeTarget") :targetId)
                     "T1")))))

;;;; The keyboard and the mouse

(ert-deftest canvas-browser-normal-state-keeps-the-keys-of-emacs ()
  ;; GIVEN a page buffer in normal state
  ;; WHEN the keys are looked up
  ;; THEN they run the commands of the package, not the page
  (canvas-browser-test--in-page
    (should (eq (key-binding (kbd "g")) #'revert-buffer))
    (should (eq (key-binding (kbd "M-p")) #'canvas-browser-back))
    (should (eq (key-binding (kbd "M-n")) #'canvas-browser-forward))
    (should (eq (key-binding (kbd "n")) #'canvas-browser-scroll-line-up))
    (should (eq (key-binding (kbd "p")) #'canvas-browser-scroll-line-down))
    (should (eq (key-binding (kbd "C-s")) #'canvas-browser-find))
    (should (eq (key-binding (kbd "M-s M-l")) #'canvas-browser-search-text))
    ;; Every key of this map names a command that exists.
    (should (cl-every #'commandp
                      (list #'canvas-browser-search-text #'canvas-browser-text
                            #'canvas-browser-find #'canvas-browser-find-previous
                            #'canvas-browser-hints #'canvas-browser-toggle-dark
                            #'canvas-browser-open-in-eww #'canvas-browser-back
                            #'canvas-browser-forward #'canvas-browser-open-url
                            #'canvas-browser-scroll-line-up #'canvas-browser-scroll-line-down)))
    (should (eq (key-binding (kbd "o")) #'canvas-browser-open-url))
    (should (eq (key-binding (kbd "i")) #'canvas-browser-insert-mode))))

(ert-deftest canvas-browser-insert-state-sends-every-key-to-the-page ()
  ;; GIVEN a page buffer that entered insert state
  ;; WHEN a letter and a return are typed, and then ESC
  ;; THEN the letter goes as text, the return as a key, AND ESC leaves
  ;;      insert state
  (canvas-browser-test--in-page
    (canvas-browser-insert-mode)
    (should canvas-browser--insert)
    (should (eq (key-binding (kbd "a")) #'canvas-browser-self-insert))
    (let ((last-command-event ?a))
      (canvas-browser-self-insert))
    (let ((down (car (canvas-browser-test--keys-sent))))
      (should (equal (plist-get down :type) "keyDown"))
      (should (equal (plist-get down :text) "a")))
    (let ((last-command-event ?\r))
      (canvas-browser-send-key))
    (should (equal (plist-get (canvas-browser-test--params "Input.dispatchKeyEvent") :key)
                   "Enter"))
    (should (eq (key-binding (kbd "<escape>")) #'canvas-browser-normal-mode))
    (canvas-browser-normal-mode)
    (should-not canvas-browser--insert)))

(ert-deftest canvas-browser-scrolling-moves-the-page-a-screen ()
  ;; GIVEN a page buffer in normal state, 800 by 600
  ;; WHEN the page is scrolled down and then up
  ;; THEN the page scrolls itself a screen each way
  (canvas-browser-test--in-page
    (canvas-browser-scroll-up)
    (should (string-search "window.scrollBy(0, 540)"
                           (plist-get (canvas-browser-test--params "Runtime.evaluate")
                                      :expression)))
    (canvas-browser-scroll-down)
    (should (string-search "window.scrollBy(0, -540)"
                           (plist-get (canvas-browser-test--params "Runtime.evaluate")
                                      :expression)))))

(ert-deftest canvas-browser-a-click-reaches-the-page-at-its-pixel ()
  ;; GIVEN a page buffer
  ;; WHEN a click at the pixel 40 by 90 is handled
  ;; THEN a press and a release go to the page at that pixel
  (canvas-browser-test--in-page
    (canvas-browser--click 40 90)
    (let ((events (cl-remove "Input.dispatchMouseEvent" canvas-browser-test--commands
                             :key #'car :test-not #'equal)))
      (should (equal (mapcar (lambda (event) (plist-get (cdr event) :type)) events)
                     '("mouseReleased" "mousePressed")))
      (should (equal (plist-get (cdr (car events)) :x) 40))
      (should (equal (plist-get (cdr (car events)) :y) 90)))))

;;;; Link hints

(ert-deftest canvas-browser-there-is-a-hint-for-every-box ()
  ;; GIVEN more boxes than two letters of the hint keys can name
  ;; WHEN their hints are made
  ;; THEN every box still has one, of three letters, and no two are equal:
  ;;      a box past the last two-letter hint was dropped without a word
  (let* ((keys (length canvas-browser-hint-keys))
         (count (1+ (* keys keys)))
         (hints (canvas-browser--hint-letters count)))
    (should (= (length hints) count))
    (should (cl-every (lambda (hint) (= (length hint) 3)) hints))
    (should (= (length (delete-dups (copy-sequence hints))) count))))

(ert-deftest canvas-browser-hint-letters-are-short-and-different ()
  ;; GIVEN three boxes to label, and then thirty
  ;; WHEN their letters are made
  ;; THEN three take one letter each, thirty take two, AND no two are equal
  (let ((few (canvas-browser--hint-letters 3))
        (many (canvas-browser--hint-letters 30)))
    (should (equal (length few) 3))
    (should (cl-every (lambda (hint) (= (length hint) 1)) few))
    (should (cl-every (lambda (hint) (= (length hint) 2)) many))
    (should (equal (length (delete-dups (copy-sequence many))) 30))))

(ert-deftest canvas-browser-hints-click-the-box-of-the-letters-typed ()
  ;; GIVEN a page that answers with two boxes
  ;; WHEN the hints are shown and the second one is chosen
  ;; THEN the page is clicked in the middle of that second box
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'canvas-browser--boxes)
               (lambda (answer) (funcall answer '((:x 0 :y 0 :w 10 :h 10)
                                                  (:x 100 :y 50 :w 20 :h 10)))))
              ((symbol-function 'canvas-browser--draw-hints) #'ignore)
              ((symbol-function 'canvas-browser--read-hint) (lambda (&rest _) (list 1))))
      (canvas-browser-hints)
      (let ((event (canvas-browser-test--params "Input.dispatchMouseEvent")))
        (should (equal (plist-get event :x) 110))
        (should (equal (plist-get event :y) 55))))))

(ert-deftest canvas-browser-hints-say-when-a-page-has-nothing-to-click ()
  ;; GIVEN a page that answers with no boxes
  ;; WHEN the hints are asked for
  ;; THEN the reader is told, and the page is not clicked
  (canvas-browser-test--in-page
    (let ((said nil))
      (cl-letf (((symbol-function 'canvas-browser--boxes)
                 (lambda (answer) (funcall answer nil)))
                ((symbol-function 'message)
                 (lambda (format &rest args) (setq said (apply #'format format args)))))
        (canvas-browser-hints)
        (should (string-search "nothing to click" said))
        (should-not (canvas-browser-test--params "Input.dispatchMouseEvent"))))))

;;;; Find in page, and the text of a page

(ert-deftest canvas-browser-text-opens-the-page-as-a-buffer ()
  ;; GIVEN a page whose text is two lines
  ;; WHEN the text is asked for
  ;; THEN a buffer of its own holds that text, with point at its start
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'canvas-browser-cdp-send)
               (lambda (_method _params &optional answer _session)
                 (when answer (funcall answer '(:result (:value "one\ntwo"))))))
              ((symbol-function 'display-buffer) #'ignore))
      (canvas-browser-text))
    (let ((text-buffer (canvas-browser--text-buffer)))
      (unwind-protect
          (with-current-buffer text-buffer
            (should (equal (buffer-string) "one\ntwo"))
            (should (= (point) (point-min))))
        (kill-buffer text-buffer)))))

;;;; The window, the header line and a chromium that died

(ert-deftest canvas-browser-a-resized-window-lays-the-page-out-again ()
  ;; GIVEN a page buffer of 800 by 600
  ;; WHEN its window becomes 900 by 700
  ;; THEN the page is laid out for the new size, and the screencast asks
  ;;      for frames of it
  (canvas-browser-test--in-page
    (canvas-browser--window-resized 900 700)
    (should (equal (plist-get (canvas-browser-test--params "Emulation.setDeviceMetricsOverride")
                              :width)
                   900))
    (should (equal (plist-get (canvas-browser-test--params "Page.startScreencast") :maxHeight)
                   700))
    (should (equal canvas-browser--size '(900 . 700)))))

(ert-deftest canvas-browser-the-header-line-names-the-page ()
  ;; GIVEN a page buffer with a title and a URL
  ;; WHEN the header line is made, in normal state and then in insert state
  ;; THEN it holds the title and the URL, and says when the keys go to the page
  (canvas-browser-test--in-page
    (setq canvas-browser--title "Example"
          canvas-browser--url "https://example.org")
    (should (string-search "Example" (canvas-browser--header)))
    (should (string-search "https://example.org" (canvas-browser--header)))
    (should-not (string-search "insert" (canvas-browser--header)))
    (setq canvas-browser--insert t)
    (should (string-search "insert" (canvas-browser--header)))))

(ert-deftest canvas-browser-a-command-after-chromium-died-starts-it-again ()
  ;; GIVEN a page buffer whose chromium is gone
  ;; WHEN a command is sent
  ;; THEN chromium is started again, and the reader is told
  (canvas-browser-test--in-page
    (let ((started nil) (said nil))
      (cl-letf (((symbol-function 'canvas-browser-cdp-running-p) (lambda () nil))
                ((symbol-function 'canvas-browser-cdp-start) (lambda () (setq started t)))
                ((symbol-function 'message)
                 (lambda (format &rest args) (setq said (apply #'format format args)))))
        (canvas-browser-refresh)
        (should started)
        (should (string-search "chromium" said))))))

(ert-deftest canvas-browser-a-frame-that-cannot-be-read-keeps-the-last-one ()
  ;; GIVEN a page buffer and a frame that the picture reader refuses
  ;; WHEN it is painted
  ;; THEN the error reaches the echo area, and nothing is signalled
  (canvas-browser-test--in-page
    (let ((said nil))
      (cl-letf (((symbol-function 'canvas-cairo-image)
                 (lambda (&rest _) (error "canvas-cairo: cannot read the image")))
                ((symbol-function 'message)
                 (lambda (format &rest args) (setq said (apply #'format format args)))))
        (canvas-browser-test--frame :data (base64-encode-string "rubbish") :sessionId "S1")
        (should (string-search "cannot read" said))))))

(ert-deftest canvas-browser-the-size-of-the-page-is-sent-as-json-chromium-reads ()
  ;; GIVEN a page buffer
  ;; WHEN the parameters of its size are encoded as chromium gets them
  ;; THEN mobile is the JSON false, not a string, which chromium refuses
  (canvas-browser-test--in-page
    (let ((json (json-encode (canvas-browser-test--params
                              "Emulation.setDeviceMetricsOverride"))))
      (should (string-search "\"mobile\":false" json)))))

;;;; What you type in the URL prompt

(ert-deftest canvas-browser-a-url-without-a-scheme-gets-one ()
  ;; GIVEN a host, a host with a scheme, a file URL, a local address and
  ;;       words with a space in them
  ;; WHEN each is made into a URL
  ;; THEN the host gets https, the scheme and the file URL stay as they
  ;;      are, the local address gets http, AND the words become a search
  (let ((canvas-browser-search-url "https://duckduckgo.com/?q=%s"))
    (should (equal (canvas-browser--url-of "www.vg.no") "https://www.vg.no"))
    (should (equal (canvas-browser--url-of "https://x.org/a") "https://x.org/a"))
    (should (equal (canvas-browser--url-of "file:///tmp/a.html") "file:///tmp/a.html"))
    (should (equal (canvas-browser--url-of "localhost:8080/x") "http://localhost:8080/x"))
    (should (equal (canvas-browser--url-of "127.0.0.1:3000") "http://127.0.0.1:3000"))
    (should (equal (canvas-browser--url-of "what is a canvas")
                   "https://duckduckgo.com/?q=what%20is%20a%20canvas"))))

(ert-deftest canvas-browser-open-url-navigates-to-the-url-it-made ()
  ;; GIVEN a page buffer
  ;; WHEN a host without a scheme is opened
  ;; THEN the page goes to that host with https, and the buffer keeps it
  (canvas-browser-test--in-page
    (canvas-browser-open-url "www.vg.no")
    (should (equal (plist-get (canvas-browser-test--params "Page.navigate") :url)
                   "https://www.vg.no"))
    (should (equal canvas-browser--url "https://www.vg.no"))))

;;;; The pointer over a link

(ert-deftest canvas-browser-hot-spots-name-the-boxes-that-can-be-clicked ()
  ;; GIVEN two boxes of the page
  ;; WHEN the hot spots are made
  ;; THEN each is a rectangle at the pixels of its box, and the pointer
  ;;      over it is a hand
  (let ((spots (canvas-browser--hot-spots '((:x 0 :y 10 :w 30 :h 12)
                                            (:x 100 :y 50 :w 20 :h 10)))))
    (should (equal (length spots) 2))
    (should (equal (car (nth 0 spots)) '(rect . ((0 . 10) . (30 . 22)))))
    (should (eq (plist-get (nth 2 (nth 1 spots)) 'pointer) 'hand))))

(ert-deftest canvas-browser-the-map-of-the-canvas-changes-only-when-it-must ()
  ;; GIVEN a page buffer whose canvas carries the hot spots of two boxes
  ;; WHEN the same boxes are put on it again, and then other boxes
  ;; THEN the first put changes nothing and flushes nothing, AND the
  ;;      second put changes the map
  (canvas-browser-test--in-page
    (let ((flushed 0)
          (boxes '((:x 0 :y 0 :w 10 :h 10))))
      (cl-letf (((symbol-function 'canvas-browser--flush-image)
                 (lambda () (cl-incf flushed))))
        (canvas-browser--put-spots boxes)
        (should (= flushed 1))
        (let ((map (plist-get (cdr canvas-browser--canvas) :map)))
          (canvas-browser--put-spots boxes)
          (should (= flushed 1))
          (should (eq (plist-get (cdr canvas-browser--canvas) :map) map)))
        (canvas-browser--put-spots '((:x 5 :y 5 :w 10 :h 10)))
        (should (= flushed 2))))))

;;;; The common canvas keys

(ert-deftest canvas-browser-takes-the-common-canvas-keys ()
  ;; GIVEN a page buffer
  ;; WHEN its map and its canvas-keys settings are read
  ;; THEN it overwrites none of the common keys, SPC opens its menu, the
  ;;      zoom keys reach its own zoom, AND g reads the page again
  (canvas-browser-test--in-page
    (should-not (canvas-keys-map-violations canvas-browser-mode-map))
    (should (eq (key-binding (kbd "SPC")) #'canvas-keys-menu))
    (should (eq canvas-keys-menu-command #'canvas-browser-menu))
    (should (eq canvas-keys-zoom-function #'canvas-browser--zoom-by-key))
    (should (eq (key-binding (kbd "+")) #'canvas-keys-zoom-in))
    (should (local-variable-p 'revert-buffer-function))
    (should (eq (key-binding (kbd "q")) #'quit-window))))

(ert-deftest canvas-browser-zoom-lays-the-page-out-smaller-and-scales-it ()
  ;; GIVEN a page buffer of 800 by 600 at its natural size
  ;; WHEN the reader zooms in, and then back to the natural size
  ;; THEN the page is laid out for fewer pixels at a larger scale, and the
  ;;      natural size comes back
  (canvas-browser-test--in-page
    (canvas-browser--zoom-by-key 'in)
    (let ((metrics (canvas-browser-test--params "Emulation.setDeviceMetricsOverride")))
      (should (> (plist-get metrics :deviceScaleFactor) 1))
      (should (< (plist-get metrics :width) 800)))
    (canvas-browser--zoom-by-key 'reset)
    (let ((metrics (canvas-browser-test--params "Emulation.setDeviceMetricsOverride")))
      (should (= (plist-get metrics :deviceScaleFactor) 1))
      (should (= (plist-get metrics :width) 800)))))

(ert-deftest canvas-browser-reverting-the-buffer-reads-the-page-again ()
  ;; GIVEN a page buffer
  ;; WHEN the buffer is reverted, as g does
  ;; THEN the page is read again
  (canvas-browser-test--in-page
    (revert-buffer)
    (should (assoc "Page.reload" canvas-browser-test--commands))))

(ert-deftest canvas-browser-writing-the-picture-keeps-the-last-frame ()
  ;; GIVEN a page buffer that painted a frame
  ;; WHEN the picture is written to a file
  ;; THEN that file holds the bytes of the frame
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'canvas-cairo-image) #'ignore)
              ((symbol-function 'canvas-refresh) #'ignore))
      (canvas-browser-test--frame :data (base64-encode-string "a picture") :sessionId "S1"))
    (let ((file (make-temp-file "canvas-browser-test-" nil ".jpg")))
      (unwind-protect
          (progn
            (canvas-browser-write-picture file)
            (should (equal (with-temp-buffer
                             (set-buffer-multibyte nil)
                             (insert-file-contents-literally file)
                             (buffer-string))
                           "a picture")))
        (delete-file file)))))

;;;; The mouse over a link area

(defun canvas-browser-test--wheel-event (kind x y)
  "A mouse event of KIND over the canvas, at the page pixel X Y."
  (list kind (list (selected-window) 'text (cons x y) 0 nil nil nil nil (cons x y) nil)))

(ert-deftest canvas-browser-the-wheel-scrolls-the-page ()
  ;; GIVEN a page buffer
  ;; WHEN the wheel turns down, and fast enough for a double turn
  ;; THEN each turn scrolls where the pointer is
  (canvas-browser-test--in-page
    (should (eq (key-binding [wheel-down]) #'canvas-browser-wheel))
    (should (eq (key-binding [double-wheel-down]) #'canvas-browser-wheel))
    (should (eq (key-binding [triple-wheel-up]) #'canvas-browser-wheel))))

(ert-deftest canvas-browser-the-wheel-scrolls-what-is-under-the-pointer ()
  ;; GIVEN a page buffer, and the pointer over the pixel 120 by 340
  ;; WHEN the wheel turns down there, and then up somewhere else
  ;; THEN a wheel event goes to the page at that very pixel, further down
  ;;      and then back, so that chromium scrolls whatever lies under it,
  ;;      as it does in a window of its own
  (canvas-browser-test--in-page
    (canvas-browser-wheel (canvas-browser-test--wheel-event 'wheel-down 120 340))
    (let ((event (canvas-browser-test--params "Input.dispatchMouseEvent")))
      (should (equal (plist-get event :type) "mouseWheel"))
      (should (equal (plist-get event :x) 120))
      (should (equal (plist-get event :y) 340))
      (should (> (plist-get event :deltaY) 0)))
    (canvas-browser-wheel (canvas-browser-test--wheel-event 'double-wheel-up 10 20))
    (let ((event (canvas-browser-test--params "Input.dispatchMouseEvent")))
      (should (equal (plist-get event :x) 10))
      (should (< (plist-get event :deltaY) 0)))))

(ert-deftest canvas-browser-the-wheel-command-takes-only-a-turn-of-the-wheel ()
  ;; GIVEN a page buffer
  ;; WHEN the wheel command is handed a click instead of a turn
  ;; THEN it says so at once, rather than scrolling by a guess
  (canvas-browser-test--in-page
    (let ((raised (should-error
                   (canvas-browser-wheel (canvas-browser-test--wheel-event 'mouse-1 5 5)))))
      (should (string-search "not a turn of the wheel" (cadr raised))))))

(ert-deftest canvas-browser-the-mouse-over-a-link-area-reaches-the-page ()
  ;; GIVEN a page buffer, where the areas of the image map carry the id
  ;;       canvas-browser-link before each mouse event
  ;; WHEN a click and a wheel turn happen over such an area
  ;; THEN they run the same commands as over the rest of the page, AND an
  ;;      event that nothing binds is ignored instead of saying it is undefined
  (canvas-browser-test--in-page
    (should (eq (key-binding [canvas-browser-link mouse-1]) #'canvas-browser-click))
    (should (eq (key-binding [canvas-browser-link down-mouse-1]) #'ignore))
    (should (eq (key-binding [canvas-browser-link wheel-down]) #'canvas-browser-wheel))
    (should (eq (key-binding [canvas-browser-link triple-wheel-up]) #'canvas-browser-wheel))
    ;; A right click keeps the canvas menu, as everywhere else in a canvas
    ;; buffer, and an event that nothing binds is ignored.
    (should (eq (key-binding [canvas-browser-link mouse-3]) #'canvas-keys-menu))
    ;; `key-binding' gives the default binding only when it is asked to.
    (should (eq (key-binding [canvas-browser-link mouse-2] t) #'ignore))))

(ert-deftest canvas-browser-a-second-screencast-stops-the-first ()
  ;; GIVEN a page buffer whose screencast runs
  ;; WHEN the window is resized, which asks for frames of the new size
  ;; THEN the screencast is stopped before it starts again, because
  ;;      chromium refuses a second one
  (canvas-browser-test--in-page
    (setq canvas-browser-test--commands nil)
    (canvas-browser--window-resized 900 700)
    (let ((methods (mapcar #'car canvas-browser-test--commands)))
      (should (member "Page.stopScreencast" methods))
      (should (< (cl-position "Page.startScreencast" methods :test #'equal)
                 (cl-position "Page.stopScreencast" methods :test #'equal))))))

(ert-deftest canvas-browser-e-opens-the-page-in-eww ()
  ;; GIVEN a page buffer at a URL
  ;; WHEN e is pressed
  ;; THEN eww opens that same URL
  (canvas-browser-test--in-page
    (let ((opened nil))
      (cl-letf (((symbol-function 'eww) (lambda (url &rest _) (setq opened url))))
        (should (eq (key-binding (kbd "e")) #'canvas-browser-open-in-eww))
        (canvas-browser-open-in-eww)
        (should (equal opened "https://example.org"))))))

(defun canvas-browser-test--menu-rows ()
  "The rows of the page menu, each the list of its column headings."
  (mapcar (lambda (row)
            (if (eq (aref row 0) 'transient-columns)
                (mapcar (lambda (column) (plist-get (aref column 1) :description))
                        (aref row 2))
              (list (plist-get (aref row 1) :description))))
          (aref (get 'canvas-browser-menu 'transient--layout) 2)))

(defun canvas-browser-test--menu-suffixes ()
  "Every entry of the page menu, as the plist of its suffix."
  (cl-loop for row in (aref (get 'canvas-browser-menu 'transient--layout) 2)
           append (cl-loop for column in (if (eq (aref row 0) 'transient-columns)
                                             (aref row 2)
                                           (list row))
                           append (mapcar #'cdr (aref column 2)))))

(defun canvas-browser-test--menu-entry (key)
  "The plist of the entry of the page menu on KEY."
  (seq-find (lambda (suffix) (equal (plist-get suffix :key) key))
            (canvas-browser-test--menu-suffixes)))

(ert-deftest canvas-browser-the-menu-carries-the-common-canvas-group ()
  ;; GIVEN the menu of a page buffer
  ;; WHEN its rows are read
  ;; THEN the page's own columns stand side by side in the first row, and
  ;;      the columns canvas-keys gives every canvas menu make the last
  (let ((rows (canvas-browser-test--menu-rows)))
    (should (equal (car rows) '("Go" "Page" "Modes")))
    (should (equal (car (last rows)) '("Zoom" "Canvas" "Settings")))))

(ert-deftest canvas-browser-the-columns-of-the-menu-line-up ()
  ;; GIVEN the menu of a page buffer, two rows of three columns
  ;; WHEN its widths are read
  ;; THEN each of the three columns has a width, so the columns of the
  ;;      page and those under them start at the same place
  (should (= (length (oref (get 'canvas-browser-menu 'transient--prefix) column-widths))
             3)))

(ert-deftest canvas-browser-the-menu-says-each-thing-in-a-word ()
  ;; GIVEN the menu of a page buffer
  ;; WHEN the words of its entries are read, the settings as they stand
  ;; THEN each is one word: a menu is read at a glance
  (canvas-browser-test--in-page
    (dolist (suffix (canvas-browser-test--menu-suffixes))
      (let* ((description (plist-get suffix :description))
             (text (string-trim (if (functionp description) (funcall description) description))))
        ;; A setting reads as its word and its value, with a star before
        ;; it when it has changed.
        (should (string-match-p "\\`\\*? *[^ ]+\\( +[^ ]+\\)?\\'" text))))))

(ert-deftest canvas-browser-has-no-whole-drawing-to-fit ()
  ;; GIVEN a page buffer, where z asks to fit the whole drawing
  ;; WHEN the fit is asked for
  ;; THEN z reaches the zoom of canvas-keys, AND the page says that it has
  ;;      nothing whole to fit, since a page has no end
  (canvas-browser-test--in-page
    (should (eq (key-binding (kbd "z")) #'canvas-keys-zoom-fit))
    (should (string-search "fit" (error-message-string
                                  (should-error (canvas-keys-zoom-fit) :type 'user-error))))))

;;;; Dark mode

(defun canvas-browser-test--navigated (type &optional parent)
  "Have chromium say the page navigated, by TYPE, in a frame with PARENT or none."
  (canvas-browser-test--event "Page.frameNavigated"
                              (list :type type
                                    :frame (append (list :id "F2" :url "https://example.org")
                                                   (and parent (list :parentId parent))))))

(ert-deftest canvas-browser-dark-mode-comes-back-with-a-page-from-the-cache ()
  ;; GIVEN a page shown dark
  ;; WHEN chromium restores the page before it from its back and forward
  ;;      cache, and later navigates to a new page, and a frame inside a
  ;;      page is restored
  ;; THEN the dark is told again after the restore, since chromium drops
  ;;      the dark it forces on a page then, AND not after the others,
  ;;      which keep it
  (canvas-browser-test--in-page
    (let ((canvas-browser-dark t))
      (setq canvas-browser-test--commands nil)
      (canvas-browser-test--navigated "BackForwardCacheRestore")
      (should (eq t (plist-get (canvas-browser-test--params "Emulation.setAutoDarkModeOverride")
                               :enabled)))
      (setq canvas-browser-test--commands nil)
      (canvas-browser-test--navigated "Navigation")
      (canvas-browser-test--navigated "BackForwardCacheRestore" "F1")
      (should-not (assoc "Emulation.setAutoDarkModeOverride" canvas-browser-test--commands)))))

(ert-deftest canvas-browser-dark-mode-tells-the-page-and-chromium ()
  ;; GIVEN a page buffer that shows the page as it is
  ;; WHEN dark mode is turned on, and then off again
  ;; THEN the page is told that the reader prefers dark, chromium darkens
  ;;      a page that has no dark of its own, AND both go back
  (canvas-browser-test--in-page
    (let ((canvas-browser-dark nil))
    (should-not canvas-browser-dark)
    (canvas-browser-toggle-dark)
    (should canvas-browser-dark)
    (let ((media (canvas-browser-test--params "Emulation.setEmulatedMedia"))
          (auto (canvas-browser-test--params "Emulation.setAutoDarkModeOverride")))
      (should (equal (plist-get (aref (plist-get media :features) 0) :value) "dark"))
      (should (eq (plist-get auto :enabled) t)))
    (canvas-browser-toggle-dark)
    (should-not canvas-browser-dark)
    (let ((media (canvas-browser-test--params "Emulation.setEmulatedMedia"))
          (auto (canvas-browser-test--params "Emulation.setAutoDarkModeOverride")))
      (should (equal (plist-get (aref (plist-get media :features) 0) :value) "light"))
      (should (eq (plist-get auto :enabled) :json-false))))))

;;;; Find in page

(ert-deftest canvas-browser-find-paints-the-hit-and-steps-through-them ()
  ;; GIVEN a page buffer
  ;; WHEN a string is searched for, and then stepped forwards and back
  ;; THEN the page is asked to find and paint that string, and the number
  ;;      of the hit follows the steps, since window.find leaves nothing
  ;;      to see in a headless chromium
  (canvas-browser-test--in-page
    (canvas-browser-find "parser")
    (should (string-search "\"parser\"" (plist-get (canvas-browser-test--params "Runtime.evaluate")
                                                   :expression)))
    (should (equal canvas-browser--find-index 0))
    (canvas-browser-find-next)
    (should (equal canvas-browser--find-index 1))
    (canvas-browser-find-previous)
    (canvas-browser-find-previous)
    (should (equal canvas-browser--find-index -1))))

(ert-deftest canvas-browser-find-says-when-a-page-holds-nothing ()
  ;; GIVEN a page that answers with no hits
  ;; WHEN a string is searched for
  ;; THEN the reader is told that the page holds it nowhere
  (canvas-browser-test--in-page
    (let ((said nil))
      (cl-letf (((symbol-function 'canvas-browser-cdp-send)
                 (lambda (_method _params &optional answer _session)
                   (when answer (funcall answer '(:result (:value (:count 0 :index 0)))))))
                ((symbol-function 'message)
                 (lambda (format &rest args) (setq said (apply #'format format args)))))
        (canvas-browser-find "nothing here")
        (should (string-search "no " said))))))

;;;; The map of canvas-minimap


;;;; The whole page for the map


(ert-deftest canvas-browser-a-frame-leaves-the-map-alone-once-the-page-is-on-it ()
  ;; GIVEN a page buffer whose map shows a picture of the whole page
  ;; WHEN a frame arrives and the page has not moved
  ;; THEN the map is not drawn again, since it shows the page, not the frame
  (canvas-browser-test--in-page
    (let ((told 0))
      (cl-letf (((symbol-function 'canvas-cairo-image) #'ignore)
                ((symbol-function 'canvas-refresh) #'ignore)
                ((symbol-function 'canvas-minimap-picture-changed)
                 (lambda (&rest _) (cl-incf told))))
        (setq canvas-browser--page-picture "/nowhere.jpg")
        (canvas-browser--frame (list :data (base64-encode-string "one") :sessionId "S1"))
        (should (= told 0))))))


(ert-deftest canvas-browser-a-capture-that-never-answers-lets-the-page-paint ()
  ;; GIVEN a capture that chromium never answered
  ;; WHEN a frame arrives long afterwards
  ;; THEN it is painted, so a lost answer cannot freeze the window
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'canvas-cairo-image) #'ignore)
              ((symbol-function 'canvas-refresh) #'ignore))
      (setq canvas-browser--capturing (- (float-time) (* 2 canvas-browser-cdp-timeout)))
      (canvas-browser-test--frame :data (base64-encode-string "one") :sessionId "S1")
      (should (= canvas-browser--frames 1)))))


(ert-deftest canvas-browser-a-burst-of-frames-paints-only-the-last ()
  ;; GIVEN three frames of a smooth scroll, arriving before Emacs has a
  ;;       moment to draw
  ;; WHEN they are taken
  ;; THEN one picture is painted, the newest: painting each of them is
  ;;      what makes scrolling crawl
  (canvas-browser-test--in-page
    (let ((painted nil))
      (cl-letf (((symbol-function 'canvas-cairo-image)
                 (lambda (_ctx file &rest _) (setq painted file)))
                ((symbol-function 'canvas-refresh) #'ignore))
        (dolist (word '("one" "two" "three"))
          (canvas-browser--frame (list :data (base64-encode-string word) :sessionId "S1")))
        (should (= canvas-browser--frames 0))
        (canvas-browser--paint-pending (current-buffer))
        (should (= canvas-browser--frames 1))
        (should (equal (with-temp-buffer
                         (set-buffer-multibyte nil)
                         (insert-file-contents-literally painted)
                         (buffer-string))
                       "three"))))))

(ert-deftest canvas-browser-the-buffer-end-keys-go-to-the-ends-of-the-page ()
  ;; GIVEN a page buffer
  ;; WHEN the keys that go to the ends of a buffer are looked up
  ;; THEN they go to the ends of the page, as they do in every other
  ;;      buffer: C-<home> and M-< to the top, C-<end> and M-> to the foot
  (canvas-browser-test--in-page
    (dolist (key '("C-<home>" "M-<"))
      (ert-info (key :prefix "Key: ")
        (should (eq (key-binding (kbd key)) 'canvas-browser-beginning-of-page))))
    (dolist (key '("C-<end>" "M->"))
      (ert-info (key :prefix "Key: ")
        (should (eq (key-binding (kbd key)) 'canvas-browser-end-of-page))))))

(ert-deftest canvas-browser-the-top-of-the-page-is-the-top-of-the-page ()
  ;; GIVEN a page scrolled down
  ;; WHEN the page is taken to its top
  ;; THEN the page itself scrolls there, not the buffer
  (canvas-browser-test--in-page
    (call-interactively #'canvas-browser-beginning-of-page)
    (should (string-search "window.scrollTo(0, 0)"
                           (plist-get (canvas-browser-test--params "Runtime.evaluate")
                                      :expression)))))

(ert-deftest canvas-browser-the-foot-of-the-page-is-the-foot-of-the-page ()
  ;; GIVEN a page
  ;; WHEN the page is taken to its foot
  ;; THEN it scrolls as far as the page goes, which the page itself knows
  (canvas-browser-test--in-page
    (call-interactively #'canvas-browser-end-of-page)
    (should (string-search "scrollHeight"
                           (plist-get (canvas-browser-test--params "Runtime.evaluate")
                                      :expression)))))

(ert-deftest canvas-browser-a-page-keeps-drawing-while-another-is-in-front ()
  ;; GIVEN a page being opened, which chromium puts in a tab of its own
  ;; WHEN the page is attached
  ;; THEN it is told it has the focus: chromium stops drawing a tab that
  ;;      is not in front, and every page buffer wants its own frames
  (canvas-browser-test--in-page
    (should (eq (plist-get (canvas-browser-test--params "Emulation.setFocusEmulationEnabled")
                           :enabled)
                t))))


(ert-deftest canvas-browser-the-page-keys-scroll-a-screen ()
  ;; GIVEN a page buffer
  ;; WHEN the page keys and the keys for the ends of a buffer are looked up
  ;; THEN PageUp and PageDown scroll a screen of the page, and Home and
  ;;      End go to its top and its foot
  (canvas-browser-test--in-page
    (should (eq (key-binding (kbd "<next>")) 'canvas-browser-scroll-up))
    (should (eq (key-binding (kbd "<prior>")) 'canvas-browser-scroll-down))
    (should (eq (key-binding (kbd "<home>")) 'canvas-browser-beginning-of-page))
    (should (eq (key-binding (kbd "<end>")) 'canvas-browser-end-of-page))))

(ert-deftest canvas-browser-a-smooth-scroll-command-scrolls-the-page ()
  ;; GIVEN an Emacs whose page keys run the interpolating scroll of
  ;;       `pixel-scroll-precision-mode', which knows nothing of a page
  ;; WHEN those commands are looked up in a page buffer
  ;; THEN they scroll the page, as every other scroll command does
  (should (eq (keymap-lookup canvas-browser-mode-map
                             "<remap> <pixel-scroll-interpolate-down>")
              'canvas-browser-scroll-up))
  (should (eq (keymap-lookup canvas-browser-mode-map
                             "<remap> <pixel-scroll-interpolate-up>")
              'canvas-browser-scroll-down)))


(ert-deftest canvas-browser-a-click-in-a-text-field-starts-typing ()
  ;; GIVEN a page, and a click that lands in a box you can type in
  ;; WHEN the page says what has the focus
  ;; THEN the keys go to the page, so that you can type where you clicked
  ;;      without pressing a key to say so first
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'canvas-browser-cdp-send)
               (lambda (method params &optional answer _session)
                 (push (cons method params) canvas-browser-test--commands)
                 (when answer
                   (funcall answer '(:result (:value (:typing t :box (10 20 30 40)))))))))
      (canvas-browser--click 10 20)
      (should canvas-browser--insert))))

(ert-deftest canvas-browser-a-click-anywhere-else-stops-typing ()
  ;; GIVEN a page whose keys go to it, and a click that lands on the page
  ;;       itself rather than in a box you can type in
  ;; WHEN the page says what has the focus
  ;; THEN the keys are Emacs's again
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'canvas-browser-cdp-send)
               (lambda (method params &optional answer _session)
                 (push (cons method params) canvas-browser-test--commands)
                 (when answer
                   (funcall answer '(:result (:value (:typing :false :box :null))))))))
      (canvas-browser-insert-mode)
      (canvas-browser--click 10 20)
      (should-not canvas-browser--insert))))

(ert-deftest canvas-browser-there-is-a-way-back-from-typing-besides-escape ()
  ;; GIVEN a page buffer in insert state, in an Emacs where another
  ;;       package has taken `ESC' for itself, as meow does
  ;; WHEN the keys that leave are looked up
  ;; THEN C-g leaves as well, so the keys can always be had back; it
  ;;      drops the mark of a field first, as in a buffer
  (should (eq (keymap-lookup canvas-browser-insert-map "C-g")
              'canvas-browser-insert-quit))
  (should (eq (keymap-lookup canvas-browser-insert-map "<escape>")
              'canvas-browser-normal-mode)))


(ert-deftest canvas-browser-a-page-in-the-background-is-not-left-frozen ()
  ;; GIVEN a page being opened, which chromium puts in a tab behind the
  ;;       one in front and may freeze
  ;; WHEN the page is attached
  ;; THEN it is made active: a frozen page runs no JavaScript, draws
  ;;      nothing and answers nothing, and the buffer would stay blank
  (canvas-browser-test--in-page
    (should (equal (plist-get (canvas-browser-test--params "Page.setWebLifecycleState")
                              :state)
                   "active"))))

(ert-deftest canvas-browser-a-window-of-no-size-still-gets-a-page ()
  ;; GIVEN a window that reports no size yet, as a frame does before the
  ;;       display has laid it out
  ;; WHEN a page is opened in it
  ;; THEN the page is laid out at a size chromium can draw, since a page
  ;;      of no width paints nothing at all and answers "Cannot take
  ;;      screenshot with 0 width"
  (canvas-browser-test--in-page
    (canvas-browser--open "https://example.org" 0 0)
    (let ((metrics (canvas-browser-test--params "Emulation.setDeviceMetricsOverride")))
      (should (>= (plist-get metrics :width) canvas-browser--least-size))
      (should (>= (plist-get metrics :height) canvas-browser--least-size)))))

(ert-deftest canvas-browser-a-page-that-never-stops-painting-is-drawn-at-a-pace ()
  ;; GIVEN a page with an advertisement that repaints without stopping,
  ;;       as a front page has
  ;; WHEN the wait before the next drawing is worked out
  ;; THEN a frame that follows one just drawn waits, and one that follows
  ;;      a quiet moment is drawn at once: Emacs has other work than
  ;;      drawing twenty frames a second
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'a-window)))
      (setq canvas-browser--painted (float-time))
      (should (> (canvas-browser--paint-delay) 0))
      (should (<= (canvas-browser--paint-delay) canvas-browser-frame-interval))
      (setq canvas-browser--painted (- (float-time) 10))
      (should (= (canvas-browser--paint-delay) 0)))))

(ert-deftest canvas-browser-a-page-nobody-looks-at-is-drawn-rarely ()
  ;; GIVEN a page buffer that no window shows, painting all the while
  ;; WHEN the wait before the next drawing is worked out
  ;; THEN it is the long one: drawing a page nobody looks at takes the
  ;;      time of the page they do look at
  (canvas-browser-test--in-page
    (setq canvas-browser--painted (float-time))
    (should (> (canvas-browser--paint-delay) canvas-browser-frame-interval))
    (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'a-window)))
      (should (<= (canvas-browser--paint-delay) canvas-browser-frame-interval)))))


(ert-deftest canvas-browser-a-page-nobody-shows-is-let-be ()
  ;; GIVEN a page buffer that no window shows
  ;; WHEN the windows change
  ;; THEN its frames are stopped: chromium then throttles the page as it
  ;;      throttles any tab behind another, and several news sites at
  ;;      once no longer fight for the machine
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil)))
      (setq canvas-browser-test--commands nil)
      (canvas-browser--follow-windows)
      (should (assoc "Page.stopScreencast" canvas-browser-test--commands))
      (should-not canvas-browser--screencast))))

(ert-deftest canvas-browser-a-page-you-come-back-to-wakes-up ()
  ;; GIVEN a page buffer that was left alone and is shown again
  ;; WHEN the windows change
  ;; THEN the page is told it has the focus, made active and asked for
  ;;      frames again, so it paints where it left off
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'a-window)))
      (canvas-browser--stop-screencast)
      (setq canvas-browser-test--commands nil)
      (cl-letf (((symbol-function 'window-live-p) (lambda (window) (eq window 'a-window)))
                ((symbol-function 'window-buffer) (lambda (&rest _) (current-buffer)))
                ((symbol-function 'window-body-width) (lambda (&rest _) 800))
                ((symbol-function 'window-body-height) (lambda (&rest _) 600)))
        (canvas-browser--follow-windows))
      (should canvas-browser--screencast)
      (should (assoc "Page.startScreencast" canvas-browser-test--commands))
      (should (equal (plist-get (canvas-browser-test--params "Page.setWebLifecycleState") :state)
                     "active"))
      ;; It had the focus before and keeps it, so nothing is sent for it.
      (should canvas-browser--focused))))

(ert-deftest canvas-browser-the-windows-are-followed ()
  ;; GIVEN canvas-browser loaded
  ;; WHEN the hook that runs on a window change is read
  ;; THEN canvas-browser is on it, so a page that comes into view wakes
  ;;      and one that leaves is let be
  (should (memq 'canvas-browser--follow-windows window-configuration-change-hook)))

;;;; The band of the page that the picture covers


(ert-deftest canvas-browser-a-page-follows-the-window-it-is-shown-in ()
  ;; GIVEN a page laid out for 800 by 600, in a window that is now 500 by
  ;;       400, as it is when the map of canvas-minimap takes its side
  ;; WHEN the windows change
  ;; THEN the page is laid out for the window it has: a canvas wider than
  ;;      its window is drawn past the edge, and the rest of it stays on
  ;;      the screen as the window scrolls
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'a-window))
              ((symbol-function 'window-live-p) (lambda (window) (eq window 'a-window)))
              ((symbol-function 'window-body-width) (lambda (&rest _) 500))
              ((symbol-function 'window-body-height) (lambda (&rest _) 400)))
      (canvas-browser--follow-windows)
      (should (equal canvas-browser--size '(500 . 400)))
      (should (= (plist-get (cdr canvas-browser--canvas) :data-width) 500))
      (should (equal (plist-get (canvas-browser-test--params
                                 "Emulation.setDeviceMetricsOverride")
                                :width)
                     500)))))

(defun canvas-browser-test--jpeg (width height)
  "The head of a JPEG that says it is WIDTH by HEIGHT, as bytes."
  (apply #'unibyte-string
         (append '(#xFF #xD8                       ; start of image
                   #xFF #xE0 #x00 #x04 #x00 #x00   ; an application segment
                   #xFF #xC0 #x00 #x11 #x08)       ; the frame, 8 bits a sample
                 (list (ash height -8) (logand height #xFF)
                       (ash width -8) (logand width #xFF))
                 '(0 0 0 0 0 0 #xFF #xD9))))       ; and the end

(defun canvas-browser-test--png (width height)
  "The head of a PNG that says it is WIDTH by HEIGHT, as bytes."
  (apply #'unibyte-string
         (append '(#x89 #x50 #x4E #x47 #x0D #x0A #x1A #x0A   ; the signature
                   0 0 0 #x0D #x49 #x48 #x44 #x52)          ; the IHDR chunk
                 (list (ash width -24) (logand (ash width -16) #xFF)
                       (logand (ash width -8) #xFF) (logand width #xFF)
                       (ash height -24) (logand (ash height -16) #xFF)
                       (logand (ash height -8) #xFF) (logand height #xFF))
                 '(8 2 0 0 0))))

(ert-deftest canvas-browser-the-size-of-a-picture-is-read-from-its-bytes ()
  ;; GIVEN a JPEG and a PNG that both say they are 1322 by 914
  ;; WHEN their size is read
  ;; THEN those are the numbers for either kind, because a moving frame
  ;;      is a JPEG and a still picture is a PNG, AND bytes that are
  ;;      neither say nothing
  (should (equal (canvas-browser--picture-size (canvas-browser-test--jpeg 1322 914))
                 '(1322 . 914)))
  (should (equal (canvas-browser--picture-size (canvas-browser-test--png 1322 914))
                 '(1322 . 914)))
  (should-not (canvas-browser--picture-size (unibyte-string 0 1 2 3 4 5 6 7 8 9))))

(ert-deftest canvas-browser-a-frame-of-another-shape-is-not-painted ()
  ;; GIVEN a page 800 by 600, and a frame that is a sliver 132 by 914, as
  ;;       chromium sends while it draws a picture of the whole page
  ;; WHEN the frame is painted
  ;; THEN nothing is drawn and nothing is kept: stretched over the
  ;;      window, such a frame is the page smeared across it
  (canvas-browser-test--in-page
    (let ((drawn nil))
      (cl-letf (((symbol-function 'canvas-cairo-image)
                 (lambda (&rest _) (setq drawn t)))
                ((symbol-function 'canvas-refresh) #'ignore))
        (canvas-browser--paint (base64-encode-string
                                (canvas-browser-test--jpeg 132 914)))
        (should-not drawn)
        (should-not canvas-browser--last-frame)
        (should (= canvas-browser--frames 0))))))


(ert-deftest canvas-browser-only-the-page-you-look-at-holds-the-focus ()
  ;; GIVEN a page that is shown, and one that is shown elsewhere
  ;; WHEN each is told where it stands
  ;; THEN only the one you look at emulates the focus: several pages
  ;;      claiming it at once leaves chromium sending input to none of
  ;;      them, and the page stops answering the keys and the wheel
  (canvas-browser-test--in-page
    (canvas-browser--awaken t)
    (should (eq (plist-get (canvas-browser-test--params
                            "Emulation.setFocusEmulationEnabled")
                           :enabled)
                t))
    (canvas-browser--awaken nil)
    (should (eq (plist-get (canvas-browser-test--params
                            "Emulation.setFocusEmulationEnabled")
                           :enabled)
                :json-false))))

(ert-deftest canvas-browser-the-window-you-are-in-gives-the-focus ()
  ;; GIVEN a page buffer shown in the window you are in
  ;; WHEN the windows change
  ;; THEN that page holds the focus
  (canvas-browser-test--in-page
    (let ((page (current-buffer)))
      (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'a-window))
                ((symbol-function 'window-live-p) (lambda (window) (eq window 'a-window)))
                ((symbol-function 'window-buffer) (lambda (&rest _) page))
                ((symbol-function 'window-body-width) (lambda (&rest _) 800))
                ((symbol-function 'window-body-height) (lambda (&rest _) 600)))
        (canvas-browser--follow-windows)
        (should (eq (plist-get (canvas-browser-test--params
                                "Emulation.setFocusEmulationEnabled")
                               :enabled)
                    t))))))

(ert-deftest canvas-browser-a-page-keeps-the-focus-while-you-look-elsewhere ()
  ;; GIVEN a page that has the focus, and a window selected that holds
  ;;       something else, as the map of canvas-minimap does while it is
  ;;       being made
  ;; WHEN the windows change
  ;; THEN the page keeps the focus: it would else stop answering the keys
  ;;      and the wheel while you type in it
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'a-window))
              ((symbol-function 'window-live-p) (lambda (window) (eq window 'a-window)))
              ((symbol-function 'window-buffer) (lambda (&rest _) (get-buffer-create "elsewhere")))
              ((symbol-function 'window-body-width) (lambda (&rest _) 800))
              ((symbol-function 'window-body-height) (lambda (&rest _) 600)))
      (canvas-browser--awaken t)
      (setq canvas-browser-test--commands nil)
      (canvas-browser--follow-windows)
      (should canvas-browser--focused)
      (should-not (canvas-browser-test--params "Emulation.setFocusEmulationEnabled")))))

(ert-deftest canvas-browser-the-focus-is-not-sent-twice ()
  ;; GIVEN a page that has been told it has the focus
  ;; WHEN it is told again
  ;; THEN nothing is sent: the windows change often, and chromium has
  ;;      better things to do
  (canvas-browser-test--in-page
    (canvas-browser--awaken t)
    (setq canvas-browser-test--commands nil)
    (canvas-browser--awaken t)
    (should-not (canvas-browser-test--params "Emulation.setFocusEmulationEnabled"))))

(ert-deftest canvas-browser-the-line-keys-scroll-a-line ()
  ;; GIVEN a page buffer
  ;; WHEN the keys that move a line and the keys that move a screen are
  ;;      looked up
  ;; THEN C-n, C-p and the arrows scroll a line, as they move a line in
  ;;      every other buffer, and C-v, M-v and the page keys a screen
  (canvas-browser-test--in-page
    (dolist (key '("C-n" "<down>"))
      (ert-info (key :prefix "Key: ")
        (should (eq (key-binding (kbd key)) 'canvas-browser-scroll-line-up))))
    (dolist (key '("C-p" "<up>"))
      (ert-info (key :prefix "Key: ")
        (should (eq (key-binding (kbd key)) 'canvas-browser-scroll-line-down))))
    (should (eq (key-binding (kbd "C-v")) 'canvas-browser-scroll-up))
    (should (eq (key-binding (kbd "M-v")) 'canvas-browser-scroll-down))
    (should (eq (key-binding (kbd "<next>")) 'canvas-browser-scroll-up))))

(ert-deftest canvas-browser-the-scroll-keys-scroll-the-page-itself ()
  ;; GIVEN a page buffer
  ;; WHEN a line key and a screen key run
  ;; THEN the page is scrolled from the page itself, not by a wheel event:
  ;;      chromium animates a wheel and can swallow a small one, and a key
  ;;      that does nothing for a second reads as a key that does nothing
  (canvas-browser-test--in-page
    (call-interactively #'canvas-browser-scroll-line-up)
    (should (string-search (format "window.scrollBy(0, %d)" canvas-browser-line-height)
                           (plist-get (canvas-browser-test--params "Runtime.evaluate")
                                      :expression)))
    (call-interactively #'canvas-browser-scroll-down)
    (should (string-search "window.scrollBy(0, -" 
                           (plist-get (canvas-browser-test--params "Runtime.evaluate")
                                      :expression)))))

(ert-deftest canvas-browser-a-window-that-changed-size-is-filled-again ()
  ;; GIVEN a page whose window has changed size, as it does when the map
  ;;       of canvas-minimap takes its side
  ;; WHEN the page is laid out for the new window
  ;; THEN a picture of the window is asked for: the canvas is new and
  ;;      empty, and a page that has settled sends no frame of its own,
  ;;      so the window would stay black
  (canvas-browser-test--in-page
    (setq canvas-browser-test--commands nil)
    (canvas-browser--window-resized 500 400)
    (let ((shot (canvas-browser-test--params "Page.captureScreenshot"))
          (order (mapcar #'car canvas-browser-test--commands)))
      (should shot)
      (should-not (plist-get shot :captureBeyondViewport))
      ;; The newest command comes first, so the layout stands behind the
      ;; picture: the picture was asked for once chromium had answered
      ;; for the new size, and is of that size.
      (should (> (seq-position order "Emulation.setDeviceMetricsOverride")
                 (seq-position order "Page.captureScreenshot"))))))

(ert-deftest canvas-browser-the-second-try-is-only-for-a-window-still-empty ()
  ;; GIVEN a page asked for a picture of its window a moment ago
  ;; WHEN it has painted since, and when it has not
  ;; THEN only the page that painted nothing is asked again
  (canvas-browser-test--in-page
    (setq canvas-browser--frames 4
          canvas-browser-test--commands nil)
    (canvas-browser--paint-if-quiet (current-buffer) 4)
    (should (canvas-browser-test--params "Page.captureScreenshot"))
    (setq canvas-browser-test--commands nil)
    (canvas-browser--paint-if-quiet (current-buffer) 3)
    (should-not (canvas-browser-test--params "Page.captureScreenshot"))))

(ert-deftest canvas-browser-a-frame-is-drawn-to-the-canvas-it-has ()
  ;; GIVEN a page whose canvas has just been made for a smaller window,
  ;;       while the size of the page has not caught up
  ;; WHEN a frame is painted
  ;; THEN it is drawn to the size of the canvas itself: drawn to another
  ;;      size, the picture is sheared and repeated across the window
  (canvas-browser-test--in-page
    (let ((box nil))
      (cl-letf (((symbol-function 'canvas-cairo-image)
                 (lambda (_ctx _file _x _y width height) (setq box (cons width height))))
                ((symbol-function 'canvas-refresh) #'ignore))
        (canvas-browser--adopt 500 400)
        (setq canvas-browser--size '(800 . 600))
        (canvas-browser--paint (base64-encode-string (canvas-browser-test--jpeg 500 400)))
        (should (equal box '(500 . 400)))))))


(ert-deftest canvas-browser-a-window-that-paints-nothing-is-filled-again ()
  ;; GIVEN a page shown in a window that has painted nothing since the
  ;;       last look, as it has when a frame was missed or dropped
  ;; WHEN the page is looked over
  ;; THEN a picture of the window is asked for, so that a black window
  ;;      heals itself within a moment
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'a-window)))
      (setq canvas-browser--frames 7
            canvas-browser--fresh-frames 7
            canvas-browser-test--commands nil)
      (canvas-browser--keep-fresh (current-buffer))
      (should (canvas-browser-test--params "Page.captureScreenshot")))))

(ert-deftest canvas-browser-a-window-that-paints-is-left-alone ()
  ;; GIVEN a page that has painted since the last look
  ;; WHEN the page is looked over
  ;; THEN nothing is asked for: the frames are doing the work
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'a-window)))
      (setq canvas-browser--frames 9
            canvas-browser--fresh-frames 7
            canvas-browser-test--commands nil)
      (canvas-browser--keep-fresh (current-buffer))
      (should-not (canvas-browser-test--params "Page.captureScreenshot"))
      (should (= canvas-browser--fresh-frames 9)))))


(ert-deftest canvas-browser-a-page-that-stops-answering-says-so ()
  ;; GIVEN a page that has painted nothing over several looks, as a page
  ;;       whose own scripts have wedged the renderer does
  ;; WHEN it is looked over once more
  ;; THEN the reader is told, and told what to do about it: a window that
  ;;      stays black with nothing said is a window that looks broken
  (canvas-browser-test--in-page
    (let ((said nil))
      (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'a-window))
                ((symbol-function 'message)
                 (lambda (format &rest args) (setq said (apply #'format format args)))))
        (setq canvas-browser--frames 5
              canvas-browser--fresh-frames 5)
        (dotimes (_ canvas-browser--quiet-looks)
          (canvas-browser--keep-fresh (current-buffer)))
        (should (string-search "g" said))
        (should (string-search "answer" said))))))



(ert-deftest canvas-browser-a-page-is-left-out-of-the-map ()
  ;; GIVEN an Emacs with canvas-minimap
  ;; WHEN canvas-browser tells it about pages
  ;; THEN the mode is among the modes the map leaves alone, so no strip
  ;;      opens beside a page: a page is a picture, and the map of it
  ;;      told the reader nothing the window does not
  (let ((before (and (boundp 'canvas-minimap-exclude-modes)
                     canvas-minimap-exclude-modes)))
    (unwind-protect
        (progn
          (set 'canvas-minimap-exclude-modes '(image-mode))
          (canvas-browser--leave-out-of-map)
          (should (memq 'canvas-browser-mode canvas-minimap-exclude-modes))
          (should (memq 'image-mode canvas-minimap-exclude-modes)))
      (set 'canvas-minimap-exclude-modes before))))

(ert-deftest canvas-browser-the-menu-shows-what-the-settings-are ()
  ;; GIVEN a page buffer shown as it is, and then shown dark
  ;; WHEN the menu entry for the dark is read
  ;; THEN it says which way the setting stands, so the menu shows the
  ;;      state rather than only the name of the key
  (canvas-browser-test--in-page
    (let ((description (plist-get (canvas-browser-test--menu-entry "d") :description)))
      (setq canvas-browser-dark nil)
      (should (string-search "off" (funcall description)))
      (setq canvas-browser-dark t)
      (should (string-search "on" (funcall description))))))

(ert-deftest canvas-browser-the-dark-is-a-setting-that-can-be-kept ()
  ;; GIVEN a page shown as it is
  ;; WHEN dark mode is turned on
  ;; THEN the setting of the package holds it, so the menu stars it and
  ;;      `C-x C-s' keeps it for the pages that follow
  (canvas-browser-test--in-page
    (let ((canvas-browser-dark nil))
      (canvas-browser-toggle-dark)
      (should canvas-browser-dark)
      (should (eq (plist-get (canvas-browser-test--params "Emulation.setAutoDarkModeOverride")
                             :enabled)
                  t)))))

(ert-deftest canvas-browser-a-page-opens-the-way-the-setting-says ()
  ;; GIVEN dark mode kept as the setting of the package
  ;; WHEN a page is opened
  ;; THEN it is shown dark from the start, without a key being pressed
  (let ((canvas-browser-dark t))
    (canvas-browser-test--in-page
      (should (eq (plist-get (canvas-browser-test--params "Emulation.setAutoDarkModeOverride")
                             :enabled)
                  t)))))

(ert-deftest canvas-browser-the-windows-are-followed-only-while-chromium-runs ()
  ;; GIVEN page buffers and a chromium that has gone, as it does when the
  ;;       last page is killed
  ;; WHEN the windows change, which happens while buffers are being killed
  ;; THEN nothing is sent and nothing is started again: a window change is
  ;;      no reason to open a browser
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'canvas-browser-cdp-running-p) (lambda () nil))
              ((symbol-function 'canvas-browser--restart)
               (lambda () (error "canvas-browser: started again for a window change"))))
      (setq canvas-browser-test--commands nil)
      (canvas-browser--follow-windows)
      (should-not canvas-browser-test--commands))))

(ert-deftest canvas-browser-the-freshness-timer-waits-for-chromium ()
  ;; GIVEN a page whose chromium has gone, and the timer that looks
  ;;       whether the window is still being painted
  ;; WHEN it fires
  ;; THEN it asks for nothing: a command sent now is a user error, and an
  ;;      error in a timer is printed over whatever the reader is doing
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'a-window))
              ((symbol-function 'canvas-browser-cdp-running-p) (lambda () nil)))
      (setq canvas-browser-test--commands nil)
      (canvas-browser--keep-fresh (current-buffer))
      (should-not canvas-browser-test--commands))))

(ert-deftest canvas-browser-the-mouse-works-while-you-type-in-the-page ()
  ;; GIVEN a page whose keys go to it, as they do after a click in a field
  ;; WHEN the mouse is used over a link
  ;; THEN the click reaches the page: the hot spots of the image put
  ;;      `canvas-browser-link' before the event, and a map without that
  ;;      prefix answers "is undefined"
  (should (eq (keymap-lookup canvas-browser-insert-map "<mouse-1>")
              'canvas-browser-click))
  (should (keymapp (keymap-lookup canvas-browser-insert-map "<canvas-browser-link>")))
  (canvas-browser-test--in-page
    (canvas-browser-insert-mode)
    (should (eq (key-binding (kbd "<canvas-browser-link> <mouse-1>"))
                'canvas-browser-click))
    (should (eq (key-binding (kbd "<canvas-browser-link> <down-mouse-1>"))
                'ignore))))

(ert-deftest canvas-browser-a-page-that-was-let-go-is-noticed ()
  ;; GIVEN a page whose target chromium has let go of, which it says with
  ;;       `Inspector.detached'
  ;; WHEN the event arrives
  ;; THEN the buffer forgets its session, so that nothing else is sent in
  ;;      it: chromium answers every such command with "Not attached to
  ;;      an active page"
  (canvas-browser-test--in-page
    (should canvas-browser--session)
    (canvas-browser--detached '(:reason "target_closed"))
    (should-not canvas-browser--session)))

(ert-deftest canvas-browser-a-page-without-a-session-is-opened-again ()
  ;; GIVEN a page buffer that has been let go of
  ;; WHEN anything is asked of the page
  ;; THEN the page is opened again rather than sent to, since a command
  ;;      without a session reaches no page at all
  (canvas-browser-test--in-page
    (setq canvas-browser--session nil
          canvas-browser-test--commands nil)
    (let ((opened nil))
      (cl-letf (((symbol-function 'canvas-browser--restart)
                 (lambda () (setq opened t))))
        (canvas-browser--tell "Page.reload" nil)
        (should opened)
        (should-not canvas-browser-test--commands)))))

(ert-deftest canvas-browser-the-hints-are-drawn-on-the-canvas ()
  ;; GIVEN a page buffer and one box that can be clicked
  ;; WHEN its hint is drawn
  ;; THEN the canvas carries the hint's own colour where the box is, and
  ;;      nothing is signalled: the text of a hint needs a font, and the
  ;;      module answers a call without one with "Wrong number of
  ;;      arguments"
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'canvas-refresh) #'ignore)
              ((symbol-function 'force-window-update) #'ignore))
      (canvas-browser--draw-hints '((:x 10 :y 20 :w 100 :h 30)) '("a"))
      (should (= (canvas-cairo-pixel canvas-browser--context 12 22)
                 canvas-browser--hint-colour)))))

(ert-deftest canvas-browser-the-page-keeps-the-state-its-mode-was-given ()
  ;; GIVEN an Emacs with meow, where a page buffer is in insert state so
  ;;       that the keys of the page work, and something has put the
  ;;       buffer back into normal state, as `ESC' does
  ;; WHEN the page takes its keys back from the page itself
  ;; THEN the state its mode was given is asked for again, so that SPC
  ;;      opens the menu rather than meow's keypad
  (canvas-browser-test--in-page
    (let ((asked nil))
      (cl-letf (((symbol-function 'meow--switch-state) (lambda (state) (setq asked state))))
        (defvar meow--current-state)
        (defvar meow-mode-state-list)
        (let ((meow--current-state 'normal)
              (meow-mode-state-list '((canvas-browser-mode . insert))))
          (canvas-browser-normal-mode)
          (should (eq asked 'insert)))))))

(ert-deftest canvas-browser-the-state-is-put-right-while-you-read ()
  ;; GIVEN a page buffer that something has put into another modal state,
  ;;       as `ESC' does through meow, and no chromium at all
  ;; WHEN the window is looked over
  ;; THEN the state its mode was given is asked for again: the reader
  ;;      would else find `SPC' taken until the next time they type in
  ;;      the page
  (canvas-browser-test--in-page
    (let ((asked nil))
      (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'a-window))
                ((symbol-function 'canvas-browser-cdp-running-p) (lambda () nil))
                ((symbol-function 'meow--switch-state) (lambda (state) (setq asked state))))
        (defvar meow--current-state)
        (defvar meow-mode-state-list)
        (let ((meow--current-state 'normal)
              (meow-mode-state-list '((canvas-browser-mode . insert))))
          (canvas-browser--keep-fresh (current-buffer))
          (should (eq asked 'insert)))))))

(ert-deftest canvas-browser-a-frame-does-not-wipe-the-hints ()
  ;; GIVEN a page showing its hints, waiting for the letters
  ;; WHEN a frame arrives, as one does whenever the page paints
  ;; THEN it is not drawn: it would paint over the hints, which is why
  ;;      they seemed to disappear as soon as they were shown
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'canvas-cairo-image) #'ignore)
              ((symbol-function 'canvas-refresh) #'ignore))
      (setq canvas-browser--hinting t)
      (canvas-browser-test--frame :data (base64-encode-string
                                         (canvas-browser-test--jpeg 800 600))
                                  :sessionId "S1")
      (should (= canvas-browser--frames 0))
      (setq canvas-browser--hinting nil)
      (canvas-browser-test--frame :data (base64-encode-string
                                         (canvas-browser-test--jpeg 800 600))
                                  :sessionId "S1")
      (should (= canvas-browser--frames 1)))))

(ert-deftest canvas-browser-the-page-is-itself-again-after-the-hints ()
  ;; GIVEN a page that has drawn its hints
  ;; WHEN the reader names one, or gives up
  ;; THEN the hints are gone and the page is asked for a picture of
  ;;      itself, so that what is on the canvas is the page again
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'canvas-browser--boxes)
               (lambda (answer) (funcall answer '((:x 10 :y 10 :w 20 :h 10)))))
              ((symbol-function 'canvas-browser--draw-hints) #'ignore)
              ((symbol-function 'canvas-browser--read-hint) (lambda (&rest _) nil)))
      (setq canvas-browser-test--commands nil)
      (canvas-browser-hints)
      (should-not canvas-browser--hinting)
      (should (canvas-browser-test--params "Page.captureScreenshot")))))

(ert-deftest canvas-browser-the-window-is-left-alone-while-the-hints-are-up ()
  ;; GIVEN a page showing its hints
  ;; WHEN the window is looked over, which happens every two seconds
  ;; THEN no picture is asked for: it would arrive as a frame and paint
  ;;      over the hints
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'a-window)))
      (setq canvas-browser--hinting t
            canvas-browser--frames 4
            canvas-browser--fresh-frames 4
            canvas-browser-test--commands nil)
      (canvas-browser--keep-fresh (current-buffer))
      (should-not canvas-browser-test--commands))))

;;;; The part of the page that scrolls on its own

(ert-deftest canvas-browser-asks-the-page-which-parts-scroll-themselves ()
  ;; GIVEN a page buffer
  ;; WHEN the parts that scroll on their own are asked for
  ;; THEN the script looks for what overflows its own box, keeps those
  ;;      elements in the page so that a later scroll finds them again,
  ;;      AND their boxes come back
  (canvas-browser-test--in-page
    (let ((boxes 'none))
      (cl-letf (((symbol-function 'canvas-browser-cdp-send)
                 (lambda (method params &optional answer _session)
                   (push (cons method params) canvas-browser-test--commands)
                   (when answer
                     (funcall answer '(:result (:value ((:x 1 :y 2 :w 3 :h 4)))))))))
        (canvas-browser--scrollers (lambda (found) (setq boxes found))))
      (let ((sent (canvas-browser-test--params "Runtime.evaluate")))
        (should (string-search "scrollHeight" (plist-get sent :expression)))
        (should (string-search "__canvasBrowserScrollers" (plist-get sent :expression)))
        (should (eq (plist-get sent :returnByValue) t)))
      (should (equal boxes '((:x 1 :y 2 :w 3 :h 4)))))))

(ert-deftest canvas-browser-picking-a-part-sends-the-scroll-keys-to-it ()
  ;; GIVEN a page with two parts that scroll on their own
  ;; WHEN the second one is picked, and then a line is scrolled
  ;; THEN the line goes to that part, by the place it was given, and not
  ;;      to the window
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'canvas-browser--scrollers)
               (lambda (answer) (funcall answer '((:x 0 :y 0 :w 200 :h 400)
                                                  (:x 300 :y 0 :w 200 :h 400)))))
              ((symbol-function 'canvas-browser--draw-hints) #'ignore)
              ;; The whole page takes the first letter, so the second part
              ;; is the third label.
              ((symbol-function 'canvas-browser--read-hint) (lambda (&rest _) (list 2))))
      (canvas-browser-pick-scroller))
    (should (equal canvas-browser--scroller 1))
    (canvas-browser-scroll-line-up)
    (let ((expression (plist-get (canvas-browser-test--params "Runtime.evaluate")
                                 :expression)))
      (should (string-search "__canvasBrowserScrollers" expression))
      (should (string-search (format "(1, 'by', %d)" canvas-browser-line-height) expression))
      (should-not (string-search "window.scrollBy" expression)))))

(ert-deftest canvas-browser-forgets-a-part-that-is-gone ()
  ;; GIVEN a page whose picked part has gone, after a click or a new page
  ;; WHEN a line is scrolled
  ;; THEN the page answers that it is gone, the reader is told, AND the
  ;;      next scroll moves the whole page again
  (canvas-browser-test--in-page
    (setq canvas-browser--scroller 1)
    (let ((said nil))
      (cl-letf (((symbol-function 'canvas-browser-cdp-send)
                 (lambda (method params &optional answer _session)
                   (push (cons method params) canvas-browser-test--commands)
                   (when answer (funcall answer '(:result (:value :false))))))
                ((symbol-function 'message)
                 (lambda (format &rest args) (setq said (apply #'format format args)))))
        (canvas-browser-scroll-line-up)
        (should-not canvas-browser--scroller)
        (should (string-search "the whole page" said))))
    (canvas-browser-scroll-line-up)
    (should (string-search "window.scrollBy"
                           (plist-get (canvas-browser-test--params "Runtime.evaluate")
                                      :expression)))))

(ert-deftest canvas-browser-escape-gives-the-keys-back-to-the-whole-page ()
  ;; GIVEN a page buffer where a part was picked
  ;; WHEN the parts are labelled again and ESC is pressed
  ;; THEN the whole page scrolls again, which is the way back
  (canvas-browser-test--in-page
    (setq canvas-browser--scroller 1)
    (cl-letf (((symbol-function 'canvas-browser--scrollers)
               (lambda (answer) (funcall answer '((:x 0 :y 0 :w 200 :h 400)))))
              ((symbol-function 'canvas-browser--draw-hints) #'ignore)
              ((symbol-function 'canvas-browser--read-hint) (lambda (&rest _) nil)))
      (canvas-browser-pick-scroller))
    (should-not canvas-browser--scroller)))

(ert-deftest canvas-browser-says-when-nothing-scrolls-on-its-own ()
  ;; GIVEN a page of one piece
  ;; WHEN the parts that scroll are asked for
  ;; THEN the reader is told, and nothing is picked
  (canvas-browser-test--in-page
    (let ((said nil))
      (cl-letf (((symbol-function 'canvas-browser--scrollers)
                 (lambda (answer) (funcall answer nil)))
                ((symbol-function 'message)
                 (lambda (format &rest args) (setq said (apply #'format format args)))))
        (canvas-browser-pick-scroller)
        (should (string-search "scrolls on its own" said))
        (should-not canvas-browser--scroller)))))

(ert-deftest canvas-browser-the-key-for-picking-a-part-is-bound ()
  ;; GIVEN a page buffer in normal state
  ;; WHEN `S\=' is looked up
  ;; THEN it labels the parts that scroll, as ace-window labels windows
  (canvas-browser-test--in-page
    (should (eq (key-binding (kbd "S")) #'canvas-browser-pick-scroller))))

(ert-deftest canvas-browser-the-whole-page-carries-a-letter-too ()
  ;; GIVEN a page with two parts that scroll on their own
  ;; WHEN the parts are labelled
  ;; THEN the whole page is labelled first, in the top corner, where a
  ;;      part is unlikely to begin, AND naming it sends the keys back to
  ;;      the whole page, as `ESC\=' does
  (canvas-browser-test--in-page
    (let ((labelled nil))
      (cl-letf (((symbol-function 'canvas-browser--scrollers)
                 (lambda (answer) (funcall answer '((:x 0 :y 0 :w 200 :h 400)
                                                    (:x 300 :y 0 :w 200 :h 400)))))
                ((symbol-function 'canvas-browser--draw-hints)
                 (lambda (boxes _hints) (setq labelled boxes)))
                ((symbol-function 'canvas-browser--read-hint) (lambda (&rest _) (list 0))))
        (setq canvas-browser--scroller 1)
        (canvas-browser-pick-scroller)
        (should (equal (length labelled) 3))
        (should (> (plist-get (car labelled) :x) 600))
        (should (equal (plist-get (car labelled) :y) 0))
        (should-not canvas-browser--scroller)))))

;;;; A crisp picture once the page is quiet

(ert-deftest canvas-browser-the-still-picture-is-asked-for-without-loss ()
  ;; GIVEN a page buffer
  ;; WHEN a picture of the window is asked for
  ;; THEN it is asked for as a PNG, which loses nothing: a still page is
  ;;      read rather than watched, and JPEG shows its workings around
  ;;      small text
  (canvas-browser-test--in-page
    (canvas-browser--paint-window)
    (let ((asked (canvas-browser-test--params "Page.captureScreenshot")))
      (should (equal (plist-get asked :format) "png"))
      (should-not (plist-get asked :quality)))))

(ert-deftest canvas-browser-a-quiet-page-is-drawn-again-without-loss ()
  ;; GIVEN a page that has just painted a frame of the screencast
  ;; WHEN the page then stays quiet for the crisp delay
  ;; THEN a picture of the window is asked for, and the timer is spent
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'canvas-cairo-image) #'ignore)
              ((symbol-function 'canvas-refresh) #'ignore)
              ((symbol-function 'get-buffer-window) (lambda (&rest _) 'a-window)))
      (canvas-browser-test--frame :data (base64-encode-string "not a picture")
                                  :sessionId "S1")
      (should (timerp canvas-browser--crisp-timer))
      (setq canvas-browser-test--commands nil)
      (canvas-browser--paint-crisp (current-buffer))
      (should (canvas-browser-test--params "Page.captureScreenshot"))
      (should-not canvas-browser--crisp-timer))))

(ert-deftest canvas-browser-a-page-that-keeps-moving-puts-the-crisp-picture-off ()
  ;; GIVEN a page painting one frame after another, as an animation does
  ;; WHEN a second frame arrives before the crisp delay is up
  ;; THEN the waiting timer is dropped for a new one, so that the crisp
  ;;      picture is taken once the page stops, and never during it
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'canvas-cairo-image) #'ignore)
              ((symbol-function 'canvas-refresh) #'ignore))
      (canvas-browser-test--frame :data (base64-encode-string "one") :sessionId "S1")
      (let ((first canvas-browser--crisp-timer))
        (canvas-browser-test--frame :data (base64-encode-string "two") :sessionId "S1")
        (should-not (eq first canvas-browser--crisp-timer))
        (should-not (memq first timer-list))
        (should (memq canvas-browser--crisp-timer timer-list))
        (cancel-timer canvas-browser--crisp-timer)))))

(ert-deftest canvas-browser-the-crisp-picture-does-not-ask-for-another ()
  ;; GIVEN a page that painted the crisp picture it asked for
  ;; WHEN that picture reaches the canvas
  ;; THEN no new crisp picture is waiting: a still that asked for a still
  ;;      would pulse between the two for ever
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'canvas-cairo-image) #'ignore)
              ((symbol-function 'canvas-refresh) #'ignore))
      (canvas-browser--paint (base64-encode-string (canvas-browser-test--png 800 600)))
      (should canvas-browser--crisp)
      (should-not canvas-browser--crisp-timer))))

(ert-deftest canvas-browser-a-frame-of-the-picture-already-there-is-dropped ()
  ;; GIVEN a page that has painted a frame
  ;; WHEN chromium sends that very frame again, as it does after every
  ;;      picture asked of it, the picture being a draw like any other
  ;; THEN nothing is painted and nothing is asked for afterwards: else
  ;;      the still picture and the frame it provokes take turns on the
  ;;      canvas, which the reader sees as a pulse
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'canvas-cairo-image) #'ignore)
              ((symbol-function 'canvas-refresh) #'ignore))
      (let ((frame (base64-encode-string (canvas-browser-test--jpeg 800 600))))
        (canvas-browser--paint frame)
        (should (= canvas-browser--frames 1))
        (cancel-timer canvas-browser--crisp-timer)
        (setq canvas-browser--crisp-timer nil)
        (canvas-browser--paint frame)
        (should (= canvas-browser--frames 1))
        (should-not canvas-browser--crisp-timer)))))

(ert-deftest canvas-browser-a-frame-that-differs-is-painted-and-asks-for-a-still ()
  ;; GIVEN a page that has painted a frame
  ;; WHEN a frame of something else arrives
  ;; THEN it is painted, and a crisp picture is asked for once the page
  ;;      is quiet again
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'canvas-cairo-image) #'ignore)
              ((symbol-function 'canvas-refresh) #'ignore))
      (canvas-browser--paint (base64-encode-string (canvas-browser-test--jpeg 800 600)))
      (canvas-browser--paint (base64-encode-string (canvas-browser-test--jpeg 800 601)))
      (should (= canvas-browser--frames 2))
      (should (timerp canvas-browser--crisp-timer))
      (cancel-timer canvas-browser--crisp-timer))))

(ert-deftest canvas-browser-a-page-in-no-window-is-left-alone-when-it-quietens ()
  ;; GIVEN a page buffer that no window shows
  ;; WHEN the crisp picture falls due
  ;; THEN nothing is asked for: a page nobody looks at needs no picture
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil)))
      (setq canvas-browser-test--commands nil)
      (canvas-browser--paint-crisp (current-buffer))
      (should-not (canvas-browser-test--params "Page.captureScreenshot")))))

(ert-deftest canvas-browser-a-picture-is-named-after-what-it-is ()
  ;; GIVEN the bytes of a JPEG frame and of a PNG still picture
  ;; WHEN each is written, and the page is then let go of
  ;; THEN each file is named after what it holds, so that nothing which
  ;;      reads it later is told a lie, AND both are taken away again
  (canvas-browser-test--in-page
    (let ((jpeg (canvas-browser--write-bytes (canvas-browser-test--jpeg 8 8) "frame"))
          (png (canvas-browser--write-bytes (canvas-browser-test--png 8 8) "frame")))
      (should (equal (file-name-extension jpeg) "jpg"))
      (should (equal (file-name-extension png) "png"))
      (should (file-exists-p jpeg))
      (should (file-exists-p png))
      (canvas-browser--forget-files)
      (should-not (file-exists-p jpeg))
      (should-not (file-exists-p png)))))

(ert-deftest canvas-browser-a-crisp-page-is-not-asked-for-more-pictures ()
  ;; GIVEN a page standing still, whose crisp picture is on the canvas
  ;; WHEN the freshness check looks at it, twice over
  ;; THEN it asks for nothing: the canvas already holds the page as it
  ;;      is, and a picture every two seconds of a page that has stopped
  ;;      is chromium drawing for nobody
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'canvas-cairo-image) #'ignore)
              ((symbol-function 'canvas-refresh) #'ignore)
              ((symbol-function 'get-buffer-window) (lambda (&rest _) 'a-window)))
      (canvas-browser--paint (base64-encode-string (canvas-browser-test--png 800 600)))
      (should canvas-browser--crisp)
      (setq canvas-browser-test--commands nil)
      (canvas-browser--keep-fresh (current-buffer))
      (canvas-browser--keep-fresh (current-buffer))
      (should-not (canvas-browser-test--params "Page.captureScreenshot")))))

(ert-deftest canvas-browser-a-page-showing-only-frames-is-still-looked-after ()
  ;; GIVEN a page whose last picture is a frame of the screencast
  ;; WHEN the freshness check finds that nothing was painted since
  ;; THEN a picture of the window is asked for, as before: a page whose
  ;;      frames stop with the window half drawn must not stay that way
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'canvas-cairo-image) #'ignore)
              ((symbol-function 'canvas-refresh) #'ignore)
              ((symbol-function 'get-buffer-window) (lambda (&rest _) 'a-window)))
      (canvas-browser--paint (base64-encode-string (canvas-browser-test--jpeg 800 600)))
      (should-not canvas-browser--crisp)
      (setq canvas-browser--fresh-frames canvas-browser--frames
            canvas-browser-test--commands nil)
      (canvas-browser--keep-fresh (current-buffer))
      (should (canvas-browser-test--params "Page.captureScreenshot")))))

(ert-deftest canvas-browser-a-new-canvas-forgets-the-picture-it-had ()
  ;; GIVEN a page that painted a frame, whose window then changed size
  ;; WHEN that very frame arrives again
  ;; THEN it is painted: the canvas is a new and empty one, so the
  ;;      picture it holds is no longer the picture that was painted
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'canvas-cairo-image) #'ignore)
              ((symbol-function 'canvas-refresh) #'ignore)
              ((symbol-function 'canvas-cairo-destroy) #'ignore)
              ((symbol-function 'canvas-cairo-context) (lambda (_canvas) 'a-context)))
      (let ((frame (base64-encode-string (canvas-browser-test--jpeg 800 600))))
        (canvas-browser--paint frame)
        (should canvas-browser--painted-mark)
        (canvas-browser--adopt 800 600)
        (should-not canvas-browser--painted-mark)
        (canvas-browser--paint frame)
        (should (= canvas-browser--frames 2))
        (when (timerp canvas-browser--crisp-timer)
          (cancel-timer canvas-browser--crisp-timer))))))

;;;; The parts of a page that keep moving

(ert-deftest canvas-browser-asks-what-keeps-moving-after-a-still-picture ()
  ;; GIVEN a page that is being drawn without loss
  ;; WHEN it is asked what keeps moving
  ;; THEN the script asks the page for the animations it is running and
  ;;      for the elements that draw themselves, which is where the
  ;;      movement of a page that is otherwise still comes from
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'a-window)))
      (canvas-browser--paint-crisp (current-buffer))
      (let ((asked (canvas-browser-test--params "Runtime.evaluate")))
        (should (string-search "getAnimations" (plist-get asked :expression)))
        (should (string-search "video" (plist-get asked :expression)))))))

(ert-deftest canvas-browser-the-moving-parts-are-grown-a-little ()
  ;; GIVEN a spinner of 22 pixels, as GitHub puts beside a running job
  ;; WHEN it is taken as a moving part
  ;; THEN its box grows by the padding, and no further than the window,
  ;;      because a turning thing reaches past the box it is measured in
  (canvas-browser-test--in-page
    (canvas-browser--took-live '((:x 100 :y 200 :w 22 :h 22)))
    (let ((box (car canvas-browser--live-boxes))
          (pad canvas-browser-live-pad))
      (should (equal (plist-get box :x) (- 100 pad)))
      (should (equal (plist-get box :y) (- 200 pad)))
      (should (equal (plist-get box :w) (+ 22 pad pad)))
      (should (equal (plist-get box :h) (+ 22 pad pad))))
    (canvas-browser--took-live '((:x 0 :y 0 :w 22 :h 22)))
    (should (equal (plist-get (car canvas-browser--live-boxes) :x) 0))))

(ert-deftest canvas-browser-a-page-that-moves-all-over-keeps-its-frames ()
  ;; GIVEN a page where what moves covers more of the window than the
  ;;       share allowed, as a video or a page of advertisements does
  ;; WHEN it is measured
  ;; THEN no part is kept: such a page is a moving page, and its frames
  ;;      belong on the whole canvas
  (canvas-browser-test--in-page
    (canvas-browser--took-live '((:x 0 :y 0 :w 800 :h 500)))
    (should-not canvas-browser--live-boxes)
    (should-not canvas-browser--live-timer)))

(ert-deftest canvas-browser-a-frame-paints-only-what-moves ()
  ;; GIVEN a page whose still picture is on the canvas, with one small
  ;;       part of it moving
  ;; WHEN a frame arrives
  ;; THEN the frame is drawn inside that part alone, so that the crisp
  ;;      text around it stays crisp, AND no new still is asked for: the
  ;;      part is looked after by a clock of its own
  (canvas-browser-test--in-page
    (let ((clips nil) (drawn 0))
      (cl-letf (((symbol-function 'canvas-cairo-image) (lambda (&rest _) (cl-incf drawn)))
                ((symbol-function 'canvas-refresh) #'ignore)
                ((symbol-function 'canvas-cairo-save) #'ignore)
                ((symbol-function 'canvas-cairo-restore) #'ignore)
                ((symbol-function 'canvas-cairo-clip) #'ignore)
                ((symbol-function 'canvas-cairo-rectangle)
                 (lambda (_context x y w h) (push (list x y w h) clips))))
        (canvas-browser--took-live '((:x 100 :y 200 :w 22 :h 22)))
        (canvas-browser--paint (base64-encode-string (canvas-browser-test--png 800 600)))
        (setq clips nil drawn 0)
        (canvas-browser--paint (base64-encode-string (canvas-browser-test--jpeg 800 600)))
        (should (equal (length clips) 1))
        (should (= drawn 1))
        (should canvas-browser--crisp)
        (should-not canvas-browser--crisp-timer)))))

(ert-deftest canvas-browser-an-empty-canvas-takes-the-whole-frame ()
  ;; GIVEN a page whose moving parts are known, whose canvas is new and
  ;;       empty, as it is after a window changed size
  ;; WHEN a frame arrives
  ;; THEN it covers the window: drawing it into the moving parts alone
  ;;      would leave the rest of a new canvas blank
  (canvas-browser-test--in-page
    (let ((clipped nil))
      (cl-letf (((symbol-function 'canvas-cairo-image) #'ignore)
                ((symbol-function 'canvas-refresh) #'ignore)
                ((symbol-function 'canvas-cairo-clip)
                 (lambda (&rest _) (setq clipped t))))
        (setq canvas-browser--live-boxes '((:x 100 :y 200 :w 22 :h 22))
              canvas-browser--painted-mark nil)
        (canvas-browser--paint (base64-encode-string (canvas-browser-test--jpeg 800 600)))
        (should-not clipped)))))

(ert-deftest canvas-browser-a-frame-keeps-to-the-moving-parts-while-they-are-known ()
  ;; GIVEN a page with known moving parts, whose canvas holds a frame
  ;;       rather than a still picture
  ;; WHEN the next frame arrives
  ;; THEN it too is drawn into the moving parts alone: painting the whole
  ;;      window from a frame would take the crisp text away again for a
  ;;      moment, which is the flicker the reader sees
  (canvas-browser-test--in-page
    (let ((clipped 0))
      (cl-letf (((symbol-function 'canvas-cairo-image) #'ignore)
                ((symbol-function 'canvas-refresh) #'ignore)
                ((symbol-function 'canvas-cairo-save) #'ignore)
                ((symbol-function 'canvas-cairo-restore) #'ignore)
                ((symbol-function 'canvas-cairo-rectangle) #'ignore)
                ((symbol-function 'canvas-cairo-clip)
                 (lambda (&rest _) (cl-incf clipped))))
        (canvas-browser--paint (base64-encode-string (canvas-browser-test--jpeg 800 600)))
        (canvas-browser--took-live '((:x 100 :y 200 :w 22 :h 22)))
        (setq canvas-browser--crisp nil)
        (canvas-browser--paint (base64-encode-string (canvas-browser-test--jpeg 800 601)))
        (should (= clipped 1))))))

(ert-deftest canvas-browser-the-freshness-check-leaves-a-moving-page-alone ()
  ;; GIVEN a page whose moving parts are known, and so has a still
  ;;       picture taken of it every interval
  ;; WHEN the freshness check looks at it
  ;; THEN it asks for nothing: that page is looked after already, and two
  ;;      clocks asking would draw twice as often for nothing
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'a-window)))
      (canvas-browser--took-live '((:x 100 :y 200 :w 22 :h 22)))
      (setq canvas-browser-test--commands nil)
      (canvas-browser--keep-fresh (current-buffer))
      (canvas-browser--keep-fresh (current-buffer))
      (should-not (canvas-browser-test--params "Page.captureScreenshot")))))

(ert-deftest canvas-browser-a-still-picture-covers-the-whole-canvas ()
  ;; GIVEN a page with a moving part
  ;; WHEN the still picture of the window arrives
  ;; THEN it is drawn over the whole canvas, moving part and all: it is
  ;;      the page as it stands
  (canvas-browser-test--in-page
    (let ((clips nil))
      (cl-letf (((symbol-function 'canvas-cairo-image) #'ignore)
                ((symbol-function 'canvas-refresh) #'ignore)
                ((symbol-function 'canvas-cairo-clip)
                 (lambda (&rest _) (push 'clipped clips))))
        (canvas-browser--took-live '((:x 100 :y 200 :w 22 :h 22)))
        (canvas-browser--paint (base64-encode-string (canvas-browser-test--png 800 600)))
        (should-not clips)
        (should canvas-browser--crisp)))))

(ert-deftest canvas-browser-a-command-forgets-what-was-moving ()
  ;; GIVEN a page with moving parts
  ;; WHEN any command is run in the buffer
  ;; THEN the parts are forgotten, so the next frame covers the window:
  ;;      a key or a click may have changed the page anywhere
  (canvas-browser-test--in-page
    (should (memq #'canvas-browser--forget-live pre-command-hook))
    (canvas-browser--took-live '((:x 100 :y 200 :w 22 :h 22)))
    (should canvas-browser--live-boxes)
    (should (timerp canvas-browser--live-timer))
    (canvas-browser--forget-live)
    (should-not canvas-browser--live-boxes)
    (should-not canvas-browser--live-timer)))

(defun canvas-browser-test--about (number expected)
  "Whether NUMBER is EXPECTED, give or take the slack of a float."
  (< (abs (- number expected)) 0.001))

(ert-deftest canvas-browser-the-still-picture-waits-on-the-keys-not-the-frames ()
  ;; GIVEN a page with a spinner on it, which sends frame after frame and
  ;;       never falls quiet
  ;; WHEN the wait for the still picture is worked out
  ;; THEN the frames do not come into it: the picture waits a moment
  ;;      after the last key, and an interval after the last picture, so
  ;;      that a page which never stops moving is still drawn crisp soon
  ;;      after the reader stops working on it
  (canvas-browser-test--in-page
    (let ((now (float-time)))
      (setq canvas-browser--commanded nil canvas-browser--crisp-when nil)
      (should (<= (canvas-browser--crisp-wait now) 0.1))
      (setq canvas-browser--commanded now)
      (should (canvas-browser-test--about (canvas-browser--crisp-wait now)
                                          canvas-browser-crisp-delay))
      (setq canvas-browser--commanded nil canvas-browser--crisp-when now)
      (should (canvas-browser-test--about (canvas-browser--crisp-wait now)
                                          canvas-browser-live-interval))
      (setq canvas-browser--commanded now canvas-browser--crisp-when now)
      (should (canvas-browser-test--about (canvas-browser--crisp-wait now)
                                          canvas-browser-live-interval)))))

(ert-deftest canvas-browser-a-still-picture-marks-its-time ()
  ;; GIVEN a page buffer
  ;; WHEN a still picture is asked for
  ;; THEN the time is kept, because the next one is due an interval after
  ;;      this one, however busy the page is in between
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'a-window)))
      (setq canvas-browser--crisp-when nil)
      (canvas-browser--paint-crisp (current-buffer))
      (should canvas-browser--crisp-when))))

(ert-deftest canvas-browser-the-moving-parts-are-drawn-in-one-go ()
  ;; GIVEN a page with five moving parts on it
  ;; WHEN a frame is drawn into them
  ;; THEN the picture is read once, not once for each part: reading a
  ;;      frame costs about four milliseconds, and five readings of it
  ;;      cost more than drawing the whole window would
  (canvas-browser-test--in-page
    (let ((drawn 0) (clips 0) (rectangles 0))
      (cl-letf (((symbol-function 'canvas-cairo-image) (lambda (&rest _) (cl-incf drawn)))
                ((symbol-function 'canvas-cairo-clip) (lambda (&rest _) (cl-incf clips)))
                ((symbol-function 'canvas-cairo-rectangle)
                 (lambda (&rest _) (cl-incf rectangles)))
                ((symbol-function 'canvas-cairo-save) #'ignore)
                ((symbol-function 'canvas-cairo-new-path) #'ignore)
                ((symbol-function 'canvas-cairo-restore) #'ignore))
        (canvas-browser--paint-boxes "a-file" '((:x 0 :y 0 :w 10 :h 10)
                                                (:x 20 :y 20 :w 10 :h 10)
                                                (:x 40 :y 40 :w 10 :h 10)
                                                (:x 60 :y 60 :w 10 :h 10)
                                                (:x 80 :y 80 :w 10 :h 10)))
        (should (= drawn 1))
        (should (= clips 1))
        (should (= rectangles 5))))))

(ert-deftest canvas-browser-a-command-puts-the-still-picture-off ()
  ;; GIVEN a page whose still picture is long overdue
  ;; WHEN a command runs in the buffer, as a held scroll key does twelve
  ;;      times a second
  ;; THEN the picture waits for the page to fall quiet again: a still
  ;;      picture costs chromium a fifth of a second, which is time it
  ;;      owes the scrolling
  (canvas-browser-test--in-page
    (setq canvas-browser--crisp-when (- (float-time) (* 10 canvas-browser-live-interval)))
    (canvas-browser--forget-live)
    (should (canvas-browser-test--about (canvas-browser--crisp-wait (float-time))
                                        canvas-browser-crisp-delay))))

(ert-deftest canvas-browser-a-still-picture-of-a-page-that-moved-on-is-dropped ()
  ;; GIVEN a still picture asked for, which chromium takes a fifth of a
  ;;       second to make
  ;; WHEN a command runs in the buffer before it arrives
  ;; THEN the picture is dropped: the page it shows is the page as it was
  ;;      before the key, and painting it would take the window backwards
  (canvas-browser-test--in-page
    (let ((answer nil) (painted 0))
      (cl-letf (((symbol-function 'canvas-browser-cdp-send)
                 (lambda (_method _params &optional then &rest _) (setq answer then)))
                ((symbol-function 'canvas-browser--paint-soon)
                 (lambda (&rest _) (cl-incf painted))))
        (canvas-browser--paint-window)
        (should answer)
        (canvas-browser--forget-live)
        (funcall answer '(:data "MA=="))
        (should (= painted 0))))))

(ert-deftest canvas-browser-a-still-picture-of-the-page-as-it-is-is-painted ()
  ;; GIVEN a still picture asked for
  ;; WHEN nothing has happened in the buffer meanwhile
  ;; THEN it is painted
  (canvas-browser-test--in-page
    (let ((answer nil) (painted 0))
      (cl-letf (((symbol-function 'canvas-browser-cdp-send)
                 (lambda (_method _params &optional then &rest _) (setq answer then)))
                ((symbol-function 'canvas-browser--paint-soon)
                 (lambda (&rest _) (cl-incf painted))))
        (canvas-browser--paint-window)
        (funcall answer '(:data "MA=="))
        (should (= painted 1))))))

;;;; A canvas painted in parts, once the parts are forgotten

(defmacro canvas-browser-test--with-drawing (drawn clipped &rest body)
  "Run BODY with the drawing stubbed.
DRAWN and CLIPPED are variables.  The first counts the pictures drawn,
the second those drawn into the moving parts alone."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'canvas-cairo-image) (lambda (&rest _) (cl-incf ,drawn)))
             ((symbol-function 'canvas-cairo-clip) (lambda (&rest _) (cl-incf ,clipped)))
             ((symbol-function 'canvas-refresh) #'ignore)
             ((symbol-function 'canvas-cairo-save) #'ignore)
             ((symbol-function 'canvas-cairo-restore) #'ignore)
             ((symbol-function 'canvas-cairo-rectangle) #'ignore))
     ,@body))

(defun canvas-browser-test--paint-into-a-part ()
  "Paint a still picture, name one moving part, and paint a frame into it."
  (canvas-browser--paint (base64-encode-string (canvas-browser-test--png 800 600)))
  (canvas-browser--took-live '((:x 100 :y 200 :w 22 :h 22)))
  (canvas-browser--paint (base64-encode-string (canvas-browser-test--jpeg 800 600))))

(ert-deftest canvas-browser-a-page-with-nothing-moving-keeps-its-still-picture ()
  ;; GIVEN a still picture asked for, and with it the question what
  ;;       keeps moving on the page
  ;; WHEN the page answers that nothing moves, before the picture arrives
  ;; THEN the picture is painted: no command ran in the buffer, so the
  ;;      picture shows the page as it is
  (canvas-browser-test--in-page
    (let ((answer nil) (painted 0))
      (cl-letf (((symbol-function 'canvas-browser-cdp-send)
                 (lambda (_method _params &optional then &rest _) (setq answer then)))
                ((symbol-function 'canvas-browser--paint-soon)
                 (lambda (&rest _) (cl-incf painted))))
        (canvas-browser--paint-window)
        (canvas-browser--took-live nil)
        (funcall answer '(:data "MA=="))
        (should (= painted 1))))))

(ert-deftest canvas-browser-forgotten-parts-have-the-window-asked-for-its-picture ()
  ;; GIVEN a page drawn into its moving part alone, whose part is then
  ;;       forgotten, AND no frame follows
  ;; WHEN the freshness check looks at the page twice
  ;; THEN it asks for a picture of the window: the canvas is no longer
  ;;      known to show the page as it is
  (canvas-browser-test--in-page
    (let ((drawn 0) (clipped 0))
      (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'a-window)))
        (canvas-browser-test--with-drawing drawn clipped
          (canvas-browser-test--paint-into-a-part)
          (canvas-browser--took-live nil)
          (setq canvas-browser-test--commands nil)
          (canvas-browser--keep-fresh (current-buffer))
          (canvas-browser--keep-fresh (current-buffer))
          (should (canvas-browser-test--params "Page.captureScreenshot")))))))

;;;; Typing into the page

(ert-deftest canvas-browser-the-focus-is-asked-about-once-the-click-is-done ()
  ;; GIVEN a page buffer
  ;; WHEN a click is sent, and chromium has not yet said it handled it
  ;; THEN the page is not yet asked what has the focus: asked too soon,
  ;;      it names what had the focus before the click, and the keys stay
  ;;      with Emacs while the reader types into the field
  (canvas-browser-test--in-page
    (let ((sent nil) (waiting nil))
      (cl-letf (((symbol-function 'canvas-browser-cdp-send)
                 (lambda (method params &optional answer _session)
                   (push (cons method params) sent)
                   (when answer (push answer waiting)))))
        (canvas-browser--click 40 90)
        (should-not (assoc "Runtime.evaluate" sent))
        (funcall (car waiting) nil)
        (should (assoc "Runtime.evaluate" sent))))))

(ert-deftest canvas-browser-the-focus-question-looks-inside-components-and-frames ()
  ;; GIVEN the script that asks what has the focus
  ;; WHEN it is read
  ;; THEN it follows the focus into the shadow root of a web component and
  ;;      into a frame of the same site, where the field itself is
  (should (string-search "shadowRoot" canvas-browser--focus-script))
  (should (string-search "contentDocument" canvas-browser--focus-script)))

(defun canvas-browser-test--keys-sent ()
  "The key events sent, oldest first, as plists."
  (reverse (mapcar #'cdr (cl-remove "Input.dispatchKeyEvent" canvas-browser-test--commands
                                    :key #'car :test-not #'equal))))

(defun canvas-browser-test--press (keys)
  "Run what KEYS, an Emacs key description of one key, runs in this buffer."
  (let* ((sequence (kbd keys))
         (last-command-event (aref (key-parse keys) 0)))
    (call-interactively (key-binding sequence))))

(ert-deftest canvas-browser-backspace-reaches-the-field-as-a-key-it-knows ()
  ;; GIVEN a page buffer in insert state
  ;; WHEN backspace is pressed
  ;; THEN it goes down and up with its code and the number Windows gives
  ;;      it: chromium edits a field by that number, and a key sent by its
  ;;      name alone reaches the page as an event that deletes nothing
  (canvas-browser-test--in-page
    (canvas-browser-insert-mode)
    (setq canvas-browser-test--commands nil)
    (canvas-browser-test--press "DEL")
    (let ((events (canvas-browser-test--keys-sent)))
      (should (equal (mapcar (lambda (e) (plist-get e :type)) events)
                     '("rawKeyDown" "keyUp")))
      (should (equal (plist-get (car events) :code) "Backspace"))
      (should (equal (plist-get (car events) :windowsVirtualKeyCode) 8)))))

(ert-deftest canvas-browser-return-reaches-the-field-with-its-text ()
  ;; GIVEN a page buffer in insert state
  ;; WHEN return is pressed
  ;; THEN it goes down with the text of a return, which is what sends a
  ;;      form, and up again
  (canvas-browser-test--in-page
    (canvas-browser-insert-mode)
    (setq canvas-browser-test--commands nil)
    (canvas-browser-test--press "RET")
    (let ((down (car (canvas-browser-test--keys-sent))))
      (should (equal (plist-get down :type) "keyDown"))
      (should (equal (plist-get down :text) "\r"))
      (should (equal (plist-get down :windowsVirtualKeyCode) 13)))))

(ert-deftest canvas-browser-a-key-without-a-code-is-refused ()
  ;; GIVEN the table of the keys chromium is sent
  ;; WHEN a key it does not hold is to be sent
  ;; THEN that is an error at once, rather than an event that does nothing
  (canvas-browser-test--in-page
    (should-error (canvas-browser--key "NoSuchKey"))))

(ert-deftest canvas-browser-the-editing-keys-of-emacs-edit-the-field ()
  ;; GIVEN a page buffer in insert state
  ;; WHEN the editing keys of Emacs are pressed
  ;; THEN each is the key of a browser field that does the same thing,
  ;;      with Control held where Emacs moves or deletes a word
  (canvas-browser-test--in-page
    (canvas-browser-insert-mode)
    (dolist (case '(("C-a" "Home" 0) ("C-e" "End" 0)
                    ("C-f" "ArrowRight" 0) ("C-b" "ArrowLeft" 0)
                    ("C-n" "ArrowDown" 0) ("C-p" "ArrowUp" 0)
                    ("M-f" "ArrowRight" 2) ("M-b" "ArrowLeft" 2)
                    ("C-d" "Delete" 0) ("M-d" "Delete" 2)
                    ("M-DEL" "Backspace" 2)))
      (setq canvas-browser-test--commands nil)
      (canvas-browser-test--press (car case))
      (let ((down (car (canvas-browser-test--keys-sent))))
        (should (equal (list (car case) (plist-get down :key) (plist-get down :modifiers))
                       case))))))

(defun canvas-browser-test--keys-down ()
  "The keys that went down, oldest first, as (KEY MODIFIERS)."
  (mapcar (lambda (event) (list (plist-get event :key) (plist-get event :modifiers)))
          (cl-remove "keyUp" (canvas-browser-test--keys-sent)
                     :key (lambda (event) (plist-get event :type)) :test #'equal)))

(ert-deftest canvas-browser-kill-line-deletes-once-the-page-told-of-the-mark ()
  ;; GIVEN a page buffer in insert state
  ;; WHEN C-k is pressed
  ;; THEN the page is asked to tell of the next change of its selection,
  ;;      AND Shift and End go to it, which mark the rest of the line, AND
  ;;      Delete does not go yet: an editor that keeps a selection of its
  ;;      own has not learned of the mark, and would delete something else
  ;; WHEN the page tells of the change
  ;; THEN Delete goes
  (canvas-browser-test--in-page
    (canvas-browser-insert-mode)
    (let ((told nil))
      (cl-letf (((symbol-function 'canvas-browser-cdp-send)
                 (lambda (method params &optional answer _session)
                   (push (cons method params) canvas-browser-test--commands)
                   (cond ((plist-get params :awaitPromise) (setq told answer))
                         ((equal method "Runtime.evaluate")
                          (funcall answer '(:result (:value "the rest"))))))))
        (setq canvas-browser-test--commands nil)
        (canvas-browser-test--press "C-k")
        (should (string-search "selectionchange"
                               (plist-get (cdr (car (last canvas-browser-test--commands)))
                                          :expression)))
        (should (equal (canvas-browser-test--keys-down) '(("End" 8))))
        (funcall told '(:result (:value "changed")))
        (should (equal (canvas-browser-test--keys-down) '(("End" 8) ("Delete" 0))))))))

(defmacro canvas-browser-test--with-field (marked lengths &rest body)
  "Run BODY in a page whose field has MARKED as its marked text.
LENGTHS is a list of numbers: each question about the length of the text
of the field takes the next of them.  The page tells of a change of its
selection at once."
  (declare (indent 2))
  `(let ((lengths ,lengths))
     (cl-letf (((symbol-function 'canvas-browser-cdp-send)
                (lambda (method params &optional answer _session)
                  (push (cons method params) canvas-browser-test--commands)
                  (when answer
                    (funcall answer
                             (when (equal method "Runtime.evaluate")
                               (list :result
                                     (list :value
                                           (if (equal (plist-get params :expression)
                                                      canvas-browser--field-length-js)
                                               (pop lengths)
                                             ,marked)))))))))
       ,@body)))

(ert-deftest canvas-browser-c-k-puts-the-rest-of-the-line-in-the-kill-ring ()
  ;; GIVEN a field whose line holds " world" after the cursor
  ;; WHEN C-k is pressed
  ;; THEN " world" is the newest kill, as after C-k in a buffer, AND
  ;;      Delete goes to the page
  (canvas-browser-test--in-page
    (canvas-browser-insert-mode)
    (let ((kill-ring nil) (this-command nil))
      (canvas-browser-test--with-field " world" nil
        (setq canvas-browser-test--commands nil)
        (canvas-browser-test--press "C-k"))
      (should (equal (car kill-ring) " world"))
      (should (member '("Delete" 0) (canvas-browser-test--keys-down))))))

(ert-deftest canvas-browser-c-k-twice-makes-one-kill ()
  ;; GIVEN a C-k that killed "one"
  ;; WHEN C-k is pressed again right after it, and kills " two"
  ;; THEN the kill ring holds one kill, "one two", as two C-k in a row
  ;;      make one kill in a buffer
  (canvas-browser-test--in-page
    (canvas-browser-insert-mode)
    (let ((kill-ring nil) (this-command nil))
      (canvas-browser-test--with-field "one" nil
        (canvas-browser-test--press "C-k"))
      (should (eq this-command 'kill-region))
      (let ((last-command this-command))
        (canvas-browser-test--with-field " two" nil
          (canvas-browser-test--press "C-k")))
      (should (equal kill-ring '("one two"))))))

(ert-deftest canvas-browser-c-k-at-the-end-of-a-line-kills-the-line-break ()
  ;; GIVEN a field with nothing after the cursor on its line, and a line
  ;;       after that one
  ;; WHEN C-k is pressed, AND Delete makes the text of the field shorter
  ;; THEN the newest kill is a newline: the line break went, as C-k at
  ;;      the end of a line kills it in a buffer
  (canvas-browser-test--in-page
    (canvas-browser-insert-mode)
    (let ((kill-ring nil) (this-command nil))
      (canvas-browser-test--with-field "" (list 20 19)
        (canvas-browser-test--press "C-k"))
      (should (equal (car kill-ring) "\n")))))

(ert-deftest canvas-browser-c-k-at-the-end-of-a-field-kills-nothing ()
  ;; GIVEN a field with nothing after the cursor at all
  ;; WHEN C-k is pressed, AND Delete leaves the text of the field as long
  ;;      as it was
  ;; THEN the kill ring is as it was
  (canvas-browser-test--in-page
    (canvas-browser-insert-mode)
    (let ((kill-ring nil) (this-command nil))
      (canvas-browser-test--with-field "" (list 20 20)
        (canvas-browser-test--press "C-k"))
      (should-not kill-ring))))

(ert-deftest canvas-browser-yank-types-the-newest-kill ()
  ;; GIVEN a page buffer in insert state, and a kill in Emacs
  ;; WHEN C-y is pressed
  ;; THEN the kill is typed into the field
  (canvas-browser-test--in-page
    (canvas-browser-insert-mode)
    (let ((kill-ring (list "a kill")) (kill-ring-yank-pointer nil)
          (interprogram-paste-function nil))
      (setq kill-ring-yank-pointer kill-ring)
      (canvas-browser-test--press "C-y")
      (should (equal (plist-get (canvas-browser-test--params "Input.insertText") :text)
                     "a kill")))))

(ert-deftest canvas-browser-a-key-read-for-a-hint-is-not-typed-afterwards ()
  ;; GIVEN the letters of a hint, read while chromium's answer was being
  ;;       handled, which Emacs then counts among the keys of the next
  ;;       command
  ;; WHEN the first letter is typed into the field the hint chose, and then
  ;;      a backspace
  ;; THEN the letter goes alone and the backspace is a backspace: the keys
  ;;      of the hint are neither typed nor taken for part of the key
  (canvas-browser-test--in-page
    (canvas-browser-insert-mode)
    (setq canvas-browser-test--commands nil)
    (cl-letf (((symbol-function 'this-command-keys) (lambda () "hla")))
      (let ((last-command-event ?a))
        (canvas-browser-self-insert)))
    (should (equal (mapcar (lambda (e) (plist-get e :text)) (canvas-browser-test--keys-sent))
                   '("a" nil)))
    (setq canvas-browser-test--commands nil)
    (cl-letf (((symbol-function 'this-command-keys) (lambda () (vconcat "hl" [127]))))
      (let ((last-command-event 127))
        (canvas-browser-send-key)))
    (should (equal (plist-get (car (canvas-browser-test--keys-sent)) :key) "Backspace"))))

(ert-deftest canvas-browser-the-keys-of-a-hint-are-forgotten-once-read ()
  ;; GIVEN a hint being read
  ;; WHEN its letters are read
  ;; THEN Emacs is told to forget them as keys of a command
  (let ((cleared nil) (keys (list ?a)))
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) (pop keys)))
              ((symbol-function 'clear-this-command-keys) (lambda (&rest _) (setq cleared t))))
      (should (equal (canvas-browser--read-hint '("a" "s")) '(0)))
      (should cleared))))

(ert-deftest canvas-browser-tab-goes-from-field-to-field-in-both-states ()
  ;; GIVEN a page buffer, in insert state and in normal state
  ;; WHEN tab and shift tab are looked up
  ;; THEN they go to the next and the previous field in either state:
  ;;      from the username to the password, whatever the page puts
  ;;      between the two
  (canvas-browser-test--in-page
    (should (eq (key-binding (kbd "TAB")) #'canvas-browser-next-field))
    (should (eq (key-binding (kbd "<backtab>")) #'canvas-browser-previous-field))
    (canvas-browser-insert-mode)
    (should (eq (key-binding (kbd "TAB")) #'canvas-browser-next-field))
    (should (eq (key-binding (kbd "<backtab>")) #'canvas-browser-previous-field))))

(ert-deftest canvas-browser-a-letter-goes-as-a-key-that-types ()
  ;; GIVEN a page buffer in insert state
  ;; WHEN a letter is typed
  ;; THEN it goes down as a key with the letter for its text, and up
  ;;      again, so that it reaches what has the focus: text put in
  ;;      without a key lands at the caret, which may be in a field the
  ;;      focus has left
  (canvas-browser-test--in-page
    (canvas-browser-insert-mode)
    (setq canvas-browser-test--commands nil)
    (let ((last-command-event ?q)) (canvas-browser-self-insert))
    (should-not (canvas-browser-test--params "Input.insertText"))
    (let ((events (canvas-browser-test--keys-sent)))
      (should (equal (mapcar (lambda (e) (plist-get e :type)) events) '("keyDown" "keyUp")))
      (should (equal (plist-get (car events) :text) "q"))
      (should (equal (plist-get (car events) :key) "q")))))

(ert-deftest canvas-browser-the-next-field-is-focused-and-then-followed ()
  ;; GIVEN a page buffer in normal state
  ;; WHEN the next field is asked for, and then the one before
  ;; THEN the page is told to focus the field one on, and then one back,
  ;;      AND once it has, the page is asked about its focus, which sends
  ;;      the keys to the field
  (canvas-browser-test--in-page
    (let ((sent nil) (waiting nil))
      (cl-letf (((symbol-function 'canvas-browser-cdp-send)
                 (lambda (method params &optional answer _session)
                   (push (cons method params) sent)
                   (when answer (push answer waiting)))))
        (canvas-browser-next-field)
        (should (= (length sent) 1))
        (should (string-search "(1)" (plist-get (cdar sent) :expression)))
        (funcall (car waiting) nil)
        (should (equal (plist-get (cdar sent) :expression) canvas-browser--focus-script))
        (setq sent nil)
        (canvas-browser-previous-field)
        (should (string-search "(-1)" (plist-get (cdar sent) :expression)))))))

;;;; Windows a page opens

(defun canvas-browser-test--created (target type opener)
  "Tell this page's chromium that TARGET of TYPE was opened by OPENER."
  (canvas-browser--target-created
   (list :targetInfo (list :targetId target :type type :openerId opener
                           :url "https://accounts.google.com/signin"))))

(ert-deftest canvas-browser-a-page-listens-for-the-windows-pages-open ()
  ;; GIVEN a page being opened
  ;; WHEN chromium is set up for it
  ;; THEN chromium is asked to tell of every page that opens and closes:
  ;;      a button to sign in with Google or Apple opens a window, which
  ;;      would else open where nobody sees it
  (canvas-browser-test--in-page
    (should (equal (canvas-browser-test--params "Target.setDiscoverTargets")
                   '(:discover t)))))

(ert-deftest canvas-browser-a-window-a-page-opens-gets-a-buffer-of-its-own ()
  ;; GIVEN a page buffer, whose target is T1
  ;; WHEN chromium says that T1 opened the page P1
  ;; THEN P1 is shown in a page buffer of its own, attached to it, AND is
  ;;      not sent anywhere: it is already at the address it was opened at
  (canvas-browser-test--in-page
    (let ((opened nil))
      (unwind-protect
          (progn
            (setq canvas-browser-test--commands nil)
            (canvas-browser-test--created "P1" "page" canvas-browser--target)
            (setq opened (canvas-browser--buffer-of-target "P1"))
            (should (buffer-live-p opened))
            (should (equal (plist-get (canvas-browser-test--params "Target.attachToTarget")
                                      :targetId)
                           "P1"))
            (should-not (canvas-browser-test--params "Page.navigate"))
            ;; Told again, as chromium may be, it makes no second buffer.
            (canvas-browser-test--created "P1" "page" canvas-browser--target)
            (should (= (cl-count-if (lambda (b) (eq (buffer-local-value 'major-mode b)
                                                    'canvas-browser-mode))
                                    (buffer-list))
                       2)))
        (when (buffer-live-p opened) (kill-buffer opened))))))

(ert-deftest canvas-browser-only-windows-of-its-own-pages-get-a-buffer ()
  ;; GIVEN a page buffer
  ;; WHEN chromium tells of a page some other page opened, and of a frame
  ;;      this page opened
  ;; THEN neither gets a buffer: only a window one of these pages opened
  (canvas-browser-test--in-page
    (canvas-browser-test--created "P2" "page" "SOMEONE-ELSE")
    (canvas-browser-test--created "F1" "iframe" canvas-browser--target)
    (should-not (canvas-browser--buffer-of-target "P2"))
    (should-not (canvas-browser--buffer-of-target "F1"))))

(ert-deftest canvas-browser-a-window-that-closes-itself-takes-its-buffer ()
  ;; GIVEN a window a page opened, in a buffer of its own
  ;; WHEN the window closes itself, as the window to sign in does once
  ;;      you have
  ;; THEN its buffer goes too, AND chromium is not asked to close a page
  ;;      it has closed already
  (canvas-browser-test--in-page
    (canvas-browser-test--created "P1" "page" canvas-browser--target)
    (let ((opened (canvas-browser--buffer-of-target "P1")))
      (setq canvas-browser-test--commands nil)
      (canvas-browser--target-destroyed '(:targetId "P1"))
      (should-not (buffer-live-p opened))
      (should-not (canvas-browser-test--params "Target.closeTarget")))))

(defvar canvas-minimap-exclude-modes)

(ert-deftest canvas-browser-a-page-has-no-map-key ()
  ;; GIVEN canvas-minimap, which a page tells to leave it out
  ;; WHEN the menu of a page is read, and m is pressed
  ;; THEN the menu offers no map, AND m says that a page has none, rather
  ;;      than turning on a map that draws nothing
  (canvas-browser-test--in-page
    (let ((canvas-minimap-exclude-modes nil))
      (canvas-browser--leave-out-of-map)
      (cl-letf (((symbol-function 'canvas-minimap-mode) #'ignore))
        (should-not (funcall (plist-get (canvas-browser-test--menu-entry "m") :if)))
        (should (string-search "no map"
                               (error-message-string
                                (should-error (call-interactively (key-binding (kbd "m")))
                                              :type 'user-error))))))))

;;;; What a hint does

(defun canvas-browser-test--reading (hints keys &rest actions)
  "Read one of HINTS from KEYS, with ACTIONS; the answer and the prompts."
  (let ((prompts nil))
    (cl-letf (((symbol-function 'read-key)
               (lambda (prompt &rest _) (push prompt prompts) (pop keys))))
      (list (canvas-browser--read-hint hints actions) (reverse prompts)))))

(ert-deftest canvas-browser-a-key-before-the-letters-picks-what-the-hint-does ()
  ;; GIVEN the hints a and s, and an action on w
  ;; WHEN s is typed, and then w and s, and then s and w
  ;; THEN s alone chooses s and no action; w first picks the action, which
  ;;      the prompt then names, as the dispatch of avy does; a key of an
  ;;      action after a letter is only a letter that names no hint
  (let ((copy '(?w "copy text" ignore)))
    (should (equal (car (canvas-browser-test--reading '("a" "s") (list ?s) copy)) '(1)))
    (let ((reading (canvas-browser-test--reading '("a" "s") (list ?w ?s) copy)))
      (should (equal (car reading) (cons 1 copy)))
      (should (string-search "copy text" (cadr (cadr reading)))))
    (should-not (car (canvas-browser-test--reading '("as" "ss") (list ?s ?w) copy)))
    (should-not (car (canvas-browser-test--reading '("a" "s") (list ?\e) copy)))))

(ert-deftest canvas-browser-an-action-may-not-share-a-key-with-the-hints ()
  ;; GIVEN hint letters that hold w, and an action on w
  ;; WHEN a hint is read
  ;; THEN that is an error at once: a w would else pick the action or
  ;;      name a hint, and nobody could say which, as avy refuses too
  (let ((canvas-browser-hint-keys "asw"))
    (should (string-search "hint letter"
                           (error-message-string
                            (should-error (canvas-browser--read-hint
                                           '("a" "s") '((?w "copy text" ignore)))))))))

(ert-deftest canvas-browser-the-actions-keep-out-of-the-hint-letters ()
  ;; GIVEN the actions of a hint and the letters of the hints
  ;; WHEN they are compared
  ;; THEN no key is both
  (should-not (cl-intersection (mapcar #'car canvas-browser--hint-actions)
                               (append canvas-browser-hint-keys nil))))

(defmacro canvas-browser-test--answering (value &rest body)
  "Run BODY in a page whose chromium answers every script with VALUE."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'canvas-browser-cdp-send)
              (lambda (method params &optional answer _session)
                (push (cons method params) canvas-browser-test--commands)
                (when answer
                  (funcall answer (if (equal method "Runtime.evaluate")
                                      (list :result (list :value ,value))
                                    (list :data (base64-encode-string "PNG")))))))
             ((symbol-function 'canvas-browser--draw-hints) #'ignore)
             ((symbol-function 'canvas-browser--paint-window) #'ignore))
     ,@body))

(ert-deftest canvas-browser-y-before-a-hint-copies-its-address ()
  ;; GIVEN a page with a link, whose hint is chosen after y
  ;; WHEN the hint is named
  ;; THEN the address of the link goes to the kill ring, not a click
  (canvas-browser-test--in-page
    (let ((kill-ring nil))
      (canvas-browser-test--answering "https://example.org/a"
        (cl-letf (((symbol-function 'canvas-browser--boxes)
                   (lambda (answer) (funcall answer '((:x 0 :y 0 :w 10 :h 10)))))
                  ((symbol-function 'canvas-browser--read-hint)
                   (lambda (&rest _) (cons 0 (assq ?y canvas-browser--hint-actions)))))
          (setq canvas-browser-test--commands nil)
          (canvas-browser-hints)
          (should (equal (car kill-ring) "https://example.org/a"))
          (should-not (canvas-browser-test--params "Input.dispatchMouseEvent")))))))

(ert-deftest canvas-browser-w-before-a-hint-copies-its-text ()
  ;; GIVEN a page with a link, whose hint is chosen after w
  ;; WHEN the hint is named
  ;; THEN the text of the link goes to the kill ring
  (canvas-browser-test--in-page
    (let ((kill-ring nil))
      (canvas-browser-test--answering "The words of it"
        (cl-letf (((symbol-function 'canvas-browser--boxes)
                   (lambda (answer) (funcall answer '((:x 0 :y 0 :w 10 :h 10)))))
                  ((symbol-function 'canvas-browser--read-hint)
                   (lambda (&rest _) (cons 0 (assq ?w canvas-browser--hint-actions)))))
          (canvas-browser-hints)
          (should (equal (car kill-ring) "The words of it")))))))

(ert-deftest canvas-browser-copy-labels-the-blocks-and-copies-a-picture ()
  ;; GIVEN a page buffer
  ;; WHEN M-w is pressed, and the first block is named with no action
  ;; THEN the blocks of the page are asked for, a picture of the window is
  ;;      taken, and the part the block shows is cut from it and copied
  (canvas-browser-test--in-page
    (should (eq canvas-keys-copy-function #'canvas-browser-copy-block))
    (let ((copied nil) (cut nil))
      (canvas-browser-test--answering '(20 30 100 50 800)
        (cl-letf (((symbol-function 'canvas-browser--blocks)
                   (lambda (answer) (funcall answer '((:x 20 :y 30 :w 100 :h 50)))))
                  ((symbol-function 'canvas-browser--read-hint) (lambda (&rest _) (list 0)))
                  ((symbol-function 'canvas-browser--picture-size) (lambda (_png) '(800 . 600)))
                  ((symbol-function 'canvas-browser--crop-png)
                   (lambda (_png x y width height) (setq cut (list x y width height)) "PART"))
                  ((symbol-function 'canvas-keys-copy-png) (lambda (bytes) (setq copied bytes))))
          (canvas-browser-copy-block)
          (should (canvas-browser-test--params "Page.captureScreenshot"))
          (should (equal cut '(20 30 100 50)))
          (should (equal copied "PART")))))))

(ert-deftest canvas-browser-browse-url-opens-the-url-in-a-page ()
  ;; GIVEN a frame that can show a canvas, and chromium stubbed
  ;; WHEN browse-url hands over a URL, with the new-window flag it may add
  ;; THEN the URL opens in a page buffer of its own
  (canvas-browser-test--with-chromium
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'image-type-available-p) (lambda (&rest _) t)))
      (let ((buffer (canvas-browser-browse-url "https://example.org/a" t)))
        (unwind-protect
            (progn
              (should (eq 'canvas-browser-mode (buffer-local-value 'major-mode buffer)))
              (should (equal (plist-get (canvas-browser-test--params "Page.navigate") :url)
                             "https://example.org/a")))
          (kill-buffer buffer))))))

(ert-deftest canvas-browser-browse-url-hands-over-where-no-canvas-shows ()
  ;; GIVEN a frame that cannot show a canvas, as a terminal frame
  ;; WHEN browse-url hands over a URL with its new-window flag
  ;; THEN the fallback browser gets the URL and the flag, AND no page opens
  (let* ((got nil)
         (canvas-browser-fallback-browser (lambda (&rest args) (setq got args))))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
      (canvas-browser-browse-url "https://example.org/b" t))
    (should (equal got '("https://example.org/b" t)))))

;;;; A local file a snap chromium cannot read

(defmacro canvas-browser-test--with-home (chromium &rest body)
  "Run BODY with HOME in a directory of its own and CHROMIUM as the chromium.
`home' is bound to that directory, which BODY may fill."
  (declare (indent 1))
  `(let* ((home (make-temp-file "canvas-browser-home-" t))
          (process-environment (cons (concat "HOME=" home) process-environment)))
     (unwind-protect
         (cl-letf (((symbol-function 'canvas-browser-cdp--executable) (lambda () ,chromium)))
           ,@body)
       (delete-directory home t))))

(defun canvas-browser-test--navigated-to ()
  "The URL the page buffer last asked chromium to go to."
  (plist-get (canvas-browser-test--params "Page.navigate") :url))

(ert-deftest canvas-browser-a-file-a-snap-cannot-read-is-copied-where-it-can ()
  ;; GIVEN a snap chromium, which has a /tmp of its own, and a page that
  ;;       another package wrote to the /tmp of Emacs
  ;; WHEN the page is opened
  ;; THEN chromium goes to a copy under the snap's own directory in the
  ;;      home, AND the copy holds the page
  (canvas-browser-test--with-home "/snap/bin/chromium"
    (let ((page (make-temp-file "canvas-browser-page-" nil ".html" "<p>hello</p>")))
      (unwind-protect
          (canvas-browser-test--in-page
            (canvas-browser-open-url (concat "file://" page))
            (let ((copy (string-remove-prefix "file://" (canvas-browser-test--navigated-to))))
              (should (string-prefix-p (expand-file-name "snap/chromium/common/" home) copy))
              (should (equal (with-temp-buffer (insert-file-contents copy) (buffer-string))
                             "<p>hello</p>"))))
        (delete-file page)))))

(ert-deftest canvas-browser-a-file-a-chromium-can-read-is-opened-where-it-is ()
  ;; GIVEN a page in /tmp and a chromium that is no snap, and a page in the
  ;;       home that a snap chromium can read
  ;; WHEN each is opened
  ;; THEN chromium goes to each where it is
  (let ((page (make-temp-file "canvas-browser-page-" nil ".html" "<p>hi</p>")))
    (unwind-protect
        (canvas-browser-test--with-home "/usr/bin/chromium"
          (canvas-browser-test--in-page
            (canvas-browser-open-url (concat "file://" page))
            (should (equal (canvas-browser-test--navigated-to) (concat "file://" page)))))
      (delete-file page)))
  (canvas-browser-test--with-home "/snap/bin/chromium"
    (let ((page (expand-file-name "notes/page.html" home)))
      (make-directory (file-name-directory page) t)
      (write-region "<p>hi</p>" nil page)
      (canvas-browser-test--in-page
        (canvas-browser-open-url (concat "file://" page))
        (should (equal (canvas-browser-test--navigated-to) (concat "file://" page)))))))

;;;; A page embedded in another buffer

(ert-deftest canvas-browser-a-page-of-its-own-opens-as-a-tab ()
  ;; GIVEN a page opened in a buffer of its own
  ;; THEN chromium opens it as a tab, not in a window of its own
  (canvas-browser-test--in-page
    (should (equal (canvas-browser-test--params "Target.createTarget")
                   '(:url "about:blank")))))

(defmacro canvas-browser-test--embedded (page text host &rest body)
  "Embed a page of 400 by 225 in HOST, a new buffer; run BODY.
PAGE is bound to the page buffer and TEXT to the text to insert in HOST,
which BODY may insert.  A frame that can show a canvas is assumed."
  (declare (indent 3))
  `(canvas-browser-test--with-chromium
     (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
               ((symbol-function 'image-type-available-p) (lambda (&rest _) t)))
       (let* ((,host (generate-new-buffer " *host*"))
              (,text (canvas-browser-embed "https://example.org/video" 400 225 ,host))
              (,page (get-text-property 0 'canvas-browser-embed ,text)))
         (unwind-protect
             (progn ,@body)
           (when (buffer-live-p ,page) (kill-buffer ,page))
           (when (buffer-live-p ,host) (kill-buffer ,host)))))))

(defun canvas-browser-test--sent-p (method)
  "Whether METHOD was sent, and forget what was sent."
  (prog1 (assoc method canvas-browser-test--commands)
    (setq canvas-browser-test--commands nil)))

(ert-deftest canvas-browser-embed-opens-a-hidden-page-for-its-host ()
  ;; GIVEN a host buffer
  ;; WHEN a page of 400 by 225 is embedded in it
  ;; THEN the text to insert shows the page's canvas at that size, under a
  ;;      pointing hand, AND the page is in a hidden buffer of its own that does not claim the
  ;;      keys, AND chromium opens it in a window of its own and goes to it
  (canvas-browser-test--embedded page text host
    (should (eq 'hand (get-text-property 0 'pointer text)))
    (let ((canvas (get-text-property 0 'display text)))
      (should (eq canvas (buffer-local-value 'canvas-browser--canvas page)))
      (should (equal (plist-get (cdr canvas) :data-width) 400))
      (should (equal (plist-get (cdr canvas) :data-height) 225)))
    (should (string-prefix-p " " (buffer-name page)))
    (should (eq host (buffer-local-value 'canvas-browser--host page)))
    ;; The keys stay with the page you browse: chromium sends them to
    ;; none when two pages claim the focus.
    (should-not (equal (canvas-browser-test--params "Emulation.setFocusEmulationEnabled")
                       '(:enabled t)))
    ;; A window of its own keeps it in front: a tab behind another is
    ;; hidden, and chromium stops drawing its video.
    (should (equal (canvas-browser-test--params "Target.createTarget")
                   '(:url "about:blank" :newWindow t)))
    (should (equal (canvas-browser-test--params "Page.navigate")
                   '(:url "https://example.org/video")))))

(ert-deftest canvas-browser-embed-needs-a-canvas ()
  ;; GIVEN a frame that cannot show a canvas
  ;; WHEN a page is embedded
  ;; THEN nothing is embedded, AND no page opens
  (canvas-browser-test--with-chromium
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
      (with-temp-buffer
        (should-not (canvas-browser-embed "https://example.org/video" 400 225
                                          (current-buffer)))
        (should-not canvas-browser-test--commands)))))

(ert-deftest canvas-browser-an-embedded-page-is-shown-while-its-host-is ()
  ;; GIVEN a page embedded in a host that no window shows
  ;; WHEN a window comes to show the host, and later goes back
  ;; THEN the page is drawn while the host is shown, at the pace of a
  ;;      page in front, AND let be once it is not
  (canvas-browser-test--embedded page text host
    (with-current-buffer host (insert text))
    (let ((before (window-buffer (selected-window))))
      (canvas-browser-test--sent-p "Page.startScreencast")
      (canvas-browser--follow-shown-windows)
      (should (canvas-browser-test--sent-p "Page.stopScreencast"))
      (should-not (canvas-browser--shown-p page))
      (set-window-buffer (selected-window) host)
      (unwind-protect
          (progn
            (canvas-browser--follow-shown-windows)
            (should (canvas-browser-test--sent-p "Page.startScreencast"))
            (should (canvas-browser--shown-p page))
            (should (<= (with-current-buffer page (canvas-browser--paint-delay))
                        canvas-browser-frame-interval)))
        (set-window-buffer (selected-window) before)))))

(ert-deftest canvas-browser-an-embedded-page-goes-with-its-host ()
  ;; GIVEN a page embedded in a host
  ;; WHEN the host is killed
  ;; THEN the page goes too
  (canvas-browser-test--embedded page text host
    (kill-buffer host)
    (should-not (buffer-live-p page))))

(ert-deftest canvas-browser-an-embedded-page-goes-when-its-text-does ()
  ;; GIVEN a page embedded in a host, its text inserted there
  ;; WHEN the host lets go of the text, as a buffer drawn again does
  ;; THEN the page goes at its next look at its freshness
  (canvas-browser-test--embedded page text host
    (with-current-buffer host (insert "before " text " after"))
    (canvas-browser--keep-fresh page)
    (should (buffer-live-p page))
    (with-current-buffer host (erase-buffer) (insert "drawn again"))
    (canvas-browser--keep-fresh page)
    (should-not (buffer-live-p page))))

(ert-deftest canvas-browser-a-click-on-an-embed-reaches-its-page ()
  ;; GIVEN a page embedded in a host, its text inserted there
  ;; WHEN the picture is clicked at 40 by 30, and then RET is pressed on it
  ;; THEN the page is clicked there, AND then in its middle
  (canvas-browser-test--embedded page text host
    (with-current-buffer host (insert text))
    (let ((before (window-buffer (selected-window))))
      (set-window-buffer (selected-window) host)
      (unwind-protect
          (with-current-buffer host
            (let ((event `(mouse-1 (,(selected-window) 1 (0 . 0) 0 nil 1 (0 . 0) nil
                                    (40 . 30) (400 . 225)))))
              (canvas-browser-embed-click event)
              (should (equal (seq-take (canvas-browser-test--params "Input.dispatchMouseEvent") 6)
                             '(:type "mouseReleased" :x 40 :y 30)))
              (goto-char 1)
              (canvas-browser-embed-click-middle)
              (should (equal (seq-take (canvas-browser-test--params "Input.dispatchMouseEvent") 6)
                             '(:type "mouseReleased" :x 200 :y 112)))))
        (set-window-buffer (selected-window) before)))))

;;;; An embedded page gone fullscreen

(defmacro canvas-browser-test--with-frames (made deleted &rest body)
  "Run BODY where a new frame is the selected one, recorded in MADE with its
parameters, and deleting a frame records it in DELETED instead."
  (declare (indent 2))
  `(let ((,made nil)
         (,deleted nil))
     (let ((pop-up-frame-function (lambda ()
                                    (push pop-up-frame-alist ,made)
                                    (selected-frame))))
       (cl-letf (((symbol-function 'delete-frame)
                  (lambda (&optional frame &rest _) (push frame ,deleted)))
                 ((symbol-function 'select-frame-set-input-focus) #'ignore))
         ,@body))))

(defun canvas-browser-test--fullscreen (on)
  "Have the embedded page say it went fullscreen, when ON, or came back."
  (canvas-browser-test--event "Runtime.bindingCalled"
                              (list :name "canvasBrowserFullscreen" :payload (if on "on" "off"))))

(ert-deftest canvas-browser-an-embedded-page-says-when-it-goes-fullscreen ()
  ;; GIVEN a page embedded in a host, and a page of its own
  ;; THEN the embedded page is given a way to say it goes fullscreen, AND
  ;;      the page of its own is not, having the whole window already
  (canvas-browser-test--embedded page text host
    (should (equal (canvas-browser-test--params "Runtime.addBinding")
                   '(:name "canvasBrowserFullscreen")))
    (should (string-match-p "fullscreenchange"
                            (plist-get (canvas-browser-test--params
                                        "Page.addScriptToEvaluateOnNewDocument")
                                       :source))))
  (canvas-browser-test--in-page
    (should-not (assoc "Runtime.addBinding" canvas-browser-test--commands))))

(ert-deftest canvas-browser-an-embedded-page-fullscreen-fills-a-frame-and-comes-back ()
  ;; GIVEN a page embedded in a host at 400 by 225, its text there
  ;; WHEN it goes fullscreen, and later comes back
  ;; THEN it is shown in a fullscreen frame of its own and laid out for
  ;;      that frame's window, AND afterwards the frame goes, the page has
  ;;      its size in the host again, and the host shows its new canvas
  (canvas-browser-test--embedded page text host
    (with-current-buffer host (insert "before " text " after"))
    (canvas-browser-test--with-frames made deleted
      (canvas-browser-test--fullscreen t)
      (should (equal (alist-get 'fullscreen (car made)) 'fullboth))
      (let ((window (get-buffer-window page)))
        (should window)
        (canvas-browser--follow-shown-windows)
        (should (equal (buffer-local-value 'canvas-browser--size page)
                       (cons (window-body-width window t) (window-body-height window t)))))
      (canvas-browser-test--fullscreen nil)
      (should (equal deleted (list (selected-frame))))
      (should (equal (buffer-local-value 'canvas-browser--size page) '(400 . 225)))
      (with-current-buffer host
        (should (eq (get-text-property (text-property-not-all (point-min) (point-max)
                                                              'canvas-browser-embed nil)
                                       'display)
                    (buffer-local-value 'canvas-browser--canvas page)))))))

(defmacro canvas-browser-test--on-x (fullscreen &rest body)
  "Run BODY as on an X display whose window manager can go FULLSCREEN, or not.
The window manager says so on the root window, where Emacs reads it."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'window-system) (lambda (&optional _) 'x))
             ((symbol-function 'x-window-property)
              (lambda (property &rest _)
                (pcase property
                  ("_NET_SUPPORTING_WM_CHECK" (vector 4194305))
                  ("_NET_SUPPORTED"
                   (if ,fullscreen
                       [_NET_WM_STATE _NET_WM_STATE_FULLSCREEN]
                     [_NET_WM_STATE _NET_WM_STATE_MAXIMIZED_VERT]))))))
     ,@body))

(defun canvas-browser-test--place-of (frame)
  "The place and size of FRAME, as the parameters of a new frame say them."
  (list (car (frame-position frame)) (cdr (frame-position frame))
        (frame-text-width frame) (frame-text-height frame)))

(defun canvas-browser-test--place-in (parameters)
  "The place and size the frame PARAMETERS ask for."
  (list (alist-get 'left parameters) (alist-get 'top parameters)
        (cdr (alist-get 'width parameters)) (cdr (alist-get 'height parameters))))

(ert-deftest canvas-browser-a-fullscreen-page-opens-over-the-frame-you-are-in ()
  ;; GIVEN an embedded page on an X display whose window manager can go
  ;;       fullscreen
  ;; WHEN it goes fullscreen
  ;; THEN its frame asks for fullscreen, AND opens where the frame you are
  ;;      in is, so it fills that monitor and not another one
  (canvas-browser-test--embedded page text host
    (canvas-browser-test--with-frames made deleted
      (canvas-browser-test--on-x t
        (canvas-browser-test--fullscreen t))
      (should (equal (alist-get 'fullscreen (car made)) 'fullboth))
      (should (equal (canvas-browser-test--place-in (car made))
                     (canvas-browser-test--place-of (selected-frame)))))))

(ert-deftest canvas-browser-a-fullscreen-page-takes-your-frame-where-fullscreen-fails ()
  ;; GIVEN an embedded page on an X display whose window manager cannot go
  ;;       fullscreen, where Emacs would stretch the frame over every monitor
  ;; WHEN it goes fullscreen
  ;; THEN its frame does not ask for fullscreen, AND takes the place and
  ;;      size of the frame you are in
  (canvas-browser-test--embedded page text host
    (canvas-browser-test--with-frames made deleted
      (canvas-browser-test--on-x nil
        (canvas-browser-test--fullscreen t))
      (should-not (assq 'fullscreen (car made)))
      (should (equal (canvas-browser-test--place-in (car made))
                     (canvas-browser-test--place-of (selected-frame)))))))

(ert-deftest canvas-browser-a-fullscreen-page-opens-left-of-the-main-monitor-too ()
  ;; GIVEN the frame you are in, on a monitor left of the main one
  ;; WHEN the place of a new frame over it is asked for
  ;; THEN its left edge is counted from the left, AND not from the right,
  ;;      as a plain negative number would be
  (cl-letf (((symbol-function 'frame-position) (lambda (&rest _) '(-1920 . 23))))
    (should (equal (alist-get 'left (canvas-browser--frame-place (selected-frame)))
                   '(+ -1920)))))

(ert-deftest canvas-browser-closing-the-fullscreen-frame-brings-the-page-back ()
  ;; GIVEN an embedded page shown fullscreen in a frame of its own
  ;; WHEN that frame is deleted, as quitting its window does
  ;; THEN the page is told to leave fullscreen, AND it has its size in the
  ;;      host again
  (canvas-browser-test--embedded page text host
    (with-current-buffer host (insert text))
    (canvas-browser-test--with-frames made deleted
      (canvas-browser-test--fullscreen t)
      (canvas-browser--follow-shown-windows)
      (run-hook-with-args 'delete-frame-functions (selected-frame))
      (should (string-match-p "exitFullscreen"
                              (plist-get (canvas-browser-test--params "Runtime.evaluate")
                                         :expression)))
      (should (equal (buffer-local-value 'canvas-browser--size page) '(400 . 225))))))


;;;; Opening a page in a buffer of its own, and the name of a buffer

(ert-deftest canvas-browser-capital-o-opens-a-url-in-a-buffer-of-its-own ()
  ;; GIVEN a page buffer in normal state
  ;; WHEN O is looked up, and the menu is read
  ;; THEN O opens a page buffer of its own, as the O of Vimium and of
  ;;      qutebrowser opens a tab, while o goes to the URL in this one
  (canvas-browser-test--in-page
    (should (eq (key-binding (kbd "O")) #'canvas-browser))
    (should (eq (key-binding (kbd "o")) #'canvas-browser-open-url))
    (should (equal (plist-get (canvas-browser-test--menu-entry "O") :description) "new"))))

(ert-deftest canvas-browser-the-command-is-what-loads-the-package ()
  ;; GIVEN the source of the package
  ;; WHEN the cookie that autoloads a command is looked for
  ;; THEN it stands on the command that opens a page, not on a helper
  (with-temp-buffer
    (insert-file-contents (locate-library "canvas-browser.el"))
    (should (re-search-forward "^;;;###autoload\n(defun canvas-browser (url)" nil t))))

(ert-deftest canvas-browser-a-page-that-moves-takes-its-buffer-name-along ()
  ;; GIVEN a page buffer, whose page goes to another address
  ;; WHEN chromium tells of the new address and title
  ;; THEN the buffer is named after the new address, and keeps the address
  ;;      and the title, so that C-x b says what each page shows
  (canvas-browser-test--in-page
    (canvas-browser--target-changed
     (list :targetInfo (list :targetId canvas-browser--target :type "page"
                             :url "https://example.org/next" :title "The next page")))
    (should (equal (buffer-name) "*canvas-browser: https://example.org/next*"))
    (should (equal canvas-browser--url "https://example.org/next"))
    (should (equal canvas-browser--title "The next page"))))

(ert-deftest canvas-browser-an-embedded-page-keeps-its-name ()
  ;; GIVEN a page embedded in another buffer, whose name its host chose
  ;; WHEN its page goes to another address
  ;; THEN its name stays as it is: the host finds it by that name
  (canvas-browser-test--in-page
    (rename-buffer " *canvas-browser embed: a video*" t)
    (setq canvas-browser--host (current-buffer))
    (canvas-browser--target-changed
     (list :targetInfo (list :targetId canvas-browser--target :type "page"
                             :url "https://example.org/other" :title "Other")))
    (should (string-prefix-p " *canvas-browser embed: a video*" (buffer-name)))))

(ert-deftest canvas-browser-a-page-that-is-not-ours-changes-nothing ()
  ;; GIVEN a page buffer
  ;; WHEN chromium tells of a new address of a page nobody here shows
  ;; THEN this buffer keeps its name and its address
  (canvas-browser-test--in-page
    (let ((name (buffer-name)))
      (canvas-browser--target-changed
       (list :targetInfo (list :targetId "SOMEONE-ELSE" :type "page"
                               :url "https://example.org/elsewhere" :title "Elsewhere")))
      (should (equal (buffer-name) name))
      (should (equal canvas-browser--url "https://example.org")))))

(ert-deftest canvas-browser-a-page-listens-for-its-address ()
  ;; GIVEN a page being opened
  ;; WHEN chromium is set up for it
  ;; THEN it is asked to tell when a page changes address or title
  (let ((heard nil))
    (cl-letf (((symbol-function 'canvas-browser-cdp-listen)
               (lambda (session method _function) (push (cons session method) heard)))
              ((symbol-function 'canvas-browser-cdp-send) #'ignore))
      (canvas-browser--watch-targets))
    (should (member '(nil . "Target.targetInfoChanged") heard))))

(ert-deftest canvas-browser-a-question-mark-lists-what-a-hint-can-do ()
  ;; GIVEN the hints a and s, and the actions of a hint
  ;; WHEN ? is typed, and then s
  ;; THEN the prompt after ? lists each action key with what it does, as
  ;;      the ? of avy does, AND s still names its hint
  (let ((reading (apply #'canvas-browser-test--reading '("a" "s") (list ?? ?s)
                        canvas-browser--hint-actions)))
    (should (equal (car reading) '(1)))
    (should (string-search "y copy address" (nth 1 (cadr reading))))
    (should (string-search "e eww" (nth 1 (cadr reading))))))

(ert-deftest canvas-browser-the-question-mark-is-kept-free ()
  ;; GIVEN hint letters that hold ?
  ;; WHEN a hint with actions is read
  ;; THEN that is an error at once: ? lists the actions
  (let ((canvas-browser-hint-keys "as?"))
    ;; A key to read, so that a reader without the check ends, not waits.
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?\e)))
      (should-error (canvas-browser--read-hint '("a" "s") canvas-browser--hint-actions)))))

(ert-deftest canvas-browser-a-page-that-has-loaded-gives-its-title ()
  ;; GIVEN a page buffer
  ;; WHEN the page has loaded
  ;; THEN the page is asked for its title, which the header line shows:
  ;;      chromium names a page after its file until it changes address
  ;;      again, whatever the page calls itself
  (canvas-browser-test--in-page
    (canvas-browser-test--answering "The title of the page"
      (canvas-browser--loaded nil)
      (should (equal canvas-browser--title "The title of the page")))))

(ert-deftest canvas-browser-a-page-listens-for-its-loads ()
  ;; GIVEN a page being attached
  ;; WHEN its events are listened for
  ;; THEN a load is among them
  (let ((heard nil))
    (cl-letf (((symbol-function 'canvas-browser-cdp-listen)
               (lambda (_session method _function) (push method heard))))
      (with-temp-buffer (canvas-browser--listen)))
    (should (member "Page.loadEventFired" heard))))

(ert-deftest canvas-browser-the-title-a-page-gives-stays-while-it-stays ()
  ;; GIVEN a page that has given its own title
  ;; WHEN chromium tells of it again, at the same address, named after
  ;;      its file
  ;; THEN the page keeps the title it gave: chromium's is only for a new
  ;;      address, until the page has loaded
  (canvas-browser-test--in-page
    (setq canvas-browser--url "file:///tmp/typing.html"
          canvas-browser--title "The page's own title")
    (canvas-browser--target-changed
     (list :targetInfo (list :targetId canvas-browser--target :type "page"
                             :url "file:///tmp/typing.html" :title "typing.html")))
    (should (equal canvas-browser--title "The page's own title"))))

;;;; Where a hint is drawn

(ert-deftest canvas-browser-a-hint-goes-at-the-corner-of-its-box ()
  ;; GIVEN one box, and a label of 20 by 16
  ;; WHEN its place is worked out
  ;; THEN the label goes at the top left corner of the box
  (should (equal (canvas-browser--hint-places '((:x 30 :y 40 :w 100 :h 30)) '((20 . 16)))
                 '((30 . 40)))))

(ert-deftest canvas-browser-a-big-box-gives-its-corner-to-a-small-one ()
  ;; GIVEN a post of 700 by 300 that is a link, and the link of its author
  ;;       in its corner, as a card of Reddit has them
  ;; WHEN the places of their labels are worked out
  ;; THEN the author keeps the corner, AND the label of the post goes in
  ;;      the middle of the post, where a click on it lands: two labels at
  ;;      one corner hide one of them, and the post could not be named
  (should (equal (canvas-browser--hint-places '((:x 0 :y 0 :w 700 :h 300)
                                                (:x 8 :y 4 :w 80 :h 24))
                                              '((20 . 16) (20 . 16)))
                 '((340 . 142) (8 . 4)))))

(ert-deftest canvas-browser-hints-apart-keep-their-corners ()
  ;; GIVEN two boxes whose labels do not meet
  ;; WHEN their places are worked out
  ;; THEN each label keeps the corner of its box
  (should (equal (canvas-browser--hint-places '((:x 0 :y 0 :w 700 :h 300)
                                                (:x 0 :y 400 :w 80 :h 24))
                                              '((20 . 16) (20 . 16)))
                 '((0 . 0) (0 . 400)))))


(ert-deftest canvas-browser-a-part-is-cut-from-a-picture-of-the-whole-window ()
  ;; GIVEN a part of the page to copy
  ;; WHEN its picture is taken
  ;; THEN chromium is asked for a picture of the whole window, not of the
  ;;      part, AND the part is cut from it in Emacs: chromium cuts a part
  ;;      by moving the view of the page to it for a while, and every
  ;;      frame and picture of that while shows the page shifted
  (canvas-browser-test--in-page
    (let ((answer nil) (cut nil) (copied nil))
      (cl-letf (((symbol-function 'canvas-browser-cdp-send)
                 (lambda (method params &optional then _session)
                   (push (cons method params) canvas-browser-test--commands)
                   (when then (setq answer then))))
                ((symbol-function 'canvas-browser--picture-size) (lambda (_png) '(1600 . 1000)))
                ((symbol-function 'canvas-browser--crop-png)
                 (lambda (png x y width height) (setq cut (list png x y width height)) "PART"))
                ((symbol-function 'canvas-keys-copy-png) (lambda (bytes) (setq copied bytes))))
        (setq canvas-browser-test--commands nil)
        (canvas-browser--capture-part '(530 197 732 628 800))
        (let ((asked (canvas-browser-test--params "Page.captureScreenshot")))
          (should asked)
          (should-not (plist-get asked :clip)))
        (should-not (canvas-browser-test--params "Page.stopScreencast"))
        (funcall answer (list :data (base64-encode-string "WHOLE")))
        ;; The picture is twice the size of the window: the page is zoomed.
        (should (equal cut '("WHOLE" 1060 394 1464 1256)))
        (should (equal copied "PART"))))))

(ert-deftest canvas-browser-a-part-of-a-picture-is-cut-where-it-is ()
  ;; GIVEN a white picture of 100 by 60 with a red block of 20 by 10 at
  ;;       40,20
  ;; WHEN the part at 40,20 of 20 by 10 is cut out
  ;; THEN the part is 20 by 10 and red
  (let* ((file (make-temp-file "canvas-browser-test-" nil ".png"))
         (canvas (list 'image :type 'canvas :id (make-symbol "whole")
                       :data-width 100 :data-height 60))
         (context (canvas-cairo-context canvas)))
    (unwind-protect
        (progn
          (canvas-cairo-set-color context 1 1 1 1)
          (canvas-cairo-rectangle context 0 0 100 60)
          (canvas-cairo-fill context)
          (canvas-cairo-set-color context 1 0 0 1)
          (canvas-cairo-rectangle context 40 20 20 10)
          (canvas-cairo-fill context)
          (canvas-cairo-write-png context file)
          (let* ((whole (with-temp-buffer (set-buffer-multibyte nil)
                                          (insert-file-contents-literally file)
                                          (buffer-string)))
                 (part (canvas-browser--crop-png whole 40 20 20 10)))
            (should (equal (canvas-browser--picture-size part) '(20 . 10)))
            (let* ((back (make-temp-file "canvas-browser-test-" nil ".png"))
                   (look (canvas-cairo-context (list 'image :type 'canvas :id (make-symbol "part")
                                                     :data-width 20 :data-height 10))))
              (unwind-protect
                  (progn
                    (let ((coding-system-for-write 'binary)) (write-region part nil back nil 'silent))
                    (canvas-cairo-image look back 0 0 20 10)
                    (should (= (canvas-cairo-pixel look 0 0) #xFFFF0000))
                    (should (= (canvas-cairo-pixel look 19 9) #xFFFF0000)))
                (canvas-cairo-destroy look)
                (delete-file back)))))
      (canvas-cairo-destroy context)
      (delete-file file))))

;;;; What was copied pulses

(defmacro canvas-browser-test--pulses (pulses &rest body)
  "Run BODY with the pulses of the page collected in PULSES, newest first."
  (declare (indent 1))
  `(let ((,pulses nil))
     (let ((canvas-browser-pulse-function (lambda (box) (push box ,pulses))))
       ,@body)))

(ert-deftest canvas-browser-a-copied-address-pulses-its-box ()
  ;; GIVEN a page with a link, whose address is copied with y
  ;; WHEN the address is in the kill ring
  ;; THEN the box of the link pulses, so the eye sees what was copied
  (canvas-browser-test--in-page
    (canvas-browser-test--pulses pulses
      (let ((kill-ring nil))
        (canvas-browser-test--answering "https://example.org/a"
          (canvas-browser--copy-address 0 '(:x 10 :y 20 :w 30 :h 40)))
        (should (equal pulses '((:x 10 :y 20 :w 30 :h 40))))))))

(ert-deftest canvas-browser-copied-text-pulses-its-box ()
  ;; GIVEN a page with an article, whose text is copied with w
  ;; THEN the box of the article pulses
  (canvas-browser-test--in-page
    (canvas-browser-test--pulses pulses
      (let ((kill-ring nil))
        (canvas-browser-test--answering "The words"
          (canvas-browser--copy-text 0 '(:x 1 :y 2 :w 3 :h 4)))
        (should (equal pulses '((:x 1 :y 2 :w 3 :h 4))))))))

(ert-deftest canvas-browser-a-copied-picture-pulses-the-part-it-shows ()
  ;; GIVEN a part of the page whose picture is taken
  ;; WHEN the picture is copied
  ;; THEN the part the picture shows pulses
  (canvas-browser-test--in-page
    (canvas-browser-test--pulses pulses
      (canvas-browser-test--answering nil
        (cl-letf (((symbol-function 'canvas-browser--picture-size) (lambda (_png) '(800 . 600)))
                  ((symbol-function 'canvas-browser--crop-png) (lambda (&rest _) "PART"))
                  ((symbol-function 'canvas-keys-copy-png) #'ignore))
          (canvas-browser--capture-part '(20 30 100 50 800))))
      (should (equal pulses '((:x 20 :y 30 :w 100 :h 50)))))))

(ert-deftest canvas-browser-nothing-copied-pulses-nothing ()
  ;; GIVEN a link without an address
  ;; WHEN its address is to be copied
  ;; THEN nothing pulses, since nothing was copied
  (canvas-browser-test--in-page
    (canvas-browser-test--pulses pulses
      (canvas-browser-test--answering ""
        (canvas-browser--copy-address 0 '(:x 10 :y 20 :w 30 :h 40)))
      (should-not pulses))))

(ert-deftest canvas-browser-the-pulse-is-smear-cursors-copy-effect ()
  ;; GIVEN a page zoomed to twice its size, shown in a window, with
  ;;       smear-cursor on
  ;; WHEN a box of the page pulses
  ;; THEN smear-cursor plays its copy effect over the box, where it lies in
  ;;      the picture of the page: the page is one glyph, a picture
  (canvas-browser-test--in-page
    (let ((flashed nil) (smear-cursor-mode t))
      (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'a-window))
                ((symbol-function 'smear-cursor-flash-in-picture)
                 (lambda (occasion pos rects window)
                   (setq flashed (list occasion pos rects window)))))
        (setq canvas-browser--zoom 2.0)
        (canvas-browser--pulse-box '(:x 10 :y 20 :w 30 :h 40))
        (should (equal flashed (list 'copy (point-min) (list (vector 20.0 40.0 60.0 80.0))
                                     'a-window)))))))

(ert-deftest canvas-browser-without-smear-cursor-nothing-pulses ()
  ;; GIVEN smear-cursor turned off
  ;; WHEN a box pulses
  ;; THEN nothing is asked of smear-cursor
  (canvas-browser-test--in-page
    (let ((flashed nil) (smear-cursor-mode nil))
      (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'a-window))
                ((symbol-function 'smear-cursor-flash-in-picture)
                 (lambda (&rest _) (setq flashed t))))
        (canvas-browser--pulse-box '(:x 10 :y 20 :w 30 :h 40))
        (should-not flashed)))))

;;;; The focus of the page moves, and the eye follows

(defmacro canvas-browser-test--flights (flights &rest body)
  "Run BODY with the flights of the focus collected in FLIGHTS, newest first."
  (declare (indent 1))
  `(let ((,flights nil))
     (let ((canvas-browser-focus-function
            (lambda (from to) (push (list from to) ,flights))))
       ,@body)))

(ert-deftest canvas-browser-the-focus-is-followed-from-box-to-box ()
  ;; GIVEN a page whose focus is learnt to be a button at 10,20
  ;; WHEN the focus is next learnt to be at 100,200, after a tab, say
  ;; THEN the eye is drawn from the first box to the second, AND a button
  ;;      keeps the keys with Emacs, since it takes no typing
  (canvas-browser-test--in-page
    (canvas-browser-test--flights flights
      (canvas-browser-test--answering '(:typing :false :box (10 20 30 40))
        (canvas-browser--follow-focus))
      (should (equal canvas-browser--focus-box '(:x 10 :y 20 :w 30 :h 40)))
      (should-not flights)
      (canvas-browser-test--answering '(:typing :false :box (100 200 30 40))
        (canvas-browser--follow-focus))
      (should (equal flights '(((:x 10 :y 20 :w 30 :h 40) (:x 100 :y 200 :w 30 :h 40)))))
      (should-not canvas-browser--insert))))

(ert-deftest canvas-browser-a-field-takes-the-keys-and-the-eye ()
  ;; GIVEN a page whose focus was on a button
  ;; WHEN the focus moves to a field
  ;; THEN the keys go to the page, AND the eye is drawn to the field
  (canvas-browser-test--in-page
    (canvas-browser-test--flights flights
      (setq canvas-browser--focus-box '(:x 10 :y 20 :w 30 :h 40))
      (canvas-browser-test--answering '(:typing t :box (50 60 300 30))
        (canvas-browser--follow-focus))
      (should canvas-browser--insert)
      (should (equal (cadr (car flights)) '(:x 50 :y 60 :w 300 :h 30))))))

(ert-deftest canvas-browser-a-focus-lost-is-forgotten ()
  ;; GIVEN a page whose focus was on a field
  ;; WHEN the focus goes to the page itself, or out of sight
  ;; THEN no box is kept and nothing flies: the next focus is not flown to
  ;;      from a place the reader no longer sees
  (canvas-browser-test--in-page
    (canvas-browser-test--flights flights
      (setq canvas-browser--focus-box '(:x 10 :y 20 :w 30 :h 40))
      (canvas-browser-test--answering '(:typing :false :box :null)
        (canvas-browser--follow-focus))
      (should-not canvas-browser--focus-box)
      (should-not flights))))

(ert-deftest canvas-browser-the-focus-flies-with-smear-cursor ()
  ;; GIVEN a page zoomed to twice its size, shown in a window, with
  ;;       smear-cursor on
  ;; WHEN the focus moves from one box to another
  ;; THEN smear-cursor flies its cursor between them, where they lie in
  ;;      the picture of the page
  (canvas-browser-test--in-page
    (let ((flown nil) (smear-cursor-mode t))
      (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'a-window))
                ((symbol-function 'smear-cursor-fly-in-picture)
                 (lambda (pos from to window) (setq flown (list pos from to window)))))
        (setq canvas-browser--zoom 2.0)
        (canvas-browser--fly-focus '(:x 10 :y 20 :w 30 :h 40) '(:x 100 :y 200 :w 30 :h 40))
        (should (equal flown (list (point-min) (vector 20.0 40.0 60.0 80.0)
                                   (vector 200.0 400.0 60.0 80.0) 'a-window)))))))

;;;; A region in a field

(ert-deftest canvas-browser-the-mark-in-a-field-makes-the-motions-mark ()
  ;; GIVEN a page buffer typing into a field
  ;; WHEN C-SPC is pressed, and then M-f
  ;; THEN M-f goes with Shift held, which marks the word in the field, as
  ;;      a motion after C-SPC does in any buffer; without the mark it
  ;;      goes as it is
  (canvas-browser-test--in-page
    (canvas-browser-insert-mode)
    (setq canvas-browser-test--commands nil)
    (canvas-browser-test--press "M-f")
    (should (equal (plist-get (car (canvas-browser-test--keys-sent)) :modifiers) 2))
    (canvas-browser-test--press "C-SPC")
    (should canvas-browser--field-mark)
    (setq canvas-browser-test--commands nil)
    (canvas-browser-test--press "M-f")
    (should (equal (plist-get (car (canvas-browser-test--keys-sent)) :modifiers) 10))))

(ert-deftest canvas-browser-copying-a-region-of-a-field ()
  ;; GIVEN a field whose marked text is "hello"
  ;; WHEN M-w is pressed
  ;; THEN the text is in the kill ring, the field pulses, AND the mark is
  ;;      gone, as it is after M-w anywhere
  (canvas-browser-test--in-page
    (canvas-browser-insert-mode)
    (canvas-browser-test--pulses pulses
      (let ((kill-ring nil))
        (setq canvas-browser--field-mark t
              canvas-browser--focus-box '(:x 1 :y 2 :w 3 :h 4))
        (canvas-browser-test--answering "hello"
          (canvas-browser-test--press "M-w"))
        (should (equal (car kill-ring) "hello"))
        (should (equal pulses '((:x 1 :y 2 :w 3 :h 4))))
        (should-not canvas-browser--field-mark)))))

(ert-deftest canvas-browser-cutting-a-region-of-a-field ()
  ;; GIVEN a field whose marked text is "hello"
  ;; WHEN C-w is pressed
  ;; THEN the text is in the kill ring AND a backspace deletes it from the
  ;;      field
  (canvas-browser-test--in-page
    (canvas-browser-insert-mode)
    (let ((kill-ring nil))
      (setq canvas-browser--field-mark t)
      (canvas-browser-test--answering "hello"
        (setq canvas-browser-test--commands nil)
        (canvas-browser-test--press "C-w"))
      (should (equal (car kill-ring) "hello"))
      (should (equal (plist-get (car (canvas-browser-test--keys-sent)) :key) "Backspace"))
      (should-not canvas-browser--field-mark))))

(ert-deftest canvas-browser-c-g-drops-the-mark-before-it-leaves-the-field ()
  ;; GIVEN a field with the mark set
  ;; WHEN C-g is pressed, and then again
  ;; THEN the first drops the mark and keeps the keys with the page, AND
  ;;      the second gives them back to Emacs
  (canvas-browser-test--in-page
    (canvas-browser-insert-mode)
    (setq canvas-browser--field-mark t)
    (canvas-browser-test--press "C-g")
    (should-not canvas-browser--field-mark)
    (should canvas-browser--insert)
    (canvas-browser-test--press "C-g")
    (should-not canvas-browser--insert)))

(ert-deftest canvas-browser-typing-over-a-region-drops-the-mark ()
  ;; GIVEN a field with the mark set
  ;; WHEN a letter is typed
  ;; THEN the mark is gone: the letter took the place of the region
  (canvas-browser-test--in-page
    (canvas-browser-insert-mode)
    (setq canvas-browser--field-mark t)
    (let ((last-command-event ?x)) (canvas-browser-self-insert))
    (should-not canvas-browser--field-mark)))

(ert-deftest canvas-browser-the-buffer-end-keys-go-to-the-ends-of-a-field ()
  ;; GIVEN a page buffer typing into a field
  ;; WHEN the keys that go to the ends of a buffer are pressed
  ;; THEN Home and End go to the page with Control held, which a field
  ;;      takes as its start and its end
  (canvas-browser-test--in-page
    (canvas-browser-insert-mode)
    (dolist (case '(("M-<" "Home" 2) ("M->" "End" 2)
                    ("C-<home>" "Home" 2) ("C-<end>" "End" 2)))
      (setq canvas-browser-test--commands nil)
      (canvas-browser-test--press (car case))
      (let ((down (car (canvas-browser-test--keys-sent))))
        (should (equal (list (car case) (plist-get down :key) (plist-get down :modifiers))
                       case))))))

(ert-deftest canvas-browser-the-mark-reaches-to-the-end-of-a-field ()
  ;; GIVEN a field with the mark set
  ;; WHEN M-> is pressed
  ;; THEN End goes to the page with Control and Shift held, which marks
  ;;      from the cursor to the end of the field
  (canvas-browser-test--in-page
    (canvas-browser-insert-mode)
    (canvas-browser-test--press "C-SPC")
    (setq canvas-browser-test--commands nil)
    (canvas-browser-test--press "M->")
    (let ((down (car (canvas-browser-test--keys-sent))))
      (should (equal (plist-get down :key) "End"))
      (should (equal (plist-get down :modifiers) 10)))))

(ert-deftest canvas-browser-c-x-h-marks-the-whole-field ()
  ;; GIVEN a page buffer typing into a field
  ;; WHEN C-x h is pressed
  ;; THEN the key a goes to the page with Control held, as a key that
  ;;      types nothing, with the code and the number of the key A: that
  ;;      marks all of a field in a browser, AND the mark is set, so that
  ;;      C-g drops the region before it leaves the field
  (canvas-browser-test--in-page
    (canvas-browser-insert-mode)
    (setq canvas-browser-test--commands nil)
    (canvas-browser-test--press "C-x h")
    (let ((down (car (canvas-browser-test--keys-sent))))
      (should (equal (plist-get down :type) "rawKeyDown"))
      (should (equal (plist-get down :key) "a"))
      (should (equal (plist-get down :code) "KeyA"))
      (should (equal (plist-get down :windowsVirtualKeyCode) 65))
      (should (equal (plist-get down :modifiers) 2)))
    (should canvas-browser--field-mark)))

(ert-deftest canvas-browser-the-keys-of-insert-state-are-bound-on-every-load ()
  ;; GIVEN a keymap of insert state from before a key was added, as a
  ;;       running Emacs keeps it when the file is loaded again
  ;; WHEN the keys of insert state are bound in it, as every load does
  ;; THEN the map has the keys, the new ones among them
  (let ((map (make-sparse-keymap)))
    (canvas-browser--bind-insert-keys map)
    (should (eq (keymap-lookup map "C-x h") 'canvas-browser-field-mark-whole))
    (should (eq (keymap-lookup map "M->") 'canvas-browser-send-key))
    (should (eq (keymap-lookup map "C-g") 'canvas-browser-insert-quit))))

;;;; A drag of the mouse

(defun canvas-browser-test--at (x y &optional off-the-page)
  "A mouse position at the pixel X Y of the page in the selected window.
With OFF-THE-PAGE, the position is in the window and not on the page."
  (list (selected-window) 1 '(0 . 0) 0 nil 1 '(0 . 0)
        (unless off-the-page canvas-browser--canvas)
        (cons x y) '(800 . 600)))

(defun canvas-browser-test--mouse-events ()
  "The mouse events sent to the page, oldest first, as (TYPE X Y)."
  (mapcar (lambda (event)
            (list (plist-get event :type) (plist-get event :x) (plist-get event :y)))
          (reverse (mapcar #'cdr (cl-remove "Input.dispatchMouseEvent"
                                            canvas-browser-test--commands
                                            :key #'car :test-not #'equal)))))

(ert-deftest canvas-browser-a-drag-reaches-the-page-as-a-press-a-move-and-a-release ()
  ;; GIVEN a page buffer
  ;; WHEN the mouse is dragged from the pixel 40 by 90 to 200 by 95
  ;; THEN the page gets a press at the first pixel, a move to the second
  ;;      with the left button held, and a release there: a browser marks
  ;;      what lies between
  (canvas-browser-test--in-page
    (setq canvas-browser-test--commands nil)
    (canvas-browser-drag (list 'drag-mouse-1 (canvas-browser-test--at 40 90)
                               (canvas-browser-test--at 200 95)))
    (should (equal (canvas-browser-test--mouse-events)
                   '(("mousePressed" 40 90) ("mouseMoved" 200 95) ("mouseReleased" 200 95))))
    (let ((move (cdr (cl-find "mouseMoved" canvas-browser-test--commands
                              :key (lambda (command) (plist-get (cdr command) :type))
                              :test #'equal))))
      (should (equal (plist-get move :buttons) 1)))))

(ert-deftest canvas-browser-a-drag-that-ends-off-the-page-is-refused ()
  ;; GIVEN a page buffer
  ;; WHEN a drag starts on the page AND ends on something else in the
  ;;      window, where there is no pixel of the page
  ;; THEN nothing goes to the page, AND the reader is told why
  (canvas-browser-test--in-page
    (setq canvas-browser-test--commands nil)
    (should-error
     (canvas-browser-drag (list 'drag-mouse-1 (canvas-browser-test--at 40 90)
                                (canvas-browser-test--at 0 0 t)))
     :type 'user-error)
    (should-not (canvas-browser-test--mouse-events))))

(ert-deftest canvas-browser-a-drag-is-bound-in-both-states ()
  ;; GIVEN a page buffer
  ;; WHEN the drag of the left button is looked up, in normal state and
  ;;      while typing into the page
  ;; THEN it runs the command that sends the drag to the page
  (canvas-browser-test--in-page
    (should (eq (key-binding [drag-mouse-1]) 'canvas-browser-drag))
    (canvas-browser-insert-mode)
    (should (eq (key-binding [drag-mouse-1]) 'canvas-browser-drag))))

;;;; The caret of the page

(ert-deftest canvas-browser-v-starts-the-caret ()
  ;; GIVEN a page buffer in normal state
  ;; WHEN v is pressed
  ;; THEN the caret has the keys, AND the page is told to put its caret
  ;;      after what has the focus, or at the first text in view
  (canvas-browser-test--in-page
    (should (eq (key-binding (kbd "v")) #'canvas-browser-caret-mode))
    (canvas-browser-test--answering '(:box (10 20 2 16) :text "")
      (canvas-browser-caret-mode))
    (should canvas-browser--caret)
    (should (eq (current-local-map) canvas-browser-caret-map))
    (let ((script (plist-get (canvas-browser-test--params "Runtime.evaluate") :expression)))
      (should (string-search "setStartAfter" script))
      (should (string-search "caretRangeFromPoint" script)))
    (should (equal canvas-browser--caret-box '(:x 10 :y 20 :w 2 :h 16)))))

(ert-deftest canvas-browser-the-motions-of-emacs-move-the-caret ()
  ;; GIVEN a page whose caret has the keys
  ;; WHEN the motions of Emacs are pressed
  ;; THEN each moves the caret of the page as far as it moves point
  (canvas-browser-test--in-page
    (canvas-browser-test--answering '(:box (10 20 2 16) :text "")
      (canvas-browser-caret-mode)
      (dolist (case '(("C-f" "move" "forward" "character") ("C-b" "move" "backward" "character")
                      ("M-f" "move" "forward" "word") ("M-b" "move" "backward" "word")
                      ("C-n" "move" "forward" "line") ("C-p" "move" "backward" "line")
                      ("C-a" "move" "backward" "lineboundary") ("C-e" "move" "forward" "lineboundary")
                      ("<right>" "move" "forward" "character") ("<down>" "move" "forward" "line")))
        (canvas-browser-test--press (car case))
        (should (string-search (apply #'format "move('%s', '%s', '%s')" (cdr case))
                               (plist-get (canvas-browser-test--params "Runtime.evaluate")
                                          :expression)))))))

(ert-deftest canvas-browser-the-caret-marks-and-copies ()
  ;; GIVEN a page whose caret has the keys
  ;; WHEN C-SPC is pressed, then M-f, then M-w
  ;; THEN M-f extends the region, M-w puts its text in the kill ring and
  ;;      pulses it, AND the mark is gone
  (canvas-browser-test--in-page
    (canvas-browser-test--pulses pulses
      (let ((kill-ring nil))
        (canvas-browser-test--answering '(:box (10 20 2 16) :text " words" :region (10 20 50 16))
          (canvas-browser-caret-mode)
          (canvas-browser-test--press "C-SPC")
          (canvas-browser-test--press "M-f")
          (should (string-search "move('extend', 'forward', 'word')"
                                 (plist-get (canvas-browser-test--params "Runtime.evaluate")
                                            :expression)))
          (canvas-browser-test--press "M-w"))
        (should (equal (car kill-ring) " words"))
        (should (equal pulses '((:x 10 :y 20 :w 50 :h 16))))
        (should-not canvas-browser--caret-mark)))))

(ert-deftest canvas-browser-c-g-drops-the-caret-mark-then-the-caret ()
  ;; GIVEN a page whose caret marks a region
  ;; WHEN C-g is pressed, and then again
  ;; THEN the first drops the mark, AND the second leaves the caret
  (canvas-browser-test--in-page
    (canvas-browser-test--answering '(:box (10 20 2 16) :text "")
      (canvas-browser-caret-mode)
      (canvas-browser-test--press "C-SPC")
      (canvas-browser-test--press "C-g")
      (should-not canvas-browser--caret-mark)
      (should canvas-browser--caret)
      (canvas-browser-test--press "C-g")
      (should-not canvas-browser--caret)
      (should (eq (current-local-map) canvas-browser-mode-map)))))

(ert-deftest canvas-browser-the-eye-follows-the-caret ()
  ;; GIVEN a page whose caret stands at 10,20
  ;; WHEN it moves to 60,20
  ;; THEN the eye is drawn from the one place to the other
  (canvas-browser-test--in-page
    (canvas-browser-test--flights flights
      (canvas-browser-test--answering '(:box (10 20 2 16) :text "")
        (canvas-browser-caret-mode))
      (canvas-browser-test--answering '(:box (60 20 2 16) :text "")
        (canvas-browser-test--press "M-f"))
      (should (equal (car flights) '((:x 10 :y 20 :w 2 :h 16) (:x 60 :y 20 :w 2 :h 16)))))))

(ert-deftest canvas-browser-the-caret-shows-on-a-light-page-and-a-dark-one ()
  ;; GIVEN a white cursor, and then a black one
  ;; WHEN the colours of the caret of the page are made
  ;; THEN it is drawn in the colour of the cursor with an edge of the
  ;;      other: a white bar alone vanishes on a white page
  (cl-letf (((symbol-function 'face-background) (lambda (&rest _) "#ffffff")))
    (should (equal (canvas-browser--caret-colours) '("#ffffff" "#000000"))))
  (cl-letf (((symbol-function 'face-background) (lambda (&rest _) "#000000")))
    (should (equal (canvas-browser--caret-colours) '("#000000" "#ffffff"))))
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'face-background) (lambda (&rest _) "#ffffff")))
      (canvas-browser-test--answering '(:box (10 20 2 16) :text "")
        (canvas-browser-caret-mode))
      (should (string-search "start(\"#ffffff\", \"#000000\")"
                             (plist-get (canvas-browser-test--params "Runtime.evaluate")
                                        :expression))))))

;;;; Jumping to text, as avy does

(defun canvas-browser-test--reading-chars (chars)
  "Read jump text from CHARS, nil meaning a pause; the text read."
  (cl-letf (((symbol-function 'read-char) (lambda (&rest _) (pop chars))))
    (canvas-browser--read-jump-text)))

(ert-deftest canvas-browser-jump-text-is-read-until-a-pause ()
  ;; GIVEN the keys typed after M-j
  ;; WHEN they are read
  ;; THEN they are read until a pause, as avy-goto-char-timer reads them;
  ;;      DEL takes the last back, RET ends at once, AND ESC gives up
  (should (equal (canvas-browser-test--reading-chars (list ?w ?o nil)) "wo"))
  (should (equal (canvas-browser-test--reading-chars (list ?w ?x ?\d ?o nil)) "wo"))
  (should (equal (canvas-browser-test--reading-chars (list ?w ?\r)) "w"))
  (should-not (canvas-browser-test--reading-chars (list ?w ?\e))))

(defmacro canvas-browser-test--jumping (text chosen &rest body)
  "Run BODY in a page that finds two places of TEXT and names CHOSEN."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'canvas-browser--read-jump-text) (lambda () ,text))
             ((symbol-function 'canvas-browser--draw-hints) #'ignore)
             ((symbol-function 'canvas-browser--paint-window) #'ignore)
             ((symbol-function 'canvas-browser--read-hint) (lambda (&rest _) ,chosen))
             ((symbol-function 'canvas-browser-cdp-send)
              (lambda (method params &optional answer _session)
                (push (cons method params) canvas-browser-test--commands)
                (when answer
                  (let ((expression (or (plist-get params :expression) "")))
                    (funcall answer
                             (list :result
                                   (list :value
                                         (cond ((string-search "__canvasBrowserCaret.find(" expression)
                                                '((:x 1 :y 2 :w 30 :h 16) (:x 10 :y 60 :w 30 :h 16)))
                                               (t '(:box (10 60 2 16) :text "")))))))))))
     ,@body))

(defun canvas-browser-test--last-script ()
  "The last script sent to the page."
  (plist-get (canvas-browser-test--params "Runtime.evaluate") :expression))

(ert-deftest canvas-browser-m-j-puts-the-caret-on-the-text-named ()
  ;; GIVEN a page in normal state, where "sec" shows twice
  ;; WHEN M-j is pressed, "sec" typed, and the second place named
  ;; THEN the page is asked for the places of "sec", the caret starts,
  ;;      AND it jumps to the second place, where the eye is drawn
  (canvas-browser-test--in-page
    (should (eq (key-binding (kbd "M-j")) #'canvas-browser-caret-jump))
    (canvas-browser-test--jumping "sec" (list 1)
      (canvas-browser-caret-jump)
      (should (cl-some (lambda (command)
                         (string-search ".find(\"sec\")"
                                        (or (plist-get (cdr command) :expression) "")))
                       canvas-browser-test--commands))
      (should (string-search ".jump(1, false)" (canvas-browser-test--last-script)))
      (should canvas-browser--caret)
      (should (eq (current-local-map) canvas-browser-caret-map))
      (should (equal canvas-browser--caret-box '(:x 10 :y 60 :w 2 :h 16))))))

(ert-deftest canvas-browser-m-j-to-a-single-place-jumps-at-once ()
  ;; GIVEN a page where the text typed shows once
  ;; WHEN M-j looks for it
  ;; THEN the caret goes there without a hint, as a single candidate of
  ;;      avy is jumped to at once
  (canvas-browser-test--in-page
    (let ((hinted nil))
      (cl-letf (((symbol-function 'canvas-browser--read-jump-text) (lambda () "parser"))
                ((symbol-function 'canvas-browser--read-hint)
                 (lambda (&rest _) (setq hinted t) nil)))
        (canvas-browser-test--answering '((:x 1 :y 2 :w 30 :h 16))
          (canvas-browser-caret-jump)))
      (should-not hinted)
      (should (string-search ".jump(0, false)" (canvas-browser-test--last-script))))))

(ert-deftest canvas-browser-the-menu-names-the-jump ()
  ;; GIVEN the menu of a page buffer
  ;; WHEN its entries are read
  ;; THEN M-j stands in it as "jump", beside the hints of f
  (should (equal (plist-get (canvas-browser-test--menu-entry "M-j") :description) "jump"))
  (should (eq (plist-get (canvas-browser-test--menu-entry "M-j") :command)
              'canvas-browser-caret-jump)))

(ert-deftest canvas-browser-the-key-of-avy-jumps-in-the-page ()
  ;; GIVEN a page buffer, and avy-goto-char-timer on M-j in a map that
  ;;       beats the map of the buffer, as `bind-key*' puts it
  ;; WHEN M-j is looked up
  ;; THEN it jumps in the page: the text of a page buffer is no text avy
  ;;      can see, so its key does in the page what avy does in text
  (canvas-browser-test--in-page
    (let* ((map (define-keymap "M-j" #'avy-goto-char-timer))
           (emulation-mode-map-alists (list (list (cons t map)))))
      (should (eq (key-binding (kbd "M-j")) #'canvas-browser-caret-jump)))))

(ert-deftest canvas-browser-m-j-with-the-mark-marks-to-the-text-named ()
  ;; GIVEN a caret with the mark set
  ;; WHEN M-j jumps to a place
  ;; THEN the region reaches to that place, as a jump of avy does
  (canvas-browser-test--in-page
    (canvas-browser-test--jumping "sec" (list 0)
      (canvas-browser-caret-mode)
      (setq canvas-browser--caret-mark t)
      (canvas-browser-caret-jump)
      (should (string-search ".jump(0, true)" (canvas-browser-test--last-script))))))

(ert-deftest canvas-browser-m-j-that-is-given-up-leaves-no-caret ()
  ;; GIVEN a page in normal state
  ;; WHEN M-j is pressed and ESC given at the hints
  ;; THEN the caret does not start: nothing was jumped to
  (canvas-browser-test--in-page
    (canvas-browser-test--jumping "sec" nil
      (canvas-browser-caret-jump)
      (should-not canvas-browser--caret)
      (should (eq (current-local-map) canvas-browser-mode-map)))))

(ert-deftest canvas-browser-m-j-says-when-nothing-matches ()
  ;; GIVEN a page where the text typed shows nowhere
  ;; WHEN M-j looks for it
  ;; THEN the reader is told, and no hints are read
  (canvas-browser-test--in-page
    (let ((said nil) (hinted nil))
      (cl-letf (((symbol-function 'canvas-browser--read-jump-text) (lambda () "zzz"))
                ((symbol-function 'canvas-browser--read-hint)
                 (lambda (&rest _) (setq hinted t) nil))
                ((symbol-function 'message)
                 (lambda (format &rest args) (setq said (apply #'format format args)))))
        (canvas-browser-test--answering nil
          (canvas-browser-caret-jump))
        (should (string-search "zzz" said))
        (should-not hinted)))))

;;;; Blocking ads with uBlock Origin Lite

(defun canvas-browser-test--release ()
  "A release of uBlock Origin Lite as GitHub describes it."
  '((tag_name . "2026.1.1")
    (assets . (((name . "uBOLite_2026.1.1.edge.zip")
                (browser_download_url . "https://example.org/edge.zip"))
               ((name . "uBOLite_2026.1.1.chromium.zip")
                (browser_download_url . "https://example.org/chromium.zip"))
               ((name . "uBOLite_2026.1.1.firefox.signed.xpi")
                (browser_download_url . "https://example.org/firefox.xpi"))))))

(ert-deftest canvas-browser-ublock-asset-is-the-chromium-zip ()
  ;; GIVEN a release with a zip for each browser, and one without chromium's
  ;; WHEN the file to install is picked
  ;; THEN it is the chromium zip, AND a release without one is an error
  (should (equal '("uBOLite_2026.1.1.chromium.zip" . "https://example.org/chromium.zip")
                 (canvas-browser--ublock-asset (canvas-browser-test--release))))
  (should-error (canvas-browser--ublock-asset '((tag_name . "x") (assets . ())))))

(defmacro canvas-browser-test--installing (directory fetched &rest body)
  "Run BODY with the network and unzip stubbed and DIRECTORY for extensions.
FETCHED is bound to the URLs downloaded.  unzip writes a manifest and a
file that names the zip, into the directory it is given."
  (declare (indent 2))
  `(let* ((,directory (make-temp-file "canvas-browser-extensions" t))
          (canvas-browser-extension-directory ,directory)
          (,fetched nil))
     (unwind-protect
         (cl-letf (((symbol-function 'canvas-browser--read-json-url)
                    (lambda (_url) (canvas-browser-test--release)))
                   ((symbol-function 'url-copy-file)
                    (lambda (url file &rest _)
                      (push url ,fetched)
                      (with-temp-file file (insert "zip"))))
                   ((symbol-function 'executable-find) (lambda (name) (concat "/usr/bin/" name)))
                   ((symbol-function 'call-process)
                    (lambda (program _in _out _show &rest args)
                      (should (equal "unzip" program))
                      (let ((into (car (last args))))
                        (with-temp-file (expand-file-name "manifest.json" into) (insert "{}"))
                        (with-temp-file (expand-file-name "new" into) (insert "new")))
                      0))
                   ((symbol-function 'canvas-browser-cdp-running-p) (lambda () nil)))
           ,@body)
       (delete-directory ,directory t))))

(ert-deftest canvas-browser-install-ublock-replaces-the-older-one ()
  ;; GIVEN an older uBlock Origin Lite among the extensions
  ;; WHEN the newest one is installed
  ;; THEN its chromium zip is fetched and unpacked in place of the older
  ;;      one, AND nothing half unpacked is left behind
  (canvas-browser-test--installing directory fetched
    (let ((old (expand-file-name "ublock-origin-lite" directory)))
      (make-directory old)
      (with-temp-file (expand-file-name "old" old) (insert "old"))
      (canvas-browser-install-ublock)
      (should (equal '("https://example.org/chromium.zip") fetched))
      (should (file-exists-p (expand-file-name "manifest.json" old)))
      (should (file-exists-p (expand-file-name "new" old)))
      (should-not (file-exists-p (expand-file-name "old" old)))
      (should (equal '("ublock-origin-lite")
                     (directory-files directory nil "\\`[^.]\\|\\`\\.[^.]"))))))

(ert-deftest canvas-browser-restart-chromium-opens-the-shown-pages-again ()
  ;; GIVEN two pages, one shown in a window and one not
  ;; WHEN chromium is restarted, as a new extension needs
  ;; THEN chromium stops, both pages forget their old sessions, AND the
  ;;      shown page is opened again at once in the new chromium
  (canvas-browser-test--with-chromium
    (let ((shown (generate-new-buffer " *shown*"))
          (hidden (generate-new-buffer " *hidden*"))
          (stopped nil))
      (unwind-protect
          (progn
            (dolist (buffer (list shown hidden))
              (with-current-buffer buffer
                (canvas-browser-mode)
                (canvas-browser--open "https://example.org" 800 600)))
            (setq canvas-browser-test--commands nil)
            (cl-letf (((symbol-function 'canvas-browser-cdp-stop) (lambda () (setq stopped t)))
                      ((symbol-function 'get-buffer-window)
                       (lambda (buffer &rest _) (and (eq buffer shown) 'a-window))))
              (canvas-browser-restart-chromium))
            (should stopped)
            (should-not (buffer-local-value 'canvas-browser--session hidden))
            (should (equal "S1" (buffer-local-value 'canvas-browser--session shown)))
            (should (equal 1 (cl-count "Target.createTarget" canvas-browser-test--commands
                                       :key #'car :test #'equal))))
        (kill-buffer shown)
        (kill-buffer hidden)))))

(ert-deftest canvas-browser-restart-chromium-shows-an-embedded-page-in-its-host ()
  ;; GIVEN a page embedded in a host buffer that a window shows
  ;; WHEN chromium is restarted
  ;; THEN the page opens again, AND the host shows the page's new canvas
  (canvas-browser-test--embedded page text host
    (with-current-buffer host (insert "before " text " after"))
    (let ((old (buffer-local-value 'canvas-browser--canvas page)))
      (cl-letf (((symbol-function 'canvas-browser-cdp-stop) #'ignore)
                ((symbol-function 'get-buffer-window)
                 (lambda (buffer &rest _) (and (eq buffer host) 'a-window))))
        (canvas-browser-restart-chromium))
      (let ((new (buffer-local-value 'canvas-browser--canvas page)))
        (should-not (eq old new))
        (with-current-buffer host
          (should (eq new (get-text-property
                           (text-property-not-all (point-min) (point-max)
                                                  'canvas-browser-embed nil)
                           'display))))))))

(ert-deftest canvas-browser-read-json-url-asks-for-a-fresh-answer ()
  ;; GIVEN url.el caching turned on by another package, and a server
  ;;       that answers 200 with JSON
  ;; WHEN the JSON of a URL is read
  ;; THEN the request asks for no cached copy and keeps none, since url.el
  ;;      sends If-Modified-Since for a cached URL and a 304 has no body,
  ;;      AND the JSON comes back as alists
  (let ((url-automatic-caching t)
        (asked nil))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _)
                 (setq asked (list url-automatic-caching url-request-extra-headers))
                 (let ((buffer (generate-new-buffer " *answer*")))
                   (with-current-buffer buffer
                     (insert "HTTP/1.1 200 OK\nContent-Type: application/json\n\n{\"tag_name\": \"1\"}")
                     (setq-local url-http-response-status 200))
                   buffer))))
      (should (equal '((tag_name . "1"))
                     (canvas-browser--read-json-url "https://example.org/release")))
      (should-not (car asked))
      (should (equal "no-cache" (cdr (assoc "Pragma" (cadr asked))))))))

(ert-deftest canvas-browser-install-ublock-downloads-a-fresh-copy ()
  ;; GIVEN url.el caching turned on by another package
  ;; WHEN uBlock Origin Lite is installed
  ;; THEN the zip is downloaded asking for no cached copy, and keeping none
  (let ((url-automatic-caching t)
        (asked nil))
    (canvas-browser-test--installing directory fetched
      (cl-letf* ((copy (symbol-function 'url-copy-file))
                 ((symbol-function 'url-copy-file)
                  (lambda (&rest args)
                    (setq asked (list url-automatic-caching url-request-extra-headers))
                    (apply copy args))))
        (canvas-browser-install-ublock)
        (should-not (car asked))
        (should (equal "no-cache" (cdr (assoc "Pragma" (cadr asked)))))))))

;;;; Bookmarks

(require 'bookmark)

;; consult is not loaded here; its variable is bound as consult binds it.
(defvar consult-bookmark-narrow)

(defun canvas-browser-test--bookmark (name url)
  "A bookmark called NAME of the page at URL, as canvas-browser makes one."
  `(,name (location . ,url) (handler . canvas-browser-bookmark-jump)))

(ert-deftest canvas-browser-bookmark-records-the-page ()
  ;; GIVEN a page buffer whose page has a title
  ;; WHEN a bookmark record is made of it, as `bookmark-set' makes one
  ;; THEN it is named by the title, holds the address, and opens through
  ;;      canvas-browser, AND the address is offered as a name as well
  (canvas-browser-test--in-page
    (setq canvas-browser--title "Example Domain")
    (let ((record (bookmark-make-record)))
      (should (equal "Example Domain" (car record)))
      (should (equal "https://example.org" (bookmark-prop-get record 'location)))
      (should (eq 'canvas-browser-bookmark-jump (bookmark-prop-get record 'handler)))
      (should (member "https://example.org" (bookmark-prop-get record 'defaults))))))

(ert-deftest canvas-browser-bookmark-of-a-page-without-an-address-is-an-error ()
  ;; GIVEN a page buffer with no address yet
  ;; WHEN a bookmark record is made of it
  ;; THEN it is an error rather than a bookmark that opens nothing
  (with-temp-buffer
    (canvas-browser-mode)
    (should-error (canvas-browser-bookmark-make-record))))

(ert-deftest canvas-browser-bookmark-jump-opens-the-page ()
  ;; GIVEN a bookmark of a page that no buffer shows
  ;; WHEN it is jumped to
  ;; THEN a page buffer opens at its address, and is the buffer shown
  (canvas-browser-test--with-chromium
    (let ((bookmark-alist (list (canvas-browser-test--bookmark "Example" "https://example.org/a")))
          (opened nil))
      (unwind-protect
          (progn
            (bookmark-jump "Example")
            (setq opened (current-buffer))
            (should (eq 'canvas-browser-mode (buffer-local-value 'major-mode opened)))
            (should (equal "https://example.org/a" (buffer-local-value 'canvas-browser--url opened)))
            (should (equal "https://example.org/a"
                           (plist-get (canvas-browser-test--params "Page.navigate") :url))))
        (when (buffer-live-p opened) (kill-buffer opened))))))

(ert-deftest canvas-browser-bookmark-jump-goes-to-a-page-already-open ()
  ;; GIVEN a page buffer that shows the address of a bookmark
  ;; WHEN the bookmark is jumped to
  ;; THEN that buffer is the one shown, AND no page opens
  (canvas-browser-test--in-page
    (let ((page (current-buffer))
          (bookmark-alist (list (canvas-browser-test--bookmark "Example" "https://example.org"))))
      (setq canvas-browser-test--commands nil)
      (with-temp-buffer
        (bookmark-jump "Example")
        (should (eq page (current-buffer))))
      (should-not (assoc "Target.createTarget" canvas-browser-test--commands)))))

(ert-deftest canvas-browser-bookmarks-are-web-bookmarks-to-consult ()
  ;; GIVEN consult's narrowing groups of bookmarks
  ;; WHEN canvas-browser joins them, twice
  ;; THEN its bookmarks are in the Web group, once, AND the other groups
  ;;      are as they were
  (let ((consult-bookmark-narrow '((?f "File" bookmark-default-handler)
                                   (?w "Web" eww-bookmark-jump))))
    (canvas-browser--join-consult-web-group)
    (canvas-browser--join-consult-web-group)
    (should (equal '((?f "File" bookmark-default-handler)
                     (?w "Web" eww-bookmark-jump canvas-browser-bookmark-jump))
                   consult-bookmark-narrow))))

(ert-deftest canvas-browser-open-bookmark-offers-only-the-pages ()
  ;; GIVEN a bookmark of a page and a bookmark of a file
  ;; WHEN a bookmark is picked to open
  ;; THEN only the page is offered, as bookmarks, AND the page opens
  (canvas-browser-test--in-page
    (let ((page (current-buffer))
          (bookmark-alist (list (canvas-browser-test--bookmark "Example" "https://example.org")
                                '("notes" (filename . "/tmp/notes.org"))))
          (offered nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (setq offered (list (all-completions "" collection)
                                       (completion-metadata-get
                                        (completion-metadata "" collection nil)
                                        'category)))
                   "Example")))
        (with-temp-buffer
          (canvas-browser-open-bookmark)
          (should (eq page (current-buffer)))))
      (should (equal '(("Example") bookmark) offered)))))

(ert-deftest canvas-browser-bookmark-keys ()
  ;; GIVEN a page buffer and its menu
  ;; WHEN B and J are looked up
  ;; THEN B keeps the page as a bookmark and J opens one, in the buffer
  ;;      and in the menu alike
  (canvas-browser-test--in-page
    (should (eq 'canvas-browser-bookmark (key-binding (kbd "B"))))
    (should (eq 'canvas-browser-open-bookmark (key-binding (kbd "J")))))
  (should (eq 'canvas-browser-bookmark
              (plist-get (canvas-browser-test--menu-entry "B") :command)))
  (should (eq 'canvas-browser-open-bookmark
              (plist-get (canvas-browser-test--menu-entry "J") :command))))

