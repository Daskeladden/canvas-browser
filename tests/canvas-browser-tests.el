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
                (lambda (method params &optional answer _session &rest _)
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

(ert-deftest canvas-browser-the-keys-of-normal-state-are-bound-on-every-load ()
  ;; GIVEN a keymap of normal state that lacks a key, as a running Emacs
  ;;       has it when the key came to the file after the package loaded
  ;; WHEN the file is loaded again
  ;; THEN the map has the key
  (let ((command (keymap-lookup canvas-browser-mode-map "o")))
    (keymap-unset canvas-browser-mode-map "o" t)
    (unwind-protect
        (progn
          (load (locate-library "canvas-browser.el") nil t)
          (should (eq (keymap-lookup canvas-browser-mode-map "o")
                      #'canvas-browser-open-url)))
      (keymap-set canvas-browser-mode-map "o" command))))

(ert-deftest canvas-browser-y-copies-the-address-of-the-page ()
  ;; GIVEN a page buffer that shows https://example.org
  ;; WHEN y is pressed
  ;; THEN the address is the newest kill
  (canvas-browser-test--in-page
    (let ((kill-ring nil))
      (should (eq (key-binding (kbd "y")) #'canvas-browser-copy-url))
      (canvas-browser-test--press "y")
      (should (equal (car kill-ring) "https://example.org")))))

(ert-deftest canvas-browser-the-menu-copies-the-address-of-the-page ()
  ;; GIVEN the menu of a page buffer
  ;; WHEN its entry on y is read
  ;; THEN it runs the command that copies the address of the page
  (should (eq (plist-get (canvas-browser-test--menu-entry "y") :command)
              'canvas-browser-copy-url)))

(ert-deftest canvas-browser-a-page-without-an-address-copies-none ()
  ;; GIVEN a page buffer with no address yet
  ;; WHEN y is pressed
  ;; THEN it is an error, AND the kill ring is as it was
  (with-temp-buffer
    (canvas-browser-mode)
    (let ((kill-ring nil))
      (should-error (canvas-browser-copy-url) :type 'user-error)
      (should-not kill-ring))))

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
               (lambda (_method _params &optional answer _session &rest _)
                 (when answer (funcall answer '(:result (:value "one\ntwo"))))))
              ((symbol-function 'display-buffer) #'ignore))
      (canvas-browser-text))
    (let ((text-buffer (canvas-browser--text-buffer)))
      (unwind-protect
          (with-current-buffer text-buffer
            (should (equal (buffer-string) "one\ntwo"))
            (should (= (point) (point-min))))
        (kill-buffer text-buffer)))))

(defmacro canvas-browser-test--with-text (text located &rest body)
  "Run BODY in the text buffer of a page whose text is TEXT.
The page answers LOCATED when it is asked where a line stands.  `page\='
is the page buffer, and `shown\=' the buffer that was popped to last."
  (declare (indent 2))
  `(canvas-browser-test--in-page
     (let ((page (current-buffer))
           (shown nil))
       (cl-letf (((symbol-function 'canvas-browser-cdp-send)
                  (lambda (method params &optional answer _session &rest _)
                    (push (cons method params) canvas-browser-test--commands)
                    (when answer
                      (funcall answer
                               (list :result
                                     (list :value
                                           (if (string-search
                                                "wanted" (or (plist-get params :expression) ""))
                                               ,located
                                             ,text)))))))
                 ((symbol-function 'display-buffer) #'ignore)
                 ((symbol-function 'pop-to-buffer)
                  (lambda (buffer &rest _) (setq shown buffer) (set-buffer buffer))))
         (canvas-browser-text)
         (let ((text-buffer (canvas-browser--text-buffer)))
           (unwind-protect
               (with-current-buffer text-buffer ,@body)
             (kill-buffer text-buffer)))))))

(defun canvas-browser-test--asked-for ()
  "The script that last asked the page where a line stands."
  (plist-get (canvas-browser-test--params "Runtime.evaluate") :expression))

(defun canvas-browser-test--show-line (line)
  "Press `M-.\=' on LINE of this text buffer; what the reader is told.
The key must be the one of the text buffer: the key of xref would wait
for an answer that a test cannot give."
  (should (eq (key-binding (kbd "M-.")) 'canvas-browser-text-show-in-page))
  (goto-char (point-min))
  (forward-line (1- line))
  (let ((said nil))
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq said (apply #'format format-string args)))))
      (call-interactively (key-binding (kbd "M-."))))
    said))

(ert-deftest canvas-browser-a-line-of-the-text-is-shown-in-its-page ()
  ;; GIVEN the text of a page, where the line Reply stands twice
  ;; WHEN M-. is pressed on the second one
  ;; THEN the page is asked for that line as the second time it stands
  ;;      there, in the whole page, AND the page buffer is shown, AND the
  ;;      reader is told which hit of how many it is
  (canvas-browser-test--with-text "Reply\nHello there\nReply\n" '(:found 0 :count 2 :rank 1)
    (let ((said (canvas-browser-test--show-line 3)))
      (should (eq shown page))
      (should (string-search "(document.body, [{\"text\":\"Reply\",\"rank\":1}], "
                             (canvas-browser-test--asked-for)))
      (should (string-search "2 of 2" said)))))

(ert-deftest canvas-browser-a-line-shown-from-the-text-is-painted-for-a-moment ()
  ;; GIVEN the text of a page
  ;; WHEN M-. shows a line of it in the page
  ;; THEN the page is told to paint the line for a moment only, a second
  ;;      and a half: the reader came to see where it is, not to keep it
  ;;      marked
  (canvas-browser-test--with-text "Reply\n" '(:found 0 :count 1 :rank 0)
    (canvas-browser-test--show-line 1)
    (should (string-suffix-p "}], 1500)" (canvas-browser-test--asked-for)))))

(ert-deftest canvas-browser-the-way-back-from-the-page-is-the-key-of-xref ()
  ;; GIVEN the text of a page, with point on its third line
  ;; WHEN M-. is pressed, and then the key that goes back in xref
  ;; THEN point is on that line of the text buffer again
  (canvas-browser-test--with-text "Reply\nHello there\nReply\n" '(:found 0 :count 2 :rank 1)
    (let ((text-buffer (current-buffer)))
      (canvas-browser-test--show-line 3)
      (set-buffer page)
      (xref-go-back)
      (should (eq (current-buffer) text-buffer))
      (should (= (line-number-at-pos) 3)))))

(ert-deftest canvas-browser-a-line-is-asked-for-without-its-white-space ()
  ;; GIVEN the text of a page with a line that is indented and has two
  ;;       spaces between its words, and a line after it
  ;; WHEN M-. is pressed on the first
  ;; THEN the page is asked for the words with one space between them,
  ;;      AND for the line after it as a second try
  (canvas-browser-test--with-text "  Hello   there \nGood day\n" '(:found 0 :count 1 :rank 0)
    (canvas-browser-test--show-line 1)
    (should (string-search (concat "{\"text\":\"Hello there\",\"rank\":0},"
                                   "{\"text\":\"Good day\",\"rank\":0}]")
                           (canvas-browser-test--asked-for)))))

(ert-deftest canvas-browser-an-empty-line-of-the-text-shows-the-line-after-it ()
  ;; GIVEN the text of a page with an empty line between two lines
  ;; WHEN M-. is pressed on the empty line
  ;; THEN the page is asked for the line after it
  (canvas-browser-test--with-text "one\n\ntwo\n" '(:found 0 :count 1 :rank 0)
    (canvas-browser-test--show-line 2)
    (should (string-search "[{\"text\":\"two\",\"rank\":0}]"
                           (canvas-browser-test--asked-for)))))

(ert-deftest canvas-browser-the-last-line-of-the-text-is-shown-from-its-end ()
  ;; GIVEN the text of a page that ends without a line break, with point
  ;;       at its end, where a search for the last line leaves it
  ;; WHEN M-. is pressed
  ;; THEN the page is asked for the last line
  (canvas-browser-test--with-text "one\ntwo" '(:found 0 :count 1 :rank 0)
    (should (eq (key-binding (kbd "M-.")) 'canvas-browser-text-show-in-page))
    (goto-char (point-max))
    (canvas-browser-text-show-in-page)
    (should (string-search "[{\"text\":\"two\",\"rank\":0}]"
                           (canvas-browser-test--asked-for)))))

(defun canvas-browser-test--picture (text)
  "TEXT as the text of a page has what stands for a picture."
  (propertize text 'canvas-browser-for-picture t))

(defmacro canvas-browser-test--ways (&rest text)
  "The ways to ask a page for the first line of TEXT and the lines after it.
TEXT is strings, which stand in a buffer one after the other.  Each way
is (TEXT RANK LINE)."
  `(with-temp-buffer
     (insert ,@text)
     (goto-char (point-min))
     (mapcar (lambda (way)
               (list (plist-get way :text) (plist-get way :rank) (plist-get way :line)))
             (seq-remove (lambda (way) (plist-get way :paint))
                         (canvas-browser--lines-to-find)))))

(defmacro canvas-browser-test--stretches (&rest text)
  "The ways to ask a page for the first line of TEXT with the lines after it.
Each is (TEXT RANK PAINT): PAINT is how much of it the page paints."
  `(with-temp-buffer
     (insert ,@text)
     (goto-char (point-min))
     (mapcar (lambda (way)
               (list (plist-get way :text) (plist-get way :rank) (plist-get way :paint)))
             (seq-filter (lambda (way) (plist-get way :paint))
                         (canvas-browser--lines-to-find)))))

(ert-deftest canvas-browser-a-line-is-first-asked-for-with-the-lines-after-it ()
  ;; GIVEN a text whose first line is a name that the page holds in many
  ;;       places, with the words of a message on the line after it, as
  ;;       a message that starts with a mention is written
  ;; WHEN the ways to ask the page for the first line are made
  ;; THEN the first ways are the line with the lines after it as one
  ;;      line, which the page holds once: all of them first, then one
  ;;      line fewer, because a page can have other text between two
  ;;      messages, AND of each the page paints the line itself only,
  ;;      AND the line alone comes after them
  (should (equal (canvas-browser-test--stretches "Kim Doe\nI saw the light was green\n\nmore\n")
                 '(("Kim Doe I saw the light was green more" 0 7)
                   ("Kim Doe I saw the light was green" 0 7))))
  (should (equal (car (canvas-browser-test--ways "Kim Doe\nI saw the light was green\n"))
                 '("Kim Doe" 0 0))))

(ert-deftest canvas-browser-a-long-stretch-is-cut-at-a-word ()
  ;; GIVEN a text whose lines after the first are longer than a stretch
  ;;       may be
  ;; THEN the stretch ends at a word, within its length, AND a stretch
  ;;      of fewer lines that is cut to the same text is not asked for
  ;;      again
  (let ((canvas-browser--locate-stretch 20))
    (should (equal (canvas-browser-test--stretches "Kim Doe\nI saw the light was green\n")
                   '(("Kim Doe I saw the" 0 7))))
    (should (equal (canvas-browser-test--stretches "Kim Doe\nI saw the light was green\nmore\n")
                   '(("Kim Doe I saw the" 0 7))))))

(ert-deftest canvas-browser-a-line-with-nothing-after-it-has-no-stretch ()
  ;; GIVEN a text of one line, AND a text where point is on an empty line
  ;; THEN the page is asked for no stretch
  (should-not (canvas-browser-test--stretches "Kim Doe\n"))
  (should-not (with-temp-buffer
                (insert "one\n\ntwo\nthree\n")
                (goto-char (point-min))
                (forward-line 1)
                (seq-filter (lambda (way) (plist-get way :paint))
                            (canvas-browser--lines-to-find)))))

(ert-deftest canvas-browser-a-stretch-is-also-asked-for-without-its-pictures ()
  ;; GIVEN a text whose first line holds a picture, with a line after it
  ;; THEN the stretch is asked for as it is written, AND without the
  ;;      picture, of which the page paints the words of the line
  (should (equal (canvas-browser-test--stretches
                  (canvas-browser-test--picture "[point]") " Hello there\nNext\n")
                 '(("[point] Hello there Next" 0 19) ("Hello there Next" 0 11)))))

(ert-deftest canvas-browser-a-line-with-a-picture-is-also-asked-for-without-it ()
  ;; GIVEN a text where a line holds two pictures, one named in brackets
  ;;       and one an emoji that stands as itself, as the text of a page
  ;;       has what an application draws as a picture
  ;; WHEN the ways to ask the page for that line are made
  ;; THEN the first is the line as it is written, AND the second its
  ;;      words without what stands for a picture, AND then comes the
  ;;      line after it
  (should (equal (canvas-browser-test--ways
                  (canvas-browser-test--picture "[point]") " Hello there "
                  (canvas-browser-test--picture "🙁") "\nNext\n")
                 '(("[point] Hello there 🙁" 0 0) ("Hello there" 0 0) ("Next" 0 1)))))

(ert-deftest canvas-browser-the-rank-of-a-line-without-its-pictures-counts-the-words ()
  ;; GIVEN a text where the same words stand on two lines, the second
  ;;       time after a picture
  ;; WHEN the ways to ask for the second line are made
  ;; THEN its words without the picture are the second time that they
  ;;      stand there
  (should (equal (with-temp-buffer
                   (insert "Hello there\n" (canvas-browser-test--picture "[image]")
                           " Hello there\n")
                   (forward-line -1)
                   (mapcar (lambda (way) (list (plist-get way :text) (plist-get way :rank)))
                           (canvas-browser--lines-to-find)))
                 '(("[image] Hello there" 0) ("Hello there" 1)))))

(ert-deftest canvas-browser-the-rank-of-a-line-is-counted-without-white-space ()
  ;; GIVEN a text where the letters of a line stand before it too, with
  ;;       other white space between them: on two lines there and in one
  ;;       word here, AND the other way round
  ;; WHEN the way to ask for the line is made
  ;; THEN it is the second time that the line stands there: the page
  ;;      compares without regard to white space, so the rank must too
  (should (equal (canvas-browser--to-find "DeeFox" "Dee\nFox\n\n")
                 '(:text "DeeFox" :rank 1)))
  (should (equal (canvas-browser--to-find "Dee Fox" "DeeFox\n")
                 '(:text "Dee Fox" :rank 1))))

(ert-deftest canvas-browser-a-line-that-is-only-a-picture-is-asked-for-once ()
  ;; GIVEN a text with a line that is a picture and no words
  ;; THEN the page is asked for the line as it is written, and not for
  ;;      nothing
  (should (equal (canvas-browser-test--ways (canvas-browser-test--picture "[image]") "\nNext\n")
                 '(("[image]" 0 0) ("Next" 0 1)))))

(ert-deftest canvas-browser-brackets-that-the-page-wrote-are-no-picture ()
  ;; GIVEN a text with brackets that are words of the page itself
  ;; THEN the line is asked for as it is written only
  (should (equal (canvas-browser-test--ways "see [1] for details\n")
                 '(("see [1] for details" 0 0)))))

(ert-deftest canvas-browser-a-line-found-without-its-pictures-is-a-hit ()
  ;; GIVEN the ways to ask for a line with a picture, and a page that
  ;;       holds the words without the picture
  ;; WHEN the page answers
  ;; THEN the reader is told of a hit: it is the line that was asked for
  (canvas-browser-test--in-page
    (let ((said nil)
          (lines (with-temp-buffer
                   (insert (canvas-browser-test--picture "[image]") " Hello there\nNext\n")
                   (goto-char (point-min))
                   (canvas-browser--lines-to-find))))
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq said (apply #'format format-string args)))))
        (canvas-browser--located '(:found 3 :count 1 :rank 0) lines)
        (should (string-search "hit 1 of 1" said))
        (canvas-browser--located '(:found 4 :count 1 :rank 0) lines)
        (should (string-search "shown is: Next" said))))))

(ert-deftest canvas-browser-a-line-that-the-page-does-not-hold-is-said ()
  ;; GIVEN the text of a page, and a page that holds none of its lines
  ;;       as they are written, as the lines that a translator made
  ;; WHEN M-. is pressed on a line
  ;; THEN the reader is told that the page does not hold it
  (canvas-browser-test--with-text "alice at noon\n" '(:found -1)
    (should (string-search "does not hold" (canvas-browser-test--show-line 1)))))

(ert-deftest canvas-browser-a-line-found-further-down-is-said ()
  ;; GIVEN the text of a page whose first line the page does not hold,
  ;;       and whose second line it holds
  ;; WHEN M-. is pressed on the first
  ;; THEN the reader is told that the line after it is shown
  (canvas-browser-test--with-text "alice at noon\nHello there\n" '(:found 2 :count 1 :rank 0)
    (should (string-search "shown is: Hello there" (canvas-browser-test--show-line 1)))))

(ert-deftest canvas-browser-a-line-of-a-part-is-looked-for-in-the-part ()
  ;; GIVEN a page where a part is picked, and the text of that part
  ;; WHEN M-. is pressed on a line of it
  ;; THEN the page is asked for the line in that part
  ;; WHEN the part is no longer picked
  ;; THEN the page is asked for it in the whole page
  (canvas-browser-test--in-page
    (let ((page (current-buffer)))
      (cl-letf (((symbol-function 'display-buffer) #'ignore)
                ((symbol-function 'pop-to-buffer) (lambda (buffer &rest _) (set-buffer buffer))))
        (canvas-browser-test--with-part '(:x 0 :y 40 :w 600 :h 500) "the part"
          (canvas-browser-text)
          (let ((text-buffer (canvas-browser--text-buffer)))
            (unwind-protect
                (progn
                  (with-current-buffer text-buffer (canvas-browser-text-show-in-page))
                  (should (string-search "window.__canvasBrowserParts[1]), [{"
                                         (canvas-browser-test--asked-for)))
                  (with-current-buffer page (setq canvas-browser--part nil))
                  (with-current-buffer text-buffer (canvas-browser-text-show-in-page))
                  (should (string-search "(document.body, " (canvas-browser-test--asked-for))))
              (kill-buffer text-buffer))))))))

(ert-deftest canvas-browser-a-text-whose-page-is-gone-says-so ()
  ;; GIVEN the text of a page whose buffer was killed since
  ;; WHEN M-. is pressed on a line
  ;; THEN it is an error that says the page is gone
  (let ((text-buffer nil))
    (canvas-browser-test--in-page
      (cl-letf (((symbol-function 'canvas-browser-cdp-send)
                 (lambda (_method _params &optional answer _session &rest _)
                   (when answer (funcall answer '(:result (:value "one\ntwo"))))))
                ((symbol-function 'display-buffer) #'ignore))
        (canvas-browser-text)
        (setq text-buffer (canvas-browser--text-buffer))))
    (unwind-protect
        (with-current-buffer text-buffer
          (should (string-search "gone"
                                 (cadr (should-error (canvas-browser-text-show-in-page)
                                                     :type 'user-error)))))
      (kill-buffer text-buffer))))

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

(defmacro canvas-browser-test--finding (answer &rest body)
  "Run BODY in a page that answers each search with ANSWER.
ANSWER is evaluated at each search.  What the reader is told goes to
`said\='."
  (declare (indent 1))
  `(canvas-browser-test--in-page
     (let ((said nil))
       (cl-letf (((symbol-function 'canvas-browser-cdp-send)
                  (lambda (method params &optional reply _session &rest _)
                    (push (cons method params) canvas-browser-test--commands)
                    (when reply (funcall reply (list :result (list :value ,answer))))))
                 ((symbol-function 'message)
                  (lambda (format-string &rest args)
                    (setq said (and format-string (apply #'format format-string args))))))
         ,@body))))

(defun canvas-browser-test--search-asked ()
  "What the page was last asked to search for: the arguments, as text."
  (let ((expression (plist-get (canvas-browser-test--params "Runtime.evaluate") :expression)))
    (and (string-match "})(\\(.*\\))\\'" expression)
         (match-string 1 expression))))

(ert-deftest canvas-browser-find-asks-the-page-and-steps-through-the-hits ()
  ;; GIVEN a page that holds a string three times
  ;; WHEN the string is searched for, and then stepped forwards and back
  ;; THEN the page is asked for the string from where it stands, AND each
  ;;      step is asked from the hit before it, AND the reader is told
  ;;      which hit of how many is shown
  (let ((answer '(:count 3 :index 0 :anchor 120)))
    (canvas-browser-test--finding answer
      (canvas-browser-find "parser")
      (should (equal (canvas-browser-test--search-asked) "\"parser\", false, null, 0"))
      (should (string-search "1 of 3" said))
      (setq answer '(:count 3 :index 1 :anchor 300))
      (canvas-browser-find-next)
      (should (equal (canvas-browser-test--search-asked) "\"parser\", false, 120, 1"))
      (should (string-search "2 of 3" said))
      (canvas-browser-find-previous)
      (should (equal (canvas-browser-test--search-asked) "\"parser\", false, 300, -1")))))

(ert-deftest canvas-browser-find-says-when-a-page-holds-nothing ()
  ;; GIVEN a page that answers with no hits
  ;; WHEN a string is searched for
  ;; THEN the reader is told that the page holds it nowhere
  (canvas-browser-test--finding '(:count 0)
    (canvas-browser-find "nothing here")
    (should (string-search "no hit" said))))

(ert-deftest canvas-browser-find-can-search-for-a-regexp ()
  ;; GIVEN a page that was searched for a string
  ;; WHEN the search is switched to a regexp
  ;; THEN the page is asked again, for the same text as a regexp, AND the
  ;;      next search of this page is for a regexp too
  (canvas-browser-test--finding '(:count 2 :index 0 :anchor 7)
    (canvas-browser-find "a|b")
    (canvas-browser-find-toggle-regexp)
    (should (equal (canvas-browser-test--search-asked) "\"a|b\", true, 7, 0"))
    (should canvas-browser--find-regexp)
    (canvas-browser-find-toggle-regexp)
    (should (equal (canvas-browser-test--search-asked) "\"a|b\", false, 7, 0"))))

(ert-deftest canvas-browser-find-says-what-is-wrong-with-a-regexp ()
  ;; GIVEN a search for a regexp that is not finished, as it is while it
  ;;       is typed
  ;; WHEN the page answers that it cannot read it
  ;; THEN the reader is told what the page says
  (canvas-browser-test--finding '(:error "Unterminated group")
    (setq canvas-browser--find-regexp t)
    (canvas-browser-find "fil(")
    (should (string-search "Unterminated group" said))))

(ert-deftest canvas-browser-find-follows-what-is-typed ()
  ;; GIVEN a search of a page that stands at a hit
  ;; WHEN more is typed
  ;; THEN the page is asked for the longer text from that hit on, as
  ;;      isearch goes on from where it stands
  ;; WHEN all of it is deleted
  ;; THEN the page is asked for nothing, which takes the paint away, AND
  ;;      the reader is not told of a miss
  (let ((answer '(:count 5 :index 2 :anchor 40)))
    (canvas-browser-test--finding answer
      (canvas-browser--find-typed "pa")
      (canvas-browser--find-typed "par")
      (should (equal (canvas-browser-test--search-asked) "\"par\", false, 40, 0"))
      (setq answer '(:count 0)
            said nil)
      (canvas-browser--find-typed "")
      (should (equal (canvas-browser-test--search-asked) "\"\", false, 40, 0"))
      (should-not said))))

(ert-deftest canvas-browser-an-answer-to-an-older-search-is-dropped ()
  ;; GIVEN two searches sent one after the other, as fast typing sends
  ;;       them
  ;; WHEN the answer to the first comes after the answer to the second
  ;; THEN the page is known to stand where the second answer said
  (canvas-browser-test--in-page
    (let ((answers nil))
      (cl-letf (((symbol-function 'canvas-browser-cdp-send)
                 (lambda (_method _params &optional answer _session &rest _)
                   (push answer answers)))
                ((symbol-function 'message) #'ignore))
        (canvas-browser--find-typed "p")
        (canvas-browser--find-typed "pa")
        (funcall (nth 0 answers) '(:result (:value (:count 2 :index 1 :anchor 90))))
        (funcall (nth 1 answers) '(:result (:value (:count 9 :index 0 :anchor 5))))
        (should (equal canvas-browser--find-anchor 90))
        (should (equal canvas-browser--find-index 1))))))

(ert-deftest canvas-browser-a-search-that-is-given-up-puts-the-page-back ()
  ;; GIVEN a search that scrolled the page
  ;; WHEN it ends with C-g
  ;; THEN the page is told to scroll back and to take the paint away,
  ;;      AND the string is not kept as the last search
  ;; WHEN a search ends with RET
  ;; THEN the page is told to stay, AND the string is the last search
  (canvas-browser-test--finding '(:count 1 :index 0 :anchor 3)
    (canvas-browser--find-typed "par")
    (canvas-browser--find-end "par" t)
    (should (string-search "(true)" (plist-get (canvas-browser-test--params "Runtime.evaluate")
                                               :expression)))
    (should-not canvas-browser--last-search)
    (canvas-browser--find-typed "par")
    (canvas-browser--find-end "par" nil)
    (should (string-search "(false)" (plist-get (canvas-browser-test--params "Runtime.evaluate")
                                                :expression)))
    (should (equal canvas-browser--last-search "par"))))

(ert-deftest canvas-browser-a-step-with-nothing-typed-takes-the-last-search ()
  ;; GIVEN a page that was searched for a string before
  ;; WHEN a new search steps on with nothing typed yet, as C-s C-s does
  ;;      in isearch
  ;; THEN the last string is what to type, AND the page is not asked
  ;; GIVEN something is typed
  ;; THEN the step is asked of the page
  (canvas-browser-test--finding '(:count 3 :index 1 :anchor 50)
    (setq canvas-browser--last-search "parser")
    (setq canvas-browser-test--commands nil)
    (should (equal (canvas-browser--find-step 1 "") "parser"))
    (should-not (canvas-browser-test--params "Runtime.evaluate"))
    (should-not (canvas-browser--find-step 1 "par"))
    (should (equal (canvas-browser-test--search-asked) "\"par\", false, null, 1"))))

(defun canvas-browser-test--searches-asked ()
  "Every search and every end of one that the page was asked for, oldest first.
Each is the text of its arguments."
  (delq nil
        (mapcar (lambda (command)
                  (let ((expression (or (plist-get (cdr command) :expression) "")))
                    (and (equal (car command) "Runtime.evaluate")
                         (string-match "})(\\(.*\\))\\'" expression)
                         (match-string 1 expression))))
                (reverse canvas-browser-test--commands))))

(defun canvas-browser-test--type (keys)
  "Press KEYS in this page buffer, as the reader does; a quit ends them."
  (switch-to-buffer (current-buffer))
  (setq canvas-browser-test--commands nil)
  (condition-case nil
      (execute-kbd-macro (kbd keys))
    (quit nil)))

(ert-deftest canvas-browser-a-search-is-typed-in-the-minibuffer ()
  ;; GIVEN a page buffer
  ;; WHEN C-s is pressed, two letters are typed, C-s is pressed again,
  ;;      and then RET
  ;; THEN the page is asked for the first letter, then for both, then
  ;;      for the next hit of both, AND it is told that the search ended
  ;;      where it stands, AND what was typed is the last search
  (canvas-browser-test--finding '(:count 3 :index 0 :anchor 10)
    (canvas-browser-test--type "C-s p a C-s RET")
    (should (equal (canvas-browser-test--searches-asked)
                   '("\"p\", false, null, 0"
                     "\"pa\", false, 10, 0"
                     "\"pa\", false, 10, 1"
                     "false")))
    (should (equal canvas-browser--last-search "pa"))))

(ert-deftest canvas-browser-a-search-is-given-up-with-the-quit-key ()
  ;; GIVEN a page buffer where a search is typed
  ;; WHEN C-g is pressed
  ;; THEN the page is told to go back to where it stood
  (canvas-browser-test--finding '(:count 3 :index 0 :anchor 10)
    (canvas-browser-test--type "C-s p C-g")
    (should (equal (canvas-browser-test--searches-asked)
                   '("\"p\", false, null, 0" "true")))))

(ert-deftest canvas-browser-a-search-takes-the-last-one-up-with-the-search-key ()
  ;; GIVEN a page that was searched for a string before
  ;; WHEN C-s is pressed twice, and then RET
  ;; THEN the last string is typed for the reader, AND the page is asked
  ;;      for it
  (canvas-browser-test--finding '(:count 3 :index 0 :anchor 10)
    (setq canvas-browser--last-search "parser")
    (canvas-browser-test--type "C-s C-s RET")
    (should (equal (canvas-browser-test--searches-asked)
                   '("\"parser\", false, null, 0" "false")))))

(ert-deftest canvas-browser-a-search-switches-to-a-regexp-while-it-is-typed ()
  ;; GIVEN a page buffer where a search is typed
  ;; WHEN M-r is pressed, and then RET
  ;; THEN the page is asked for what was typed as a regexp
  (canvas-browser-test--finding '(:count 3 :index 0 :anchor 10)
    (canvas-browser-test--type "C-s a M-r RET")
    (should (equal (canvas-browser-test--searches-asked)
                   '("\"a\", false, null, 0" "\"a\", true, 10, 0" "false")))))

(defmacro canvas-browser-test--landing (&rest body)
  "Run BODY in a page and keep how the eye is drawn to its hits.
Each flash goes to `drawn\=' as (flash BOX), and each flight as
(fly FROM TO), newest first.  The page answers a search with `answer\='."
  (declare (indent 0))
  `(let ((drawn nil)
         (answer nil))
     (canvas-browser-test--finding answer
       (let ((canvas-browser-pulse-function (lambda (box) (push (list 'flash box) drawn)))
             (canvas-browser-focus-function (lambda (from to) (push (list 'fly from to) drawn))))
         ,@body))))

(ert-deftest canvas-browser-the-eye-is-drawn-to-where-a-search-lands ()
  ;; GIVEN a page that is searched
  ;; WHEN the page shows the first hit, and then the next one
  ;; THEN the first hit is flashed, AND the eye is flown from it to the
  ;;      next one
  ;; WHEN a new search starts
  ;; THEN its first hit is flashed again: it is not flown to from a hit
  ;;      of the search before
  (canvas-browser-test--landing
    (setq answer '(:count 2 :index 0 :anchor 5 :box (10 20 30 12)))
    (canvas-browser-find "Reply")
    (should (equal drawn '((flash (:x 10 :y 20 :w 30 :h 12)))))
    (setq answer '(:count 2 :index 1 :anchor 90 :box (10 300 30 12)))
    (canvas-browser-find-next)
    (should (equal (car drawn) '(fly (:x 10 :y 20 :w 30 :h 12) (:x 10 :y 300 :w 30 :h 12))))
    (canvas-browser-find "other")
    (should (equal (car drawn) '(flash (:x 10 :y 300 :w 30 :h 12))))))

(ert-deftest canvas-browser-a-search-that-finds-nothing-draws-nothing ()
  ;; GIVEN a page that is searched for what it does not hold
  ;; THEN nothing is flashed and nothing flies
  (canvas-browser-test--landing
    (setq answer '(:count 0))
    (canvas-browser-find "nothing here")
    (should-not drawn)))

(ert-deftest canvas-browser-the-eye-is-drawn-to-a-line-shown-from-the-text ()
  ;; GIVEN the text of a page
  ;; WHEN M-. shows a line of it in the page
  ;; THEN the line is flashed in the page
  (let* ((drawn nil)
         (canvas-browser-pulse-function (lambda (box) (push box drawn))))
    (canvas-browser-test--with-text "Reply\nHello there\n"
        '(:found 0 :count 1 :rank 0 :box (10 20 30 12))
      (canvas-browser-test--show-line 1)
      (should (equal drawn '((:x 10 :y 20 :w 30 :h 12)))))))

(ert-deftest canvas-browser-the-keys-of-a-search-are-those-of-isearch ()
  ;; GIVEN the keys of a search of a page
  ;; THEN C-s and C-r step to the next hit and the one before, AND M-r
  ;;      switches between a string and a regexp, AND the other keys are
  ;;      those of the minibuffer
  (should (eq (lookup-key canvas-browser-find-map (kbd "C-s")) #'canvas-browser-find-next))
  (should (eq (lookup-key canvas-browser-find-map (kbd "C-r")) #'canvas-browser-find-previous))
  (should (eq (lookup-key canvas-browser-find-map (kbd "M-r")) #'canvas-browser-find-toggle-regexp))
  (should (eq (keymap-parent canvas-browser-find-map) minibuffer-local-map)))

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
               (lambda (method params &optional answer _session &rest _)
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
               (lambda (method params &optional answer _session &rest _)
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

;;;; A command that chromium refuses for a moment

(defmacro canvas-browser-test--refusing (method times text &rest body)
  "Run BODY in a page whose chromium refuses METHOD the first TIMES with TEXT.
Every other command is taken, and METHOD after those times.  A timer
runs at once, so a command that is sent again is sent within BODY.  What
is said goes to `said'."
  (declare (indent 3))
  `(let ((left ,times) (said nil))
     (cl-letf (((symbol-function 'canvas-browser-cdp-send)
                (lambda (method params &optional answer _session refused)
                  (push (cons method params) canvas-browser-test--commands)
                  (if (and (equal method ,method) (> left 0))
                      (progn (setq left (1- left))
                             (if refused
                                 (funcall refused ,text)
                               (push (format "refused %s" method) said)))
                    (when answer (funcall answer nil)))))
               ((symbol-function 'run-with-timer)
                (lambda (_seconds _repeat function &rest args) (apply function args)))
               ((symbol-function 'message)
                (lambda (format &rest args) (push (apply #'format format args) said))))
       ,@body)))

(defun canvas-browser-test--count (method)
  "How many times METHOD was sent."
  (cl-count method canvas-browser-test--commands :key #'car :test #'equal))

(ert-deftest canvas-browser-a-screencast-that-is-refused-for-a-moment-is-asked-for-again ()
  ;; GIVEN a page that chromium does not count as active for a moment, as
  ;;       while it moves to another site, so that it refuses the
  ;;       screencast twice
  ;; WHEN the screencast is started
  ;; THEN it is asked for again until chromium takes it, where the page
  ;;      was left without frames, AND nothing is said to the reader
  (canvas-browser-test--in-page
    (setq canvas-browser-test--commands nil)
    (canvas-browser-test--refusing "Page.startScreencast" 2 "Not attached to an active page"
      (canvas-browser--start-screencast)
      (should (= (canvas-browser-test--count "Page.startScreencast") 3))
      (should canvas-browser--screencast)
      (should-not said))))

(ert-deftest canvas-browser-a-page-is-told-again-that-it-is-active ()
  ;; GIVEN a page that chromium does not count as active for a moment
  ;; WHEN the page is told that it is the one in front
  ;; THEN it is told again until chromium takes it
  (canvas-browser-test--in-page
    (setq canvas-browser-test--commands nil)
    (canvas-browser-test--refusing "Page.setWebLifecycleState" 1 "Not attached to an active page"
      (canvas-browser--awaken t)
      (should (= (canvas-browser-test--count "Page.setWebLifecycleState") 2))
      (should-not said))))

(ert-deftest canvas-browser-a-command-refused-again-and-again-is-given-up ()
  ;; GIVEN a page that chromium never counts as active
  ;; WHEN the screencast is started
  ;; THEN it is asked for a bounded number of times, AND the reader is
  ;;      told once that chromium refused it, with what chromium says
  (canvas-browser-test--in-page
    (setq canvas-browser-test--commands nil)
    (canvas-browser-test--refusing "Page.startScreencast" 1000 "Not attached to an active page"
      (canvas-browser--start-screencast)
      (should (= (canvas-browser-test--count "Page.startScreencast")
                 (1+ canvas-browser--retries)))
      (should (= (length said) 1))
      (should (string-search "Page.startScreencast" (car said)))
      (should (string-search "Not attached to an active page" (car said))))))

(ert-deftest canvas-browser-another-refusal-is-said-and-not-asked-again ()
  ;; GIVEN a page whose chromium refuses the screencast for another reason
  ;; WHEN the screencast is started
  ;; THEN it is asked for once, AND the reader is told what chromium says
  (canvas-browser-test--in-page
    (setq canvas-browser-test--commands nil)
    (canvas-browser-test--refusing "Page.startScreencast" 1 "Invalid parameters"
      (canvas-browser--start-screencast)
      (should (= (canvas-browser-test--count "Page.startScreencast") 1))
      (should (= (length said) 1))
      (should (string-search "Invalid parameters" (car said))))))

(ert-deftest canvas-browser-a-page-put-to-sleep-is-not-asked-for-frames-again ()
  ;; GIVEN a page whose screencast chromium refused, AND that no window
  ;;       shows any more by the time it is asked for again
  ;; WHEN the moment to ask again comes
  ;; THEN the screencast is not asked for: a page that nobody sees is
  ;;      left to chromium
  (canvas-browser-test--in-page
    (setq canvas-browser-test--commands nil)
    (let ((later nil))
      (canvas-browser-test--refusing "Page.startScreencast" 1 "Not attached to an active page"
        (cl-letf (((symbol-function 'run-with-timer)
                   (lambda (_seconds _repeat function &rest args)
                     (setq later (lambda () (apply function args))))))
          (canvas-browser--start-screencast))
        (canvas-browser--sleep)
        (funcall later)
        (should (= (canvas-browser-test--count "Page.startScreencast") 1))))))

;;;; The part of the page that is picked

(defconst canvas-browser-test--two-parts
  '((:x 0 :y 0 :w 200 :h 400 :scrolls :false)
    (:x 300 :y 0 :w 200 :h 400 :scrolls t))
  "The boxes of two parts of a page: one that the page marks, which does
not scroll on its own, and one that does.")

(defmacro canvas-browser-test--picking (boxes chosen &rest body)
  "Run BODY in a page whose parts have BOXES, where the hint names CHOSEN.
CHOSEN is the place of a label, where the whole page has the first, or
nil for ESC.  What is drawn goes to `canvas-browser-test--drawn'."
  (declare (indent 2))
  `(let ((canvas-browser-test--drawn nil))
     (cl-letf (((symbol-function 'canvas-browser--parts)
                (lambda (answer) (funcall answer ,boxes)))
               ((symbol-function 'canvas-browser--draw-hints)
                (lambda (boxes _hints) (push (cons 'hints boxes) canvas-browser-test--drawn)))
               ((symbol-function 'canvas-browser--draw-outline)
                (lambda (box) (push (cons 'outline box) canvas-browser-test--drawn)))
               ((symbol-function 'canvas-browser--read-hint)
                (lambda (&rest _) (and ,chosen (list ,chosen)))))
       ,@body)))

(defvar canvas-browser-test--drawn nil
  "What was drawn over the page, newest first.
Each is (outline . BOX), (tint . BOX), (hints . BOXES), or the symbol
`repaint' for a new picture of the page, which takes the marks off.")

(defun canvas-browser-test--scripts ()
  "The scripts that were sent to the page, newest first."
  (cl-loop for (method . params) in canvas-browser-test--commands
           when (equal method "Runtime.evaluate")
           collect (plist-get params :expression)))

(ert-deftest canvas-browser-asks-the-page-for-its-parts ()
  ;; GIVEN a page buffer
  ;; WHEN the parts of the page are asked for
  ;; THEN the script looks for what overflows its own box and for what
  ;;      the page marks as a part, keeps those elements in the page so
  ;;      that a later key finds them again, AND their boxes come back
  (canvas-browser-test--in-page
    (let ((boxes 'none))
      (cl-letf (((symbol-function 'canvas-browser-cdp-send)
                 (lambda (method params &optional answer _session &rest _)
                   (push (cons method params) canvas-browser-test--commands)
                   (when answer
                     (funcall answer '(:result (:value ((:x 1 :y 2 :w 3 :h 4)))))))))
        (canvas-browser--parts (lambda (found) (setq boxes found))))
      (let ((sent (canvas-browser-test--params "Runtime.evaluate")))
        (should (string-search "scrollHeight" (plist-get sent :expression)))
        (should (string-search "main,aside" (plist-get sent :expression)))
        (should (string-search "__canvasBrowserParts" (plist-get sent :expression)))
        (should (eq (plist-get sent :returnByValue) t)))
      (should (equal boxes '((:x 1 :y 2 :w 3 :h 4)))))))

(ert-deftest canvas-browser-picking-a-part-that-scrolls-sends-the-scroll-keys-to-it ()
  ;; GIVEN a page with two parts, of which the second scrolls on its own
  ;; WHEN the second one is picked, and then a line is scrolled
  ;; THEN the line goes to that part, by the place it was given, and not
  ;;      to the window
  (canvas-browser-test--in-page
    ;; The whole page takes the first letter, so the second part is the
    ;; third label.
    (canvas-browser-test--picking canvas-browser-test--two-parts 2
      (canvas-browser-pick-part))
    (should (equal canvas-browser--part '(:place 1 :scrolls t)))
    (setq canvas-browser-test--commands nil)
    (canvas-browser-scroll-line-up)
    ;; The chromium of the tests answers no scroll with true, so the
    ;; part is forgotten after, and the scroll is the script before that.
    (let ((expression (car (last (canvas-browser-test--scripts)))))
      (should (string-search (format "__canvasBrowserParts[1]), 'by', %d)"
                                     canvas-browser-line-height)
                             expression))
      (should-not (string-search "window.scrollBy" expression)))))

(ert-deftest canvas-browser-a-part-that-does-not-scroll-leaves-the-scroll-keys-to-the-page ()
  ;; GIVEN a page with two parts, of which the first does not scroll on
  ;;       its own
  ;; WHEN the first one is picked, and then a line is scrolled
  ;; THEN the part is the one picked, AND the line moves the whole page
  (canvas-browser-test--in-page
    (canvas-browser-test--picking canvas-browser-test--two-parts 1
      (canvas-browser-pick-part))
    (should (equal canvas-browser--part '(:place 0 :scrolls nil)))
    (canvas-browser-scroll-line-up)
    (should (string-search "window.scrollBy"
                           (plist-get (canvas-browser-test--params "Runtime.evaluate")
                                      :expression)))))

(ert-deftest canvas-browser-the-parts-are-outlined-while-one-is-picked ()
  ;; GIVEN a page with two parts
  ;; WHEN the parts are labelled
  ;; THEN each part gets an outline before the labels are drawn, so that
  ;;      its size shows, AND the whole page gets a label and no outline
  (canvas-browser-test--in-page
    (canvas-browser-test--picking canvas-browser-test--two-parts 1
      (canvas-browser-pick-part)
      (let ((drawn (reverse canvas-browser-test--drawn)))
        (should (equal (seq-take drawn 2)
                       `((outline . ,(nth 0 canvas-browser-test--two-parts))
                         (outline . ,(nth 1 canvas-browser-test--two-parts)))))
        (should (eq (car (nth 2 drawn)) 'hints))
        (should (= (length (cdr (nth 2 drawn))) 3))))))

(ert-deftest canvas-browser-the-part-that-is-picked-keeps-a-mark-in-the-page ()
  ;; GIVEN a page with two parts
  ;; WHEN the first one is picked
  ;; THEN the page is told to draw an outline on that part, in the
  ;;      colour of a hint, which stays while the part is picked, AND
  ;;      that counts as a command, so that a still picture asked for
  ;;      before it, without the outline, is dropped
  (canvas-browser-test--in-page
    (let ((commands canvas-browser--commands))
      (canvas-browser-test--picking canvas-browser-test--two-parts 1
        (canvas-browser-pick-part))
      (let ((script (car (canvas-browser-test--scripts))))
        (should (string-search "__canvasBrowserMark" script))
        (should (string-search "solid #FFD633" script))
        (should (string-suffix-p
                 "((window.__canvasBrowserParts && window.__canvasBrowserParts[0]))"
                 script)))
      (should (> canvas-browser--commands commands)))))

(ert-deftest canvas-browser-picking-the-part-that-is-picked-leaves-the-page-alone ()
  ;; GIVEN a page where the first part is picked
  ;; WHEN that same part is picked again
  ;; THEN the page is not told to move its mark, AND it counts as no
  ;;      command: the picture asked for after the labels is then
  ;;      painted, which takes the labels off.  A mark put where one was
  ;;      changes nothing that chromium would send a frame for
  (canvas-browser-test--in-page
    (canvas-browser-test--picking canvas-browser-test--two-parts 1
      (canvas-browser-pick-part)
      (setq canvas-browser-test--commands nil)
      (let ((commands canvas-browser--commands))
        (canvas-browser-pick-part)
        (should (= canvas-browser--commands commands))))
    (should (equal canvas-browser--part '(:place 0 :scrolls nil)))
    (should-not (cl-some (lambda (script) (string-search "__canvasBrowserMark" script))
                         (canvas-browser-test--scripts)))))

(ert-deftest canvas-browser-the-mark-goes-when-the-whole-page-is-picked ()
  ;; GIVEN a page buffer where a part was picked
  ;; WHEN the whole page is picked again
  ;; THEN the page is told to take the outline off, with no part to put
  ;;      one on
  (canvas-browser-test--in-page
    (setq canvas-browser--part '(:place 1 :scrolls t))
    (canvas-browser-test--picking canvas-browser-test--two-parts 0
      (canvas-browser-pick-part))
    (let ((script (car (canvas-browser-test--scripts))))
      (should (string-search "__canvasBrowserMark" script))
      (should (string-suffix-p "(null)" script)))))

(ert-deftest canvas-browser-the-mark-goes-with-a-part-that-is-forgotten ()
  ;; GIVEN a page buffer where a part was picked
  ;; WHEN the page goes to another address
  ;; THEN the page is told to take the outline off, since a page that
  ;;      draws itself anew under another address may keep the element
  (canvas-browser-test--in-page
    (setq canvas-browser--part '(:place 1 :scrolls t))
    (setq canvas-browser-test--commands nil)
    (canvas-browser--target-changed
     (list :targetInfo (list :targetId canvas-browser--target :type "page"
                             :url "https://example.org/next" :title "The next page")))
    (should (string-suffix-p "(null)" (car (canvas-browser-test--scripts))))))

(ert-deftest canvas-browser-a-new-address-without-a-part-takes-no-mark-off ()
  ;; GIVEN a page buffer where no part is picked
  ;; WHEN the page goes to another address
  ;; THEN the page is not asked to take an outline off
  (canvas-browser-test--in-page
    (setq canvas-browser-test--commands nil)
    (canvas-browser--target-changed
     (list :targetInfo (list :targetId canvas-browser--target :type "page"
                             :url "https://example.org/next" :title "The next page")))
    (should-not (cl-some (lambda (script) (string-search "__canvasBrowserMark" script))
                         (canvas-browser-test--scripts)))))

(ert-deftest canvas-browser-forgets-a-part-that-is-gone ()
  ;; GIVEN a page whose picked part has gone, after a click or a new page
  ;; WHEN a line is scrolled
  ;; THEN the page answers that it is gone, the reader is told, AND the
  ;;      next scroll moves the whole page again
  (canvas-browser-test--in-page
    (setq canvas-browser--part '(:place 1 :scrolls t))
    (let ((said nil))
      (cl-letf (((symbol-function 'canvas-browser-cdp-send)
                 (lambda (method params &optional answer _session &rest _)
                   (push (cons method params) canvas-browser-test--commands)
                   (when answer (funcall answer '(:result (:value :false))))))
                ((symbol-function 'message)
                 (lambda (format &rest args) (setq said (apply #'format format args)))))
        (canvas-browser-scroll-line-up)
        (should-not canvas-browser--part)
        (should (string-search "the whole page" said))))
    (canvas-browser-scroll-line-up)
    (should (string-search "window.scrollBy"
                           (plist-get (canvas-browser-test--params "Runtime.evaluate")
                                      :expression)))))

(ert-deftest canvas-browser-a-new-address-forgets-the-part ()
  ;; GIVEN a page buffer where a part was picked
  ;; WHEN the page goes to another address
  ;; THEN no part is picked, since the part belonged to the page before
  (canvas-browser-test--in-page
    (setq canvas-browser--part '(:place 1 :scrolls t))
    (canvas-browser--target-changed
     (list :targetInfo (list :targetId canvas-browser--target :type "page"
                             :url "https://example.org/next" :title "The next page")))
    (should-not canvas-browser--part)))

(ert-deftest canvas-browser-a-choice-that-is-given-up-keeps-the-part-that-was-picked ()
  ;; GIVEN a page buffer where a part was picked
  ;; WHEN the parts are labelled again, and no label is named: ESC, or a
  ;;      key that is no label
  ;; THEN the part that was picked is still picked, AND the page is not
  ;;      told to move its mark
  (canvas-browser-test--in-page
    (setq canvas-browser--part '(:place 1 :scrolls t))
    (setq canvas-browser-test--commands nil)
    (canvas-browser-test--picking canvas-browser-test--two-parts nil
      (canvas-browser-pick-part))
    (should (equal canvas-browser--part '(:place 1 :scrolls t)))
    (should-not (cl-some (lambda (script) (string-search "__canvasBrowserMark" script))
                         (canvas-browser-test--scripts)))))

(ert-deftest canvas-browser-c-g-gives-the-keys-back-to-the-whole-page ()
  ;; GIVEN a page buffer in normal state where a part is picked
  ;; WHEN C-g is pressed
  ;; THEN no part is picked, AND the page is told to take the mark off,
  ;;      AND a message says that the keys act on the whole page
  (canvas-browser-test--in-page
    (setq canvas-browser--part '(:place 1 :scrolls t))
    (setq canvas-browser-test--commands nil)
    (let ((said nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest args) (setq said (apply #'format format args)))))
        (should (eq (key-binding (kbd "C-g")) #'canvas-browser-quit))
        (canvas-browser-test--press "C-g"))
      (should (string-search "the whole page" said)))
    (should-not canvas-browser--part)
    (should (string-suffix-p "(null)" (car (canvas-browser-test--scripts))))))

(ert-deftest canvas-browser-c-g-quits-as-ever-when-no-part-is-picked ()
  ;; GIVEN a page buffer in normal state where no part is picked
  ;; WHEN C-g is pressed
  ;; THEN it quits, as it does in any buffer, AND the page is told nothing
  (canvas-browser-test--in-page
    (setq canvas-browser-test--commands nil)
    ;; A quit is no error, so `should-error' does not see it.
    (should (eq 'quit (condition-case nil
                          (canvas-browser-test--press "C-g")
                        (quit 'quit))))
    (should-not (canvas-browser-test--scripts))))

(ert-deftest canvas-browser-says-when-a-page-has-no-parts ()
  ;; GIVEN a page of one piece
  ;; WHEN the parts are asked for
  ;; THEN the reader is told, and nothing is picked or drawn
  (canvas-browser-test--in-page
    (let ((said nil))
      (canvas-browser-test--picking nil nil
        (cl-letf (((symbol-function 'message)
                   (lambda (format &rest args) (setq said (apply #'format format args)))))
          (canvas-browser-pick-part))
        (should-not canvas-browser-test--drawn))
      (should (string-search "no parts" said))
      (should-not canvas-browser--part))))

(ert-deftest canvas-browser-the-key-for-picking-a-part-is-bound ()
  ;; GIVEN a page buffer in normal state
  ;; WHEN `S\=' is looked up
  ;; THEN it labels the parts of the page, as ace-window labels windows,
  ;;      AND the name the command had before still runs it
  (canvas-browser-test--in-page
    (should (eq (key-binding (kbd "S")) #'canvas-browser-pick-part))
    (should (eq (indirect-function 'canvas-browser-pick-scroller)
                (indirect-function 'canvas-browser-pick-part)))))

(ert-deftest canvas-browser-the-whole-page-carries-a-letter-too ()
  ;; GIVEN a page with two parts
  ;; WHEN the parts are labelled
  ;; THEN the whole page is labelled first, in the top corner, where a
  ;;      part is unlikely to begin, AND naming it gives the keys back to
  ;;      the whole page
  (canvas-browser-test--in-page
    (setq canvas-browser--part '(:place 1 :scrolls t))
    (canvas-browser-test--picking canvas-browser-test--two-parts 0
      (canvas-browser-pick-part)
      (let ((labelled (cdr (assq 'hints canvas-browser-test--drawn))))
        (should (equal (length labelled) 3))
        (should (> (plist-get (car labelled) :x) 600))
        (should (equal (plist-get (car labelled) :y) 0))))
    (should-not canvas-browser--part)))

(ert-deftest canvas-browser-the-header-says-that-a-part-is-picked ()
  ;; GIVEN a page buffer
  ;; WHEN a part is picked
  ;; THEN the header line says so, AND it did not before
  (canvas-browser-test--in-page
    (should-not (string-search "part" (canvas-browser--header)))
    (canvas-browser-test--picking canvas-browser-test--two-parts 1
      (canvas-browser-pick-part))
    (should (string-search " · part" (canvas-browser--header)))))

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

(ert-deftest canvas-browser-the-picture-under-the-hints-is-painted-again-after-them ()
  ;; GIVEN a page that painted the still picture of itself, AND hints
  ;;       that were drawn over it and given up
  ;; WHEN chromium sends that very picture again, as a page that stands
  ;;      still does
  ;; THEN it is painted: the canvas held the hints, not that picture.
  ;;      It was dropped as a picture already there, and the hints stayed
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'canvas-cairo-image) #'ignore)
              ((symbol-function 'canvas-refresh) #'ignore)
              ((symbol-function 'canvas-browser--draw-hints) #'ignore)
              ((symbol-function 'canvas-browser--read-hint) (lambda (&rest _) nil)))
      (let ((still (base64-encode-string (canvas-browser-test--png 800 600))))
        (canvas-browser--paint still)
        (should (= canvas-browser--frames 1))
        (canvas-browser--choose-box '((:x 0 :y 0 :w 10 :h 10)))
        (canvas-browser--paint still)
        (should (= canvas-browser--frames 2))))))

(ert-deftest canvas-browser-the-picture-under-the-tint-is-painted-again-after-it ()
  ;; GIVEN a page that painted the still picture of itself, AND a tint
  ;;       that was drawn over a part of it while a receiver was asked for
  ;; WHEN chromium sends that very picture again
  ;; THEN it is painted, and takes the tint off
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'canvas-cairo-image) #'ignore)
              ((symbol-function 'canvas-refresh) #'ignore)
              ((symbol-function 'canvas-browser--draw-tint) #'ignore)
              ((symbol-function 'completing-read) (lambda (&rest _) "buffer")))
      (let ((still (base64-encode-string (canvas-browser-test--png 800 600))))
        (canvas-browser--paint still)
        (canvas-browser--read-receiver-over '(:x 0 :y 0 :w 10 :h 10))
        (canvas-browser--paint still)
        (should (= canvas-browser--frames 2))))))

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
                 (lambda (method params &optional answer _session &rest _)
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
                    ("M-DEL" "Backspace" 2)
                    ;; The keys of a browser itself, which Emacs binds to
                    ;; the same motions and deletions.
                    ("C-<right>" "ArrowRight" 2) ("C-<left>" "ArrowLeft" 2)
                    ("M-<right>" "ArrowRight" 2) ("M-<left>" "ArrowLeft" 2)
                    ("C-<up>" "ArrowUp" 2) ("C-<down>" "ArrowDown" 2)
                    ("C-<backspace>" "Backspace" 2) ("C-<delete>" "Delete" 2)
                    ;; Shift with a motion marks, as it does in a browser.
                    ("S-<right>" "ArrowRight" 8) ("S-<left>" "ArrowLeft" 8)
                    ("S-<up>" "ArrowUp" 8) ("S-<down>" "ArrowDown" 8)
                    ("S-<home>" "Home" 8) ("S-<end>" "End" 8)
                    ("C-S-<right>" "ArrowRight" 10) ("C-S-<left>" "ArrowLeft" 10)
                    ("C-S-<home>" "Home" 10) ("C-S-<end>" "End" 10)
                    ;; A line break that does not send, and the key that sends.
                    ("S-<return>" "Enter" 8) ("C-<return>" "Enter" 2)
                    ("C-j" "Enter" 0)
                    ("C-v" "PageDown" 0) ("M-v" "PageUp" 0)
                    ("M-{" "ArrowUp" 2) ("M-}" "ArrowDown" 2)))
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
                 (lambda (method params &optional answer _session &rest _)
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
                (lambda (method params &optional answer _session &rest _)
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

(ert-deftest canvas-browser-a-key-that-is-no-character-ends-the-hints-and-runs-after ()
  ;; GIVEN a hint being read
  ;; WHEN a key that is no character comes: a key with Control or Meta
  ;;      that no character stands for, as C-: and M-w, or a mouse click
  ;; THEN no hint is named, AND the key is kept as the next key of
  ;;      Emacs, so that it does what it does without the hints, where it
  ;;      was an error before
  (dolist (key (list (aref (kbd "C-:") 0) (aref (kbd "M-w") 0) '(mouse-1 (nil 1 (2 . 3) 0))))
    (let ((unread-command-events nil))
      (cl-letf (((symbol-function 'read-key) (lambda (&rest _) key))
                ((symbol-function 'clear-this-command-keys) #'ignore))
        (should-not (canvas-browser--read-hint '("a" "s")))
        (should (equal unread-command-events (list key)))))))

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
                 (lambda (method params &optional answer _session &rest _)
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

(defun canvas-browser-test--tab-info (target url)
  "What chromium says of the tab TARGET at URL, which no page opened."
  (list :targetInfo (list :targetId target :type "page" :url url :title "A tab")))

(defun canvas-browser-test--page-buffers ()
  "How many page buffers there are."
  (cl-count-if (lambda (buffer) (eq (buffer-local-value 'major-mode buffer) 'canvas-browser-mode))
               (buffer-list)))

(defmacro canvas-browser-test--with-tabs (&rest body)
  "Run BODY in a page, and kill the page buffers that BODY made after it."
  (declare (indent 0))
  `(let ((canvas-browser--rest-target "REST")
         (canvas-browser--closed-targets nil))
     (canvas-browser-test--in-page
       (let ((page (current-buffer)))
         (unwind-protect
             (progn ,@body)
           (dolist (buffer (buffer-list))
             (when (and (eq (buffer-local-value 'major-mode buffer) 'canvas-browser-mode)
                        (not (eq buffer page)))
               (kill-buffer buffer))))))))

(ert-deftest canvas-browser-a-tab-that-chromium-opens-on-its-own-gets-a-buffer ()
  ;; GIVEN a page buffer
  ;; WHEN chromium opens a tab at a web address that no page opened, as
  ;;      it does when the service worker of a site opens a window after
  ;;      its notification was clicked
  ;; THEN the tab is shown in a page buffer of its own, attached to it
  ;;      and not sent anywhere.  Left without a buffer it stood in front
  ;;      in chromium where nobody saw it, and its site took it as looked
  ;;      at and showed no notification any more
  ;; WHEN chromium tells of the tab again
  ;; THEN no second buffer is made
  (canvas-browser-test--with-tabs
    (setq canvas-browser-test--commands nil)
    (canvas-browser--target-created
     (canvas-browser-test--tab-info "S1" "https://mail.example/mail/1"))
    (let ((opened (canvas-browser--buffer-of-target "S1")))
      (should (buffer-live-p opened))
      (should (equal (buffer-local-value 'canvas-browser--url opened)
                     "https://mail.example/mail/1"))
      (should (equal (plist-get (canvas-browser-test--params "Target.attachToTarget") :targetId)
                     "S1"))
      (should-not (canvas-browser-test--params "Page.navigate")))
    (canvas-browser--target-created
     (canvas-browser-test--tab-info "S1" "https://mail.example/mail/1"))
    (should (= (canvas-browser-test--page-buffers) 2))))

(ert-deftest canvas-browser-a-tab-that-gets-its-address-later-gets-its-buffer-then ()
  ;; GIVEN a tab that chromium opened on its own with no address yet
  ;; THEN it has no buffer: a page buffer makes its own tab that way too
  ;; WHEN chromium says that the tab is at a web address now
  ;; THEN it is shown in a page buffer, once
  (canvas-browser-test--with-tabs
    (canvas-browser--target-created (canvas-browser-test--tab-info "S2" "about:blank"))
    (should-not (canvas-browser--buffer-of-target "S2"))
    (canvas-browser--target-changed
     (canvas-browser-test--tab-info "S2" "https://mail.example/mail/2"))
    (should (buffer-live-p (canvas-browser--buffer-of-target "S2")))
    (canvas-browser--target-changed
     (canvas-browser-test--tab-info "S2" "https://mail.example/mail/3"))
    (should (= (canvas-browser-test--page-buffers) 2))))

(ert-deftest canvas-browser-tabs-that-are-no-pages-of-the-web-get-no-buffer ()
  ;; GIVEN a page buffer
  ;; WHEN chromium tells of its own new-tab page, of the empty tab that
  ;;      stands in front of the pages that are put away, of a tab that
  ;;      a page buffer closed a moment ago, and of a page that it loads
  ;;      ahead of a click
  ;; THEN none of them gets a buffer
  (canvas-browser-test--with-tabs
    (push "GONE" canvas-browser--closed-targets)
    (dolist (info (list (canvas-browser-test--tab-info "N1" "chrome://newtab/")
                        (canvas-browser-test--tab-info "REST" "https://example.org/")
                        (canvas-browser-test--tab-info "GONE" "https://example.org/")
                        ;; A page that chromium loads ahead of a click.
                        (list :targetInfo (list :targetId "AHEAD" :type "page" :subtype "prerender"
                                                :url "https://example.org/next"))))
      (canvas-browser--target-created info)
      (canvas-browser--target-changed info))
    (should (= (canvas-browser-test--page-buffers) 1))))

(ert-deftest canvas-browser-a-page-buffer-that-is-killed-notes-its-tab-as-closed ()
  ;; GIVEN a page buffer
  ;; WHEN it lets go of its tab, as it does when it is killed
  ;; THEN the tab is among those that were closed of late, so that what
  ;;      chromium still says of it makes no new buffer
  (let ((canvas-browser--closed-targets nil))
    (canvas-browser-test--in-page
      (let ((target canvas-browser--target))
        (canvas-browser--release)
        (should (member target canvas-browser--closed-targets))))))

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
              (lambda (method params &optional answer _session &rest _)
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
    (should (member '("Runtime.addBinding" :name "canvasBrowserFullscreen")
                    canvas-browser-test--commands))
    (should (cl-some (lambda (script) (string-match-p "fullscreenchange" script))
                     (canvas-browser-test--scripts-before-load))))
  (canvas-browser-test--in-page
    (should-not (member '("Runtime.addBinding" :name "canvasBrowserFullscreen")
                        canvas-browser-test--commands))))

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

(ert-deftest canvas-browser-a-page-that-names-itself-later-gets-that-title ()
  ;; GIVEN a page buffer with the title that its page had when it loaded
  ;; WHEN the page has gone quiet after it moved, which is when the
  ;;      still picture of it is asked for
  ;; THEN the page is asked for its title again, AND the buffer takes
  ;;      it: a page may name itself after it has loaded, as Reddit names
  ;;      a post, and chromium tells nobody of that
  (canvas-browser-test--in-page
    (setq canvas-browser--title "Reddit - The heart of the internet")
    (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'a-window))
              ((symbol-function 'canvas-browser-cdp-send)
               (lambda (method params &optional answer _session &rest _)
                 (push (cons method params) canvas-browser-test--commands)
                 (when (and answer (equal (plist-get params :expression) "document.title"))
                   (funcall answer '(:result (:value "A post : r/emacs")))))))
      (canvas-browser--paint-crisp (current-buffer)))
    (should (member "document.title" (canvas-browser-test--scripts)))
    (should (equal canvas-browser--title "A post : r/emacs"))))

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
  ;; WHEN chromium tells of a new address of a page that another page
  ;;      opened, which nobody here shows
  ;; THEN this buffer keeps its name and its address
  (canvas-browser-test--in-page
    (let ((name (buffer-name)))
      (canvas-browser--target-changed
       (list :targetInfo (list :targetId "SOMEONE-ELSE" :type "page" :openerId "A-STRANGER"
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
                 (lambda (method params &optional then _session &rest _)
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

(ert-deftest canvas-browser-the-text-of-a-page-is-searched-from-insert-state-too ()
  ;; GIVEN a page whose field took the focus, so that the keys go to the
  ;;       page, as an application does when it opens a talk
  ;; WHEN the key that searches the text of the page is looked up
  ;; THEN it searches the text of the page, as in normal state: without
  ;;      it the key searches the page buffer, which holds no text
  (canvas-browser-test--in-page
    (should (eq (key-binding (kbd "M-s M-l")) #'canvas-browser-search-text))
    (canvas-browser-insert-mode)
    (should (eq (key-binding (kbd "M-s M-l")) #'canvas-browser-search-text))))

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

(ert-deftest canvas-browser-a-shifted-motion-with-the-mark-holds-shift-once ()
  ;; GIVEN a field with the mark set
  ;; WHEN a motion is pressed with Shift held
  ;; THEN it goes to the page with Shift, counted once: the mark asks
  ;;      for Shift, and the key has it already
  (canvas-browser-test--in-page
    (canvas-browser-insert-mode)
    (canvas-browser-test--press "C-SPC")
    (dolist (case '(("S-<right>" 8) ("C-S-<right>" 10)))
      (setq canvas-browser-test--commands nil)
      (canvas-browser-test--press (car case))
      (should (equal (plist-get (car (canvas-browser-test--keys-sent)) :modifiers)
                     (cadr case))))))

(ert-deftest canvas-browser-a-key-held-with-control-types-no-text ()
  ;; GIVEN a page buffer in insert state
  ;; WHEN C-<return> is pressed
  ;; THEN Enter goes down with Control as a key that types nothing: a
  ;;      page that sends its form on that key gets the key, and no line
  ;;      break is typed into the field
  (canvas-browser-test--in-page
    (canvas-browser-insert-mode)
    (setq canvas-browser-test--commands nil)
    (canvas-browser-test--press "C-<return>")
    (let ((down (car (canvas-browser-test--keys-sent))))
      (should (equal (plist-get down :type) "rawKeyDown"))
      (should-not (plist-member down :text)))))

(ert-deftest canvas-browser-the-undo-keys-undo-in-the-field ()
  ;; GIVEN a page buffer in insert state
  ;; WHEN a key that undoes in Emacs is pressed, or one that redoes
  ;; THEN the key z goes to the page with Control, and with Shift as well
  ;;      for a redo, which undo and redo in a browser
  (canvas-browser-test--in-page
    (canvas-browser-insert-mode)
    (dolist (case '(("C-/" 2) ("C-_" 2) ("C-x u" 2) ("C-?" 10) ("C-M-_" 10)))
      (setq canvas-browser-test--commands nil)
      (canvas-browser-test--press (car case))
      (let ((down (car (canvas-browser-test--keys-sent))))
        (should (equal (list (car case) (plist-get down :key) (plist-get down :code)
                             (plist-get down :windowsVirtualKeyCode)
                             (plist-get down :modifiers))
                       (list (car case) "z" "KeyZ" 90 (cadr case))))))))

(ert-deftest canvas-browser-the-paste-keys-type-the-newest-kill ()
  ;; GIVEN a page buffer in insert state, and "pasted" as the newest kill
  ;; WHEN S-<insert> is pressed, or the middle button of the mouse
  ;; THEN the kill is typed into the field, as C-y types it
  (canvas-browser-test--in-page
    (canvas-browser-insert-mode)
    (let ((kill-ring (list "pasted")) (kill-ring-yank-pointer nil))
      (should (eq (key-binding [mouse-2]) 'canvas-browser-yank))
      (setq canvas-browser-test--commands nil)
      (canvas-browser-test--press "S-<insert>")
      (should (equal (plist-get (canvas-browser-test--params "Input.insertText") :text)
                     "pasted")))))

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

(defmacro canvas-browser-test--dragging (typing marked &rest body)
  "Run BODY in a page where a drag leaves MARKED as the text that is marked.
TYPING says whether the focus of the page takes typing after the drag."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'canvas-browser-cdp-send)
              (lambda (method params &optional answer _session &rest _)
                (push (cons method params) canvas-browser-test--commands)
                (when answer
                  (funcall
                   answer
                   (when (equal method "Runtime.evaluate")
                     (let ((script (plist-get params :expression)))
                       (list :result
                             (list :value
                                   (cond ((equal script "getSelection().toString()") ,marked)
                                         ((string-search "__canvasBrowserCaret" script)
                                          (list :box '(10 20 2 16) :text ,marked
                                                :region '(10 20 80 16)))
                                         (t (list :typing ,typing :box '(5 5 100 20)))))))))))))
     ,@body))

(defun canvas-browser-test--drag ()
  "Drag the mouse over this page, from the pixel 40 by 90 to 200 by 95."
  (canvas-browser-drag (list 'drag-mouse-1 (canvas-browser-test--at 40 90)
                             (canvas-browser-test--at 200 95))))

(ert-deftest canvas-browser-a-drag-over-text-gives-the-keys-to-the-caret ()
  ;; GIVEN a page buffer in normal state
  ;; WHEN a drag marks text of the page, outside any field
  ;; THEN the caret has the keys, with the mark set: the text is its
  ;;      region, which M-w copies and C-g drops, AND nothing is copied
  ;;      yet, as after a drag in a buffer
  (canvas-browser-test--in-page
    (let ((kill-ring nil) (mouse-drag-copy-region nil))
      (canvas-browser-test--dragging nil "marked words"
        (canvas-browser-test--drag))
      (should canvas-browser--caret)
      (should canvas-browser--caret-mark)
      (should (eq (current-local-map) canvas-browser-caret-map))
      (should (equal canvas-browser--caret-box '(:x 10 :y 20 :w 2 :h 16)))
      (should-not kill-ring))))

(ert-deftest canvas-browser-a-drag-in-a-field-leaves-the-caret-alone ()
  ;; GIVEN a page buffer in normal state
  ;; WHEN a drag marks text in a field
  ;; THEN the keys go to the page, AND the caret does not take them: a
  ;;      field copies its own mark
  (canvas-browser-test--in-page
    (canvas-browser-test--dragging t "marked words"
      (canvas-browser-test--drag))
    (should canvas-browser--insert)
    (should-not canvas-browser--caret)))

(ert-deftest canvas-browser-a-drag-that-marks-nothing-leaves-the-keys-with-emacs ()
  ;; GIVEN a page buffer in normal state
  ;; WHEN a drag marks no text, as one over a picture
  ;; THEN the caret does not take the keys
  (canvas-browser-test--in-page
    (canvas-browser-test--dragging nil ""
      (canvas-browser-test--drag))
    (should-not canvas-browser--caret)))

(ert-deftest canvas-browser-a-drag-copies-for-one-who-asked-emacs-for-that ()
  ;; GIVEN `mouse-drag-copy-region\=' set, as by one who wants a drag in a
  ;;       buffer to copy
  ;; WHEN a drag marks text of the page
  ;; THEN the text is the newest kill
  (canvas-browser-test--in-page
    (let ((kill-ring nil) (mouse-drag-copy-region t))
      (canvas-browser-test--dragging nil "marked words"
        (canvas-browser-test--drag))
      (should (equal (car kill-ring) "marked words")))))

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
              (lambda (method params &optional answer _session &rest _)
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

;;;; A file for a page, picked in dired

(require 'dired)

(defmacro canvas-browser-test--attaching (directory &rest body)
  "Run BODY in a page buffer, with DIRECTORY bound to a new directory.
The directory holds the files one.txt and two.txt and the directory
deeper, and it is where the page last took a file from.  The chromium
is no snap, so a file goes to the page as it is.  Whatever BODY leaves
of a pending choice of files is cleared away."
  (declare (indent 1))
  `(let* ((,directory (file-name-as-directory (make-temp-file "canvas-browser-attach-" t)))
          (canvas-browser--attach-directory ,directory))
     (dolist (name '("one.txt" "two.txt"))
       (write-region name nil (expand-file-name name ,directory) nil 'silent))
     (make-directory (expand-file-name "deeper" ,directory))
     (unwind-protect
         (cl-letf (((symbol-function 'canvas-browser-cdp-snap-p) #'ignore))
           (canvas-browser-test--in-page ,@body))
       (canvas-browser--attach-finish)
       (dolist (buffer (buffer-list))
         (when (string-prefix-p ,directory (buffer-local-value 'default-directory buffer))
           (kill-buffer buffer)))
       (delete-directory ,directory t))))

(defun canvas-browser-test--ask-for-files (multiple)
  "Have the page of this buffer ask for a file, or several with MULTIPLE.
Return the dired buffer in which they are picked."
  (canvas-browser-test--event
   "Page.fileChooserOpened"
   (list :frameId "F1" :mode (if multiple "selectMultiple" "selectSingle") :backendNodeId 7))
  (seq-find (lambda (buffer) (buffer-local-value 'canvas-browser-attach-mode buffer))
            (buffer-list)))

(defun canvas-browser-test--mark (&rest names)
  "Mark the files of NAMES in this dired buffer, and leave point on the last."
  (dolist (name names)
    (dired-goto-file (expand-file-name name default-directory))
    (dired-mark 1)
    (dired-goto-file (expand-file-name name default-directory))))

(ert-deftest canvas-browser-a-page-hands-its-file-chooser-to-emacs ()
  ;; GIVEN a page buffer
  ;; WHEN it opens its page
  ;; THEN chromium is told to hand the file chooser of the page over:
  ;;      its own dialog opens on a display that nobody sees
  (canvas-browser-test--in-page
    (should (eq (plist-get (canvas-browser-test--params "Page.setInterceptFileChooserDialog")
                           :enabled)
                t))))

(ert-deftest canvas-browser-a-page-that-asks-for-a-file-opens-dired ()
  ;; GIVEN a page buffer, and the directory that a file was last taken from
  ;; WHEN the page asks for a file
  ;; THEN a dired buffer of that directory is the buffer of the selected
  ;;      window, with the keys that send and cancel
  (canvas-browser-test--attaching directory
    (let ((dired (canvas-browser-test--ask-for-files nil)))
      (should dired)
      (should (eq dired (window-buffer (selected-window))))
      (with-current-buffer dired
        (should (derived-mode-p 'dired-mode))
        (should (equal default-directory directory))
        (should (eq (key-binding (kbd "C-c C-c")) #'canvas-browser-attach-send))
        (should (eq (key-binding (kbd "C-c C-k")) #'canvas-browser-attach-cancel))))))

(ert-deftest canvas-browser-the-file-at-point-goes-to-the-page ()
  ;; GIVEN a page that asked for one file, and dired with point on a file
  ;;       and nothing marked
  ;; WHEN C-c C-c is pressed
  ;; THEN the file is given to the field that asked, AND the keys of the
  ;;      choice are gone from the dired buffer
  (canvas-browser-test--attaching directory
    (let ((dired (canvas-browser-test--ask-for-files nil)))
      (with-current-buffer dired
        (dired-goto-file (expand-file-name "one.txt" directory))
        (setq canvas-browser-test--commands nil)
        (canvas-browser-attach-send))
      (let ((sent (canvas-browser-test--params "DOM.setFileInputFiles")))
        (should (equal (plist-get sent :files) (vector (expand-file-name "one.txt" directory))))
        (should (equal (plist-get sent :backendNodeId) 7)))
      (should-not (buffer-local-value 'canvas-browser-attach-mode dired)))))

(ert-deftest canvas-browser-the-marked-files-go-to-a-field-that-takes-several ()
  ;; GIVEN a page that asked for several files, and two files marked
  ;; WHEN C-c C-c is pressed
  ;; THEN both are given to the field
  (canvas-browser-test--attaching directory
    (with-current-buffer (canvas-browser-test--ask-for-files t)
      (canvas-browser-test--mark "one.txt" "two.txt")
      (setq canvas-browser-test--commands nil)
      (canvas-browser-attach-send))
    (should (equal (plist-get (canvas-browser-test--params "DOM.setFileInputFiles") :files)
                   (vector (expand-file-name "one.txt" directory)
                           (expand-file-name "two.txt" directory))))))

(ert-deftest canvas-browser-a-field-for-one-file-refuses-several ()
  ;; GIVEN a page that asked for one file, and two files marked
  ;; WHEN C-c C-c is pressed
  ;; THEN it is refused, nothing goes to the page, AND the page still
  ;;      waits, so that the marks can be put right
  (canvas-browser-test--attaching directory
    (let ((dired (canvas-browser-test--ask-for-files nil)))
      (with-current-buffer dired
        (canvas-browser-test--mark "one.txt" "two.txt")
        (setq canvas-browser-test--commands nil)
        (should-error (canvas-browser-attach-send) :type 'user-error))
      (should-not (canvas-browser-test--params "DOM.setFileInputFiles"))
      (should (buffer-local-value 'canvas-browser-attach-mode dired)))))

(ert-deftest canvas-browser-a-directory-is-no-file-to-attach ()
  ;; GIVEN a page that asked for a file, and dired with point on a directory
  ;; WHEN C-c C-c is pressed
  ;; THEN it is refused AND nothing goes to the page
  (canvas-browser-test--attaching directory
    (with-current-buffer (canvas-browser-test--ask-for-files nil)
      (dired-goto-file (expand-file-name "deeper" directory))
      (setq canvas-browser-test--commands nil)
      (should-error (canvas-browser-attach-send) :type 'user-error))
    (should-not (canvas-browser-test--params "DOM.setFileInputFiles"))))

(ert-deftest canvas-browser-the-choice-follows-into-another-directory ()
  ;; GIVEN a page that asked for a file
  ;; WHEN dired opens another directory while the page waits
  ;; THEN the keys that send and cancel are there as well, AND the
  ;;      directory a file is sent from is where the next choice starts
  (canvas-browser-test--attaching directory
    (canvas-browser-test--ask-for-files nil)
    (let ((deeper (expand-file-name "deeper/" directory)))
      (write-region "three" nil (expand-file-name "three.txt" deeper) nil 'silent)
      (with-current-buffer (dired-noselect deeper)
        (should canvas-browser-attach-mode)
        (dired-goto-file (expand-file-name "three.txt" deeper))
        (canvas-browser-attach-send))
      (should (equal canvas-browser--attach-directory deeper)))))

(ert-deftest canvas-browser-return-on-a-file-picks-it-for-the-page ()
  ;; GIVEN a page that asked for a file, and dired with point on a file
  ;; WHEN RET is pressed
  ;; THEN the file goes to the page: in a choice of files, RET picks,
  ;;      where it would else open the file in a buffer
  (canvas-browser-test--attaching directory
    (with-current-buffer (canvas-browser-test--ask-for-files nil)
      (dired-goto-file (expand-file-name "one.txt" directory))
      (setq canvas-browser-test--commands nil)
      (should (eq (key-binding (kbd "RET")) #'canvas-browser-attach-open))
      (canvas-browser-attach-open))
    (should (equal (plist-get (canvas-browser-test--params "DOM.setFileInputFiles") :files)
                   (vector (expand-file-name "one.txt" directory))))))

(ert-deftest canvas-browser-return-on-a-directory-goes-into-it ()
  ;; GIVEN a page that asked for a file, and dired with point on a directory
  ;; WHEN RET is pressed
  ;; THEN dired shows that directory, nothing goes to the page, AND the
  ;;      page still waits
  (canvas-browser-test--attaching directory
    (with-current-buffer (canvas-browser-test--ask-for-files nil)
      (dired-goto-file (expand-file-name "deeper" directory))
      (setq canvas-browser-test--commands nil)
      (canvas-browser-attach-open)
      (should (equal default-directory (expand-file-name "deeper/" directory)))
      (should canvas-browser-attach-mode))
    (should-not (canvas-browser-test--params "DOM.setFileInputFiles"))
    (should canvas-browser--chooser)))

(ert-deftest canvas-browser-a-page-that-is-killed-waits-for-no-file ()
  ;; GIVEN a page that asked for a file
  ;; WHEN its buffer lets go of its page, as it does when it is killed
  ;; THEN no page waits any more, AND the keys of the choice are gone
  ;;      from the dired buffer
  (canvas-browser-test--attaching directory
    (let ((dired (canvas-browser-test--ask-for-files nil)))
      ;; A temporary buffer runs no hook when it is killed, so the
      ;; function of the hook is called.
      (canvas-browser--release)
      (should-not canvas-browser--chooser)
      (should-not (buffer-local-value 'canvas-browser-attach-mode dired)))))

(ert-deftest canvas-browser-cancelling-tells-the-field-that-nothing-was-chosen ()
  ;; GIVEN a page that asked for a file
  ;; WHEN C-c C-k is pressed in dired
  ;; THEN the field that asked gets a cancel event, as it does when a
  ;;      file dialog is closed, AND the keys of the choice are gone
  (canvas-browser-test--attaching directory
    (cl-letf (((symbol-function 'canvas-browser-cdp-send)
               (lambda (method params &optional answer _session &rest _)
                 (push (cons method params) canvas-browser-test--commands)
                 (when answer
                   (funcall answer (when (equal method "DOM.resolveNode")
                                     '(:object (:objectId "O1"))))))))
      (let ((dired (canvas-browser-test--ask-for-files nil)))
        (with-current-buffer dired (canvas-browser-attach-cancel))
        (should (equal (plist-get (canvas-browser-test--params "DOM.resolveNode") :backendNodeId)
                       7))
        (let ((call (canvas-browser-test--params "Runtime.callFunctionOn")))
          (should (equal (plist-get call :objectId) "O1"))
          (should (string-search "'cancel'" (plist-get call :functionDeclaration))))
        (should-not (buffer-local-value 'canvas-browser-attach-mode dired))))))

(ert-deftest canvas-browser-a-file-a-snap-cannot-read-is-copied-for-it ()
  ;; GIVEN a snap chromium, and a file in a place that a snap cannot read
  ;; WHEN the file is sent to the page
  ;; THEN the page gets a copy of it in the directory of the snap
  (canvas-browser-test--attaching directory
    (let ((snap-home (file-name-as-directory (make-temp-file "canvas-browser-snap-" t))))
      (unwind-protect
          (cl-letf (((symbol-function 'canvas-browser-cdp-snap-p) (lambda () t))
                    ((symbol-function 'canvas-browser--snap-can-read-p) (lambda (_file) nil))
                    ((symbol-function 'canvas-browser-cdp-snap-home) (lambda () snap-home)))
            (with-current-buffer (canvas-browser-test--ask-for-files nil)
              (dired-goto-file (expand-file-name "one.txt" directory))
              (setq canvas-browser-test--commands nil)
              (canvas-browser-attach-send))
            (let ((sent (aref (plist-get (canvas-browser-test--params "DOM.setFileInputFiles")
                                         :files)
                              0)))
              (should (string-prefix-p snap-home sent))
              (should (file-exists-p sent))))
        (delete-directory snap-home t)))))

;;;; The targets of embark

(defvar embark-target-finders)
(defvar embark-keymap-alist)
(defvar embark-general-map)
(defvar embark-target-injection-hooks)

(ert-deftest canvas-browser-embark-acts-on-the-address-of-the-page ()
  ;; GIVEN a page buffer that shows https://example.org
  ;; WHEN embark asks for its targets
  ;; THEN the address is the target, as a page, which has the actions
  ;;      that embark has for any URL and those of a page beside them
  (canvas-browser-test--in-page
    (should (equal (canvas-browser-embark-target)
                   '((canvas-browser-page . "https://example.org"))))))

(ert-deftest canvas-browser-embark-acts-on-the-region-of-the-caret-first ()
  ;; GIVEN a page whose caret marks the words " words"
  ;; WHEN embark asks for its targets
  ;; THEN the marked text comes first AND the address of the page after
  ;;      it, which embark reaches by cycling
  (canvas-browser-test--in-page
    (canvas-browser-test--answering '(:box (10 20 2 16) :text " words" :region (10 20 50 16))
      (canvas-browser-caret-mode)
      (canvas-browser-test--press "C-SPC")
      (canvas-browser-test--press "M-f"))
    (should (equal (canvas-browser-embark-target)
                   '((canvas-browser-text . " words")
                     (canvas-browser-page . "https://example.org"))))))

(ert-deftest canvas-browser-embark-acts-on-the-part-that-is-picked-before-the-page ()
  ;; GIVEN a page where a part is picked
  ;; WHEN embark asks for its targets
  ;; THEN the part comes before the page, both named by the address
  (canvas-browser-test--in-page
    (setq canvas-browser--part '(:place 1 :scrolls nil))
    (should (equal (canvas-browser-embark-target)
                   '((canvas-browser-part . "https://example.org")
                     (canvas-browser-page . "https://example.org"))))))

(ert-deftest canvas-browser-embark-forgets-the-region-with-the-caret ()
  ;; GIVEN a page whose caret marked text, AND the caret left since
  ;; WHEN embark asks for its targets
  ;; THEN the page is the only target
  (canvas-browser-test--in-page
    (canvas-browser-test--answering '(:box (10 20 2 16) :text " words" :region (10 20 50 16))
      (canvas-browser-caret-mode)
      (canvas-browser-test--press "C-SPC")
      (canvas-browser-test--press "M-f"))
    (canvas-browser-test--answering '(:box nil :text "")
      (canvas-browser-caret-leave))
    (should (equal (canvas-browser-embark-target)
                   '((canvas-browser-page . "https://example.org"))))))

(ert-deftest canvas-browser-embark-finds-nothing-outside-a-page ()
  ;; GIVEN a buffer that is no page, and a page with no address yet
  ;; WHEN embark asks for its targets in each
  ;; THEN there are none
  (with-temp-buffer
    (should-not (canvas-browser-embark-target))
    (canvas-browser-mode)
    (should-not (canvas-browser-embark-target))))

(ert-deftest canvas-browser-embark-looks-at-a-page-through-its-own-finder-alone ()
  ;; GIVEN a page buffer
  ;; WHEN the target finders of embark are read in it
  ;; THEN there is one, the finder of canvas-browser: the buffer holds
  ;;      one character that shows the picture of the page, and a finder
  ;;      that reads text takes that character as its target, so that
  ;;      embark shows the picture in its prompt when it cycles to it
  (canvas-browser-test--in-page
    (should (local-variable-p 'embark-target-finders))
    (should (equal embark-target-finders '(canvas-browser-embark-target)))))

(ert-deftest canvas-browser-embark-setup-reaches-the-pages-that-are-open ()
  ;; GIVEN a page buffer made before embark loaded, with no finders of
  ;;       its own
  ;; WHEN canvas-browser sets itself up with embark
  ;; THEN that buffer has the finder of canvas-browser alone as well
  (canvas-browser-test--in-page
    (kill-local-variable 'embark-target-finders)
    (let ((embark-keymap-alist nil)
          (embark-target-injection-hooks nil)
          (embark-general-map (make-sparse-keymap)))
      (canvas-browser--embark-setup))
    (should (equal (buffer-local-value 'embark-target-finders (current-buffer))
                   '(canvas-browser-embark-target)))))

(ert-deftest canvas-browser-embark-is-told-of-the-targets-when-it-loads ()
  ;; GIVEN embark with no keymaps of its own
  ;; WHEN canvas-browser sets itself up with it
  ;; THEN the marked text has a keymap whose parent holds the actions of
  ;;      embark for any target, where s searches for the text in a page
  ;;      buffer of its own
  (let ((embark-keymap-alist nil)
        (embark-target-injection-hooks nil)
        (embark-general-map (make-sparse-keymap)))
    (canvas-browser--embark-setup)
    (should (equal (alist-get 'canvas-browser-text embark-keymap-alist)
                   '(canvas-browser-embark-text-map)))
    (should (eq (keymap-parent canvas-browser-embark-text-map) embark-general-map))
    (should (eq (keymap-lookup canvas-browser-embark-text-map "s") #'canvas-browser))))

(ert-deftest canvas-browser-embark-gives-a-page-the-actions-of-a-url-and-its-own ()
  ;; GIVEN embark with no keymaps of its own
  ;; WHEN canvas-browser sets itself up with it
  ;; THEN a page has the keymap of a page before the one embark has for
  ;;      any URL, so the actions on a URL stay
  (let ((embark-keymap-alist nil)
        (embark-target-injection-hooks nil)
        (embark-general-map (make-sparse-keymap)))
    (canvas-browser--embark-setup)
    (should (equal (alist-get 'canvas-browser-page embark-keymap-alist)
                   '(canvas-browser-embark-page-map embark-url-map)))
    (should (equal (alist-get 'canvas-browser-part embark-keymap-alist)
                   '(canvas-browser-embark-part-map embark-url-map)))))

(ert-deftest canvas-browser-embark-sends-the-target-to-a-receiver ()
  ;; GIVEN embark with no keymaps of its own
  ;; WHEN canvas-browser sets itself up with it
  ;; THEN > sends marked text to a receiver, and sends the whole page
  ;;      when the page is the target AND embark does not answer the
  ;;      question of which receiver with the target
  (let ((embark-keymap-alist nil)
        (embark-target-injection-hooks nil)
        (embark-general-map (make-sparse-keymap)))
    (canvas-browser--embark-setup)
    (should (eq (keymap-lookup canvas-browser-embark-text-map ">")
                #'canvas-browser-send-to))
    (should (eq (keymap-lookup canvas-browser-embark-page-map ">")
                #'canvas-browser-send-page-to))
    (should (eq (keymap-lookup canvas-browser-embark-part-map ">")
                #'canvas-browser-send-part-to))
    (should (equal (alist-get 'canvas-browser-send-part-to embark-target-injection-hooks)
                   '(embark--ignore-target)))
    (should (equal (alist-get 'canvas-browser-send-to embark-target-injection-hooks)
                   '(embark--ignore-target)))
    (should (equal (alist-get 'canvas-browser-send-page-to embark-target-injection-hooks)
                   '(embark--ignore-target)))))

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


;;;; Sending the page to a receiver

(defvar org-capture-templates)
(defvar org-roam-directory)
(defvar org-roam-db-location)
(defvar org-id-track-globally)

(defvar canvas-browser-test--received nil
  "What the receiver of a test was given.")

(defun canvas-browser-test--receive (content)
  "Keep CONTENT, as the receiver of a test."
  (setq canvas-browser-test--received content))

(defmacro canvas-browser-test--sending (&rest body)
  "Run BODY in a page titled \"A page\" whose one receiver keeps what it gets.
The question of which receiver is answered with that one."
  (declare (indent 0))
  `(canvas-browser-test--in-page
     (let ((canvas-browser-test--received nil)
           (canvas-browser-receivers '(("test" . canvas-browser-test--receive))))
       (setq canvas-browser--title "A page")
       (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "test")))
         ,@body))))

(defun canvas-browser-test--mark-words ()
  "Have the caret of this page mark the text \" words\"."
  (canvas-browser-test--answering '(:box (10 20 2 16) :text " words" :region (10 20 50 16))
    (canvas-browser-caret-mode)
    (canvas-browser-test--press "C-SPC")
    (canvas-browser-test--press "M-f")))

(ert-deftest canvas-browser-send-to-gives-the-marked-text-to-the-receiver ()
  ;; GIVEN a page whose caret marks the text " words"
  ;; WHEN the page is sent to a receiver
  ;; THEN the receiver gets that text, with the address and the title of
  ;;      the page it comes from
  (canvas-browser-test--sending
    (canvas-browser-test--mark-words)
    (canvas-browser-send-to)
    (should (equal canvas-browser-test--received
                   '(:kind text :text " words"
                     :url "https://example.org" :title "A page")))))

(ert-deftest canvas-browser-send-to-gives-the-text-of-the-page-when-none-is-marked ()
  ;; GIVEN a page whose text is two lines, with nothing marked
  ;; WHEN the page is sent to a receiver
  ;; THEN the receiver gets the text of the whole page
  (canvas-browser-test--sending
    (canvas-browser-test--answering "one\ntwo"
      (canvas-browser-send-to))
    (should (equal canvas-browser-test--received
                   '(:kind page :text "one\ntwo"
                     :url "https://example.org" :title "A page")))))

(ert-deftest canvas-browser-send-page-to-gives-the-page-also-when-text-is-marked ()
  ;; GIVEN a page whose caret marks text
  ;; WHEN the whole page is sent to a receiver
  ;; THEN the receiver gets the text of the page, not the marked text
  (canvas-browser-test--sending
    (canvas-browser-test--mark-words)
    (canvas-browser-test--answering "one\ntwo"
      (canvas-browser-send-page-to))
    (should (eq (plist-get canvas-browser-test--received :kind) 'page))
    (should (equal (plist-get canvas-browser-test--received :text) "one\ntwo"))))

(defmacro canvas-browser-test--with-part (box text &rest body)
  "Run BODY in a page where the second part is picked.
The page gives BOX as the box of the part in the window and TEXT as its
text; nil for either is the null of JSON, which the page gives for a
part that has gone, and for a box out of the window.  The text of the
whole page is \"all of it\".  What is drawn goes to
`canvas-browser-test--drawn', and the drawing itself is left out."
  (declare (indent 2))
  `(let ((canvas-browser-test--drawn nil))
     (setq canvas-browser--part '(:place 1 :scrolls nil))
     (cl-letf (((symbol-function 'canvas-browser-cdp-send)
                (lambda (method params &optional answer _session &rest _)
                  (push (cons method params) canvas-browser-test--commands)
                  (when answer
                    (let ((script (or (plist-get params :expression) "")))
                      (funcall answer
                               (list :result
                                     (list :value
                                           (cond ((string-search "shown(part)" script)
                                                  (or ,box :null))
                                                 ((string-search "__canvasBrowserParts[1]" script)
                                                  (or ,text :null))
                                                 (t "all of it")))))))))
               ((symbol-function 'canvas-browser--draw-tint)
                (lambda (box) (push (cons 'tint box) canvas-browser-test--drawn)))
               ((symbol-function 'canvas-browser--paint-window)
                (lambda () (push 'repaint canvas-browser-test--drawn))))
       ,@body)))

(ert-deftest canvas-browser-send-to-gives-the-part-that-is-picked ()
  ;; GIVEN a page where a part is picked, and nothing is marked
  ;; WHEN the page is sent to a receiver
  ;; THEN the receiver gets the text of that part, not of the whole page
  (canvas-browser-test--sending
    (canvas-browser-test--with-part '(:x 0 :y 40 :w 600 :h 500) "the part"
      (canvas-browser-send-to))
    (should (equal canvas-browser-test--received
                   '(:kind part :text "the part"
                     :url "https://example.org" :title "A page")))))

(ert-deftest canvas-browser-marked-text-goes-before-the-part-that-is-picked ()
  ;; GIVEN a page where a part is picked, AND a caret that marks text
  ;; WHEN the page is sent to a receiver
  ;; THEN the receiver gets the marked text
  (canvas-browser-test--sending
    (canvas-browser-test--mark-words)
    (canvas-browser-test--with-part '(:x 0 :y 40 :w 600 :h 500) "the part"
      (canvas-browser-send-to))
    (should (eq (plist-get canvas-browser-test--received :kind) 'text))))

(ert-deftest canvas-browser-the-part-is-tinted-while-the-receiver-is-asked-for ()
  ;; GIVEN a page where a part is picked, which the window shows
  ;; WHEN the part is sent to a receiver
  ;; THEN the part is tinted when the receiver is asked for, so that what
  ;;      goes there shows, AND the receiver runs after a new picture of
  ;;      the page took the tint off
  (canvas-browser-test--sending
    (let* ((at-the-question nil)
           (at-the-receiver nil)
           (canvas-browser-receivers
            (list (cons "test" (lambda (_content)
                                 (setq at-the-receiver
                                       (list canvas-browser--hinting
                                             (car canvas-browser-test--drawn))))))))
      (canvas-browser-test--with-part '(:x 0 :y 40 :w 600 :h 500) "the part"
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _)
                     (setq at-the-question (list canvas-browser--hinting
                                                 (car canvas-browser-test--drawn)))
                     "test")))
          (canvas-browser-send-part-to)))
      (should (equal at-the-question '(t (tint :x 0 :y 40 :w 600 :h 500))))
      (should (equal at-the-receiver '(nil repaint))))))

(ert-deftest canvas-browser-a-part-out-of-the-window-is-sent-without-a-tint ()
  ;; GIVEN a page where a part is picked, which is scrolled out of view
  ;; WHEN the part is sent to a receiver
  ;; THEN nothing is drawn, AND the receiver gets its text all the same
  (canvas-browser-test--sending
    (canvas-browser-test--with-part '(:x 0 :y 0 :w 600 :h -20) "the part"
      (canvas-browser-send-part-to)
      (should-not canvas-browser-test--drawn))
    (should (equal (plist-get canvas-browser-test--received :text) "the part"))))

(ert-deftest canvas-browser-a-part-that-has-gone-is-not-sent ()
  ;; GIVEN a page where a part was picked, which the page has thrown away
  ;; WHEN the part is sent to a receiver
  ;; THEN an error says that the part has gone, AND nothing is sent, AND
  ;;      no part is picked any more
  (canvas-browser-test--sending
    (canvas-browser-test--with-part nil nil
      (should (string-search "has gone"
                             (cadr (should-error (canvas-browser-send-part-to)
                                                 :type 'user-error)))))
    (should-not canvas-browser-test--received)
    (should-not canvas-browser--part)))

(ert-deftest canvas-browser-text-shows-the-part-that-is-picked ()
  ;; GIVEN a page where a part is picked
  ;; WHEN the text of the page is asked for
  ;; THEN the buffer holds the text of that part, not of the whole page,
  ;;      as what is sent to a receiver is
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (canvas-browser-test--with-part '(:x 0 :y 40 :w 600 :h 500) "the part"
        (canvas-browser-text)))
    (let ((text-buffer (canvas-browser--text-buffer)))
      (unwind-protect
          (with-current-buffer text-buffer
            (should (equal (buffer-string) "the part")))
        (kill-buffer text-buffer)))))

(ert-deftest canvas-browser-text-of-a-part-that-has-gone-says-so ()
  ;; GIVEN a page where a part was picked, which the page has thrown away
  ;; WHEN the text of the page is asked for
  ;; THEN an error says that the part has gone, AND no part is picked
  ;;      any more, so that the next time shows the whole page
  (canvas-browser-test--in-page
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (canvas-browser-test--with-part nil nil
        (should (string-search "has gone"
                               (cadr (should-error (canvas-browser-text)
                                                   :type 'user-error))))))
    (should-not canvas-browser--part)))

(defmacro canvas-browser-test--asked-scripts (answer &rest body)
  "Run BODY in a page that answers every script with ANSWER.
Return the scripts that BODY had the page run, oldest first."
  (declare (indent 1))
  `(progn
     (setq canvas-browser-test--commands nil)
     (cl-letf (((symbol-function 'canvas-browser-cdp-send)
                (lambda (method params &optional answer _session &rest _)
                  (push (cons method params) canvas-browser-test--commands)
                  (when answer (funcall answer (list :result (list :value ,answer)))))))
       ,@body)
     (reverse (canvas-browser-test--scripts))))

(ert-deftest canvas-browser-the-text-that-is-sent-is-read-by-one-script ()
  ;; GIVEN a page, first with no part picked and then with one
  ;; WHEN the text to send is read
  ;; THEN the script that reads the text of an element is run on the body
  ;;      of the page, and then on the part, AND what it gives comes back
  ;;      as it is
  (canvas-browser-test--in-page
    (should (equal (canvas-browser-test--asked-scripts "words"
                     (should (equal (canvas-browser--page-text) "words")))
                   (list (concat canvas-browser--text-js "(document.body)"))))
    (setq canvas-browser--part '(:place 1 :scrolls nil))
    (let ((scripts (canvas-browser-test--asked-scripts "words"
                     (should (equal (canvas-browser--part-text) "words")))))
      (should (= (length scripts) 1))
      (should (string-search canvas-browser--text-js (car scripts)))
      (should (string-search "__canvasBrowserParts[1]" (car scripts))))))

(ert-deftest canvas-browser-the-html-of-what-is-sent-comes-from-the-page-as-it-is ()
  ;; GIVEN a page with a part picked, and what a receiver was given of
  ;;       it: a whole page, a part, and marked text
  ;; WHEN a receiver asks for the HTML of each
  ;; THEN the page is asked for its own HTML, the part for its own, and
  ;;      the selection for the HTML that it covers, AND each comes back
  ;;      as the page gave it
  (canvas-browser-test--in-page
    (setq canvas-browser--part '(:place 1 :scrolls nil))
    (should (equal (canvas-browser-test--asked-scripts "<p>x</p>"
                     (should (equal (canvas-browser-content-html '(:kind page)) "<p>x</p>")))
                   (list (concat canvas-browser--html-js "(document.body)"))))
    (let ((scripts (canvas-browser-test--asked-scripts "<p>x</p>"
                     (should (equal (canvas-browser-content-html '(:kind part)) "<p>x</p>")))))
      (should (string-search canvas-browser--html-js (car scripts)))
      (should (string-search "__canvasBrowserParts[1]" (car scripts))))
    (let ((scripts (canvas-browser-test--asked-scripts "<p>x</p>"
                     (should (equal (canvas-browser-content-html '(:kind text)) "<p>x</p>")))))
      (should (string-search "getSelection()" (car scripts)))
      (should (string-search "cloneContents()" (car scripts))))))

(ert-deftest canvas-browser-what-has-no-html-gives-none ()
  ;; GIVEN marked text in a field, which has no HTML, so the page gives null
  ;; WHEN a receiver asks for its HTML
  ;; THEN there is none
  (canvas-browser-test--in-page
    (canvas-browser-test--asked-scripts :null
      (should-not (canvas-browser-content-html '(:kind text))))))

(defmacro canvas-browser-test--with-page-of (html text &rest body)
  "Run BODY in a page that gives HTML as its HTML and TEXT as its text."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'canvas-browser-cdp-send)
              (lambda (method params &optional answer _session &rest _)
                (push (cons method params) canvas-browser-test--commands)
                (when answer
                  (funcall answer
                           (list :result
                                 (list :value
                                       (if (string-search canvas-browser--html-js
                                                          (or (plist-get params :expression) ""))
                                           ,html
                                         ,text)))))))
             ((symbol-function 'display-buffer) #'ignore))
     ,@body))

(defun canvas-browser-test--html-asked-p ()
  "Whether the page was asked for HTML."
  (cl-some (lambda (script) (string-search canvas-browser--html-js script))
           (canvas-browser-test--scripts)))

(ert-deftest canvas-browser-a-page-with-a-translator-is-sent-as-its-translation ()
  ;; GIVEN a page at an address that has a translator, which makes text
  ;;       of the comments in the HTML of the page
  ;; WHEN the page is sent to a receiver
  ;; THEN the receiver gets the text that the translator made, not the
  ;;      text that the page shows, AND the thread as data beside it
  (skip-unless (libxml-available-p))
  (canvas-browser-test--sending
    (let ((canvas-browser-translators
           (list (cons "example\\.org"
                       (canvas-browser-thread-translator
                        '(x-comment :author (attr by) :body (inside slot "comment")))))))
      (canvas-browser-test--with-page-of
          "<x-comment by=\"alice\"><div slot=\"comment\">Hi.</div><button>Reply</button></x-comment>"
          "alice Hi. Reply"
        (canvas-browser-send-to)))
    (should (equal (plist-get canvas-browser-test--received :text) "alice\n\nHi."))
    (should (equal (plist-get canvas-browser-test--received :thread)
                   '((:author "alice" :text "Hi."))))
    (should (equal (plist-get canvas-browser-test--received :url) "https://example.org"))))

(ert-deftest canvas-browser-a-translator-that-finds-nothing-leaves-the-text-of-the-page ()
  ;; GIVEN a page at an address that has a translator, which finds
  ;;       nothing to translate in what is sent
  ;; WHEN the page is sent to a receiver
  ;; THEN the receiver gets the text that the page shows, AND a message
  ;;      says that the translator found nothing, so that a snippet that
  ;;      does not fit the page is seen
  (skip-unless (libxml-available-p))
  (canvas-browser-test--sending
    (let ((canvas-browser-translators (list (cons "example\\.org" #'ignore)))
          (said nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest args) (setq said (apply #'format format args)))))
        (canvas-browser-test--with-page-of "<p>words</p>" "plain words"
          (canvas-browser-send-to)))
      (should (string-search "found nothing" said))
      (should (string-search "example\\.org" said)))
    (should (equal (plist-get canvas-browser-test--received :text) "plain words"))))

(ert-deftest canvas-browser-a-page-without-a-translator-is-not-asked-for-its-html ()
  ;; GIVEN a page at an address that no translator matches
  ;; WHEN the page is sent to a receiver
  ;; THEN the receiver gets the text that the page shows, AND the page
  ;;      was not asked for its HTML, which nobody reads then
  (canvas-browser-test--sending
    (let ((canvas-browser-translators (list (cons "elsewhere\\.org" #'ignore))))
      (setq canvas-browser-test--commands nil)
      (canvas-browser-test--with-page-of "<p>words</p>" "plain words"
        (canvas-browser-send-to))
      (should-not (canvas-browser-test--html-asked-p)))
    (should (equal (plist-get canvas-browser-test--received :text) "plain words"))))

(ert-deftest canvas-browser-the-part-that-is-picked-is-translated-too ()
  ;; GIVEN a page with a translator, where a part is picked
  ;; WHEN the part is sent, and when its text is shown
  ;; THEN the receiver gets the translation of the HTML of that part,
  ;;      AND the buffer with the text holds the same
  (skip-unless (libxml-available-p))
  (canvas-browser-test--sending
    (setq canvas-browser--part '(:place 1 :scrolls nil))
    (let ((canvas-browser-translators
           (list (cons "example\\.org" (lambda (dom) (format "%d paragraphs"
                                                            (length (dom-by-tag dom 'p))))))))
      (canvas-browser-test--with-page-of "<p>one</p><p>two</p>" "one two"
        (cl-letf (((symbol-function 'canvas-browser--part-box) #'ignore))
          (canvas-browser-send-to))
        (canvas-browser-text)))
    (should (equal (plist-get canvas-browser-test--received :kind) 'part))
    (should (equal (plist-get canvas-browser-test--received :text) "2 paragraphs"))
    (let ((text-buffer (canvas-browser--text-buffer)))
      (unwind-protect
          (should (equal (with-current-buffer text-buffer (buffer-string)) "2 paragraphs"))
        (kill-buffer text-buffer)))))

(ert-deftest canvas-browser-sending-a-part-needs-a-part ()
  ;; GIVEN a page where no part is picked
  ;; WHEN a part is sent
  ;; THEN an error says how a part is picked
  (canvas-browser-test--sending
    (should (string-search "no part of the page is picked"
                           (cadr (should-error (canvas-browser-send-part-to)
                                               :type 'user-error))))))

(ert-deftest canvas-browser-send-to-calls-the-receiver-in-the-page-buffer ()
  ;; GIVEN a page and a receiver that notes the buffer it runs in
  ;; WHEN the page is sent to it
  ;; THEN the receiver runs in the page buffer, where it can read more
  (canvas-browser-test--sending
    (let* ((page (current-buffer))
           (ran-in nil)
           (canvas-browser-receivers
            (list (cons "test" (lambda (_content) (setq ran-in (current-buffer)))))))
      (canvas-browser-test--answering "text"
        (canvas-browser-send-to))
      (should (eq ran-in page)))))

(ert-deftest canvas-browser-send-to-without-receivers-says-so ()
  ;; GIVEN a page and an empty list of receivers
  ;; WHEN the page is sent
  ;; THEN an error says that there is no receiver, and the page is not read
  (canvas-browser-test--sending
    (let ((canvas-browser-receivers nil))
      (setq canvas-browser-test--commands nil)
      (should (string-search "no receiver"
                             (cadr (should-error (canvas-browser-send-to) :type 'user-error))))
      (should-not (canvas-browser-test--params "Runtime.evaluate")))))

(ert-deftest canvas-browser-send-to-gives-up-on-a-page-that-does-not-answer ()
  ;; GIVEN a page whose chromium never answers
  ;; WHEN the page is sent to a receiver
  ;; THEN an error names the wait, and the receiver gets nothing
  (canvas-browser-test--sending
    (let ((canvas-browser-cdp-timeout 0.05))
      (cl-letf (((symbol-function 'canvas-browser-cdp-send) #'ignore))
        (should (string-search "did not answer"
                               (cadr (should-error (canvas-browser-send-to))))))
      (should-not canvas-browser-test--received))))

(defmacro canvas-browser-test--failing-script (&rest body)
  "Run BODY in a page where the script of the page text fails with a TypeError."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'canvas-browser-cdp-send)
              (lambda (_method _params &optional answer _session &rest _)
                (when answer
                  (funcall answer
                           '(:result (:type "object" :subtype "error")
                             :exceptionDetails
                             (:text "Uncaught"
                              :exception (:description "TypeError: no split")))))))
             ((symbol-function 'display-buffer) #'ignore))
     ,@body))

(ert-deftest canvas-browser-send-to-says-so-when-the-page-cannot-give-its-text ()
  ;; GIVEN a page in which the script of the page text fails
  ;; WHEN the page is sent to a receiver
  ;; THEN an error names the failure, AND the receiver gets nothing, where
  ;;      it got an empty text before
  (canvas-browser-test--sending
    (canvas-browser-test--failing-script
      (should (string-search "TypeError: no split"
                             (cadr (should-error (canvas-browser-send-to))))))
    (should-not canvas-browser-test--received)))

(ert-deftest canvas-browser-text-says-so-when-the-page-cannot-give-its-text ()
  ;; GIVEN a page in which the script of the page text fails
  ;; WHEN the text of the page is asked for
  ;; THEN an error names the failure
  (canvas-browser-test--in-page
    (unwind-protect
        (canvas-browser-test--failing-script
          (should (string-search "TypeError: no split"
                                 (cadr (should-error (canvas-browser-text))))))
      (kill-buffer (canvas-browser--text-buffer)))))

(ert-deftest canvas-browser-the-receivers-that-come-with-it-are-functions ()
  ;; GIVEN the receivers the package comes with
  ;; WHEN they are read
  ;; THEN each has a name and a function that exists
  (let ((receivers (default-value 'canvas-browser-receivers)))
    (should (equal (mapcar #'car receivers) '("buffer" "org capture" "org-roam node")))
    (should (cl-every #'fboundp (mapcar #'cdr receivers)))))

(ert-deftest canvas-browser-the-buffer-receiver-inserts-the-text-at-point ()
  ;; GIVEN a buffer that no window shows, with point between a and b
  ;; WHEN text is sent to that buffer
  ;; THEN the text stands between a and b
  (with-temp-buffer
    (insert "ab")
    (goto-char 2)
    (let ((target (current-buffer)))
      (cl-letf (((symbol-function 'read-buffer) (lambda (&rest _) (buffer-name target))))
        (with-temp-buffer
          (canvas-browser-send-to-buffer '(:kind text :text "X" :url "u" :title "t"))))
      (should (equal (buffer-string) "aXb")))))

(ert-deftest canvas-browser-the-buffer-receiver-inserts-where-the-window-has-point ()
  ;; GIVEN a buffer that a window shows with its point after b, while the
  ;;       point of the buffer itself is before a
  ;; WHEN text is sent to that buffer
  ;; THEN the text stands after b, where the user sees the cursor
  (with-temp-buffer
    (insert "ab")
    (goto-char (point-min))
    (let ((target (current-buffer)))
      (save-window-excursion
        (let ((window (split-window)))
          (set-window-buffer window target)
          (set-window-point window 3))
        (cl-letf (((symbol-function 'read-buffer) (lambda (&rest _) (buffer-name target))))
          (with-temp-buffer
            (canvas-browser-send-to-buffer '(:kind text :text "X" :url "u" :title "t")))))
      (should (equal (buffer-string) "abX")))))

(ert-deftest canvas-browser-the-buffer-receiver-pastes-as-the-buffer-does ()
  ;; GIVEN a read-only buffer with a yank command of its own, as the
  ;;       buffer of a terminal has, which hands the text to its process
  ;; WHEN text is sent to that buffer
  ;; THEN the yank command of the buffer gets the text, and nothing is
  ;;      inserted AND the kill ring is as it was
  (with-temp-buffer
    (let* ((target (current-buffer))
           (pasted nil)
           (kill-ring (list "an old kill"))
           (kill-ring-yank-pointer kill-ring)
           (map (make-sparse-keymap)))
      (define-key map [remap yank]
                  (lambda () (interactive) (setq pasted (current-kill 0))))
      (use-local-map map)
      (setq buffer-read-only t)
      (cl-letf (((symbol-function 'read-buffer) (lambda (&rest _) (buffer-name target))))
        (with-temp-buffer
          (canvas-browser-send-to-buffer '(:kind text :text "X" :url "u" :title "t"))))
      (should (equal pasted "X"))
      (should (equal (buffer-string) ""))
      (should (equal kill-ring '("an old kill"))))))

(ert-deftest canvas-browser-the-org-capture-receiver-fills-a-template ()
  ;; GIVEN a capture template that takes a link to the page and the text
  ;; WHEN marked text is sent to org capture, and that template is chosen
  ;; THEN the note holds the link, named by the title, and the text
  (let* ((file (make-temp-file "canvas-browser-capture" nil ".org"))
         (org-capture-templates
          `(("t" "test" entry (file ,file) "* %a\n%i" :immediate-finish t)))
         (unread-command-events (listify-key-sequence "t")))
    (unwind-protect
        (progn
          (canvas-browser-send-to-org-capture
           '(:kind text :text "the words" :url "https://example.org/a" :title "A page"))
          (with-current-buffer (find-buffer-visiting file)
            (should (string-search "[[https://example.org/a][A page]]" (buffer-string)))
            (should (string-search "the words" (buffer-string)))))
      (when-let* ((buffer (find-buffer-visiting file)))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-file file))))

(defmacro canvas-browser-test--with-roam (directory &rest body)
  "Run BODY with org-roam in a new DIRECTORY of its own.
The templates of the receiver are as they come, but finished at once:
nobody is there to press C-c C-c.  The ids of the nodes stay out of the
file of ids of the user."
  (declare (indent 1))
  `(let* ((,directory (file-name-as-directory (make-temp-file "canvas-browser-roam" t)))
          (org-roam-directory ,directory)
          (org-roam-db-location (expand-file-name "roam.db" ,directory))
          (org-id-track-globally nil)
          (canvas-browser-org-roam-templates
           (mapcar (lambda (template) (append template '(:immediate-finish t)))
                   (default-value 'canvas-browser-org-roam-templates))))
     (unwind-protect
         (progn ,@body)
       (dolist (buffer (buffer-list))
         (when (and (buffer-file-name buffer)
                    (file-in-directory-p (buffer-file-name buffer) ,directory))
           (with-current-buffer buffer (set-buffer-modified-p nil))
           (kill-buffer buffer)))
       (delete-directory ,directory t))))

(defun canvas-browser-test--note (file)
  "What the note FILE holds."
  (with-current-buffer (find-file-noselect file) (buffer-string)))

(ert-deftest canvas-browser-the-org-roam-receiver-asks-which-node-takes-the-text ()
  ;; GIVEN org-roam with an empty directory
  ;; WHEN marked text is sent to org-roam, and a node that does not exist
  ;;      yet is named for it
  ;; THEN a node with the name that was given holds the text as a quote,
  ;;      under a heading that links to the page by its title, AND the
  ;;      title of the page is not the title of the node, where every
  ;;      note from one page had the title that the page happened to have
  (skip-unless (locate-library "org-roam"))
  (require 'org-roam)
  (canvas-browser-test--with-roam directory
    (cl-letf (((symbol-function 'org-roam-node-read)
               (lambda (&rest _) (org-roam-node-create :title "Reading notes"))))
      (canvas-browser-send-to-org-roam
       '(:kind text :text "the words" :url "https://example.org/a" :title "A page")))
    (let ((note (canvas-browser-test--note (expand-file-name "reading_notes.org" directory))))
      (should (string-search "#+title: Reading notes" note))
      ;; What was sent has a heading of its own, which links to the page.
      (should (string-search "\n* From [[https://example.org/a][A page]]\n" note))
      (should (string-search "#+begin_quote\nthe words\n#+end_quote" note))
      (should-not (string-search "#+title: A page" note)))))

(ert-deftest canvas-browser-the-org-receivers-write-a-thread-as-an-org-list ()
  ;; GIVEN what a translator made of a thread: the text, and the thread
  ;;       as data
  ;; WHEN it is sent to org capture, and to an org-roam node
  ;; THEN each note holds the thread as an org list, a reply inside the
  ;;      item it answers, and not the text for a reader, AND the note of
  ;;      org-roam has the list on its own, not in a quote
  (let* ((thread '((:author "alice" :text "What is the role?"
                    :replies ((:author "bob" :text "The drawing layer.")))))
         (content (list :kind 'page :text "alice\n\nWhat is the role?" :thread thread
                        :url "https://example.org/a" :title "A page"))
         (file (make-temp-file "canvas-browser-capture" nil ".org"))
         (org-capture-templates
          `(("t" "test" entry (file ,file) "* %a\n%i" :immediate-finish t)))
         (unread-command-events (listify-key-sequence "t")))
    (unwind-protect
        (progn
          (canvas-browser-send-to-org-capture content)
          (with-current-buffer (find-buffer-visiting file)
            (should (string-search
                     "- *alice*\n\n  What is the role?\n\n  - *bob*\n\n    The drawing layer."
                     (buffer-string)))))
      (when-let* ((buffer (find-buffer-visiting file)))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-file file))
    (skip-unless (locate-library "org-roam"))
    (require 'org-roam)
    (canvas-browser-test--with-roam directory
      (cl-letf (((symbol-function 'org-roam-node-read)
                 (lambda (&rest _) (org-roam-node-create :title "Roles"))))
        (canvas-browser-send-to-org-roam content))
      (let ((note (canvas-browser-test--note (expand-file-name "roles.org" directory))))
        (should (string-search
                 "- *alice*\n\n  What is the role?\n\n  - *bob*\n\n    The drawing layer."
                 note))
        (should-not (string-search "#+begin_quote" note))))))

(defconst canvas-browser-test--png
  (base64-decode-string
   "iVBORw0KGgoAAAANSUhEUgAAAAIAAAACCAIAAAD91JpzAAAAEklEQVR4nGNgYGD4z8DAwMAAAA0EAQBFH5uKAAAAAElFTkSuQmCC")
  "The bytes of a small picture.")

(defun canvas-browser-test--thread-with-picture (alt)
  "A thread of one item whose text holds a picture at plot.png, named ALT."
  (list (list :author "alice"
              :text (concat "Look: " (propertize (format "[%s]" alt)
                                                 'canvas-browser-picture "plot.png")))))

(ert-deftest canvas-browser-the-org-roam-receiver-attaches-the-pictures-of-a-thread ()
  ;; GIVEN a thread from a translator whose text holds a picture, AND a
  ;;       page that has that picture loaded
  ;; WHEN it is sent to a new org-roam node
  ;; THEN the picture is a file among the attachments of the node, as
  ;;      org-attach keeps them, AND the note links to it where the
  ;;      picture stood, so that org shows it there
  (skip-unless (locate-library "org-roam"))
  (require 'org-roam)
  (canvas-browser-test--in-page
    (let (asked)
      (canvas-browser-test--with-roam directory
        (cl-letf (((symbol-function 'org-roam-node-read)
                   (lambda (&rest _) (org-roam-node-create :title "Plots")))
                  ((symbol-function 'canvas-browser-picture-bytes)
                   (lambda (url) (setq asked url) canvas-browser-test--png)))
          (canvas-browser-send-to-org-roam
           (list :kind 'page :text "alice\n\nLook: [A plot]"
                 :thread (canvas-browser-test--thread-with-picture "A plot")
                 :url "https://example.org/a" :title "A page")))
        (should (equal asked "https://example.org/plot.png"))
        (let* ((note (canvas-browser-test--note (expand-file-name "plots.org" directory)))
               (id (and (string-match ":ID: +\\([^ \n]+\\)" note) (match-string 1 note)))
               (file (and (string-match "\\[\\[attachment:\\([^]]+\\)\\]\\]" note)
                          (match-string 1 note))))
          (should (string-search "Look: [[attachment:" note))
          (should file)
          (let ((default-directory directory))
            (should (equal (with-temp-buffer
                             (set-buffer-multibyte nil)
                             (insert-file-contents-literally
                              (expand-file-name file (org-attach-dir-from-id id)))
                             (buffer-string))
                           canvas-browser-test--png))))))))

(defun canvas-browser-test--picture-at-link (file)
  "The file that the first attachment link of the note FILE leads to, or nil.
It is found as org finds it, from where the link stands."
  (with-current-buffer (find-file-noselect file)
    (org-mode)
    (goto-char (point-min))
    (when (re-search-forward "\\[\\[attachment:\\([^]]+\\)\\]\\]" nil t)
      (let ((picture (ignore-errors (org-attach-expand (match-string 1)))))
        (and picture (file-exists-p picture) picture)))))

(ert-deftest canvas-browser-a-picture-under-the-heading-of-a-send-is-found-by-org ()
  ;; GIVEN a thread with a picture, sent to a new org-roam node
  ;; WHEN org looks for the picture from where its link stands, under the
  ;;      heading of the send
  ;; THEN it finds the file: the heading names the directory of the
  ;;      attachments, since org-attach does not look above a heading
  ;;      for the id of the note
  (skip-unless (locate-library "org-roam"))
  (require 'org-roam)
  (require 'org-attach)
  (canvas-browser-test--in-page
    (canvas-browser-test--with-roam directory
      (cl-letf (((symbol-function 'org-roam-node-read)
                 (lambda (&rest _) (org-roam-node-create :title "Plots")))
                ((symbol-function 'canvas-browser-picture-bytes)
                 (lambda (_url) canvas-browser-test--png)))
        (canvas-browser-send-to-org-roam
         (list :kind 'page :text "alice\n\nLook: [A plot]"
               :thread (canvas-browser-test--thread-with-picture "A plot")
               :url "https://example.org/a" :title "A page")))
      (let ((file (expand-file-name "plots.org" directory)))
        (should (string-match-p "^\\* From .*\n:PROPERTIES:\n:DIR: +data/"
                                (canvas-browser-test--note file)))
        (should (canvas-browser-test--picture-at-link file))))))

;; `org-startup-with-link-previews' is the name since org 9.8.
(defvar org-startup-with-link-previews)

(ert-deftest canvas-browser-a-send-with-a-picture-works-where-org-shows-pictures-at-once ()
  ;; GIVEN an org that shows the pictures of a note as soon as it opens
  ;;       one
  ;; WHEN a thread with a picture is sent to a new org-roam node
  ;; THEN the note holds the picture under a heading that names the
  ;;      directory of the attachments, where the capture stopped with
  ;;      "Need absolute `org-attach-id-dir'": org fills a template in a
  ;;      buffer without a file, shows the pictures there too, and
  ;;      org-attach takes no directory that is relative in such a buffer
  (skip-unless (locate-library "org-roam"))
  (require 'org-roam)
  (require 'org-attach)
  (skip-unless (boundp 'org-startup-with-link-previews))
  (canvas-browser-test--in-page
    (canvas-browser-test--with-roam directory
      (let ((org-startup-with-link-previews t))
        (cl-letf (((symbol-function 'org-roam-node-read)
                   (lambda (&rest _) (org-roam-node-create :title "Plots")))
                  ((symbol-function 'canvas-browser-picture-bytes)
                   (lambda (_url) canvas-browser-test--png)))
          (canvas-browser-send-to-org-roam
           (list :kind 'page :text "alice\n\nLook: [A plot]"
                 :thread (canvas-browser-test--thread-with-picture "A plot")
                 :url "https://example.org/a" :title "A page"))))
      (let ((file (expand-file-name "plots.org" directory)))
        (should (string-match-p "^\\* From .*\n:PROPERTIES:\n:DIR: +data/"
                                (canvas-browser-test--note file)))
        (should (canvas-browser-test--picture-at-link file))))))

(ert-deftest canvas-browser-a-merge-that-brings-a-picture-names-the-attachments ()
  ;; GIVEN a node whose heading holds a thread without a picture
  ;; WHEN the thread is sent again with a new message that has a picture
  ;; THEN the picture is attached, AND the heading of the thread names
  ;;      the directory of the attachments, so that org finds the picture
  (skip-unless (locate-library "org-roam"))
  (require 'org-roam)
  (require 'org-attach)
  (canvas-browser-test--in-page
    (canvas-browser-test--with-roam directory
      (cl-letf (((symbol-function 'org-roam-node-read)
                 (lambda (&rest _) (org-roam-node-create :title "Plots")))
                ((symbol-function 'canvas-browser-picture-bytes)
                 (lambda (_url) canvas-browser-test--png)))
        (canvas-browser-send-to-org-roam
         (list :kind 'page :text "bob\n\nNo picture."
               :thread (list (list :author "bob" :text "No picture."))
               :url "https://example.org/a" :title "A page"))
        (org-roam-db-sync)
        (cl-letf (((symbol-function 'org-roam-node-read)
                   (lambda (&rest _) (org-roam-node-from-title-or-alias "Plots"))))
          (canvas-browser-send-to-org-roam
           (list :kind 'page :text "bob\n\nNo picture.\n\nalice\n\nLook: [A plot]"
                 :thread (append (list (list :author "bob" :text "No picture."))
                                 (canvas-browser-test--thread-with-picture "A plot"))
                 :url "https://example.org/a" :title "A page"))))
      (let ((file (expand-file-name "plots.org" directory)))
        (should (canvas-browser-test--picture-at-link file))))))

(ert-deftest canvas-browser-a-picture-the-page-cannot-give-stays-as-its-name ()
  ;; GIVEN a thread whose text holds a picture that the page cannot give
  ;; WHEN it is sent to an org-roam node
  ;; THEN the note keeps the picture by its name, in brackets, AND no
  ;;      file is attached
  (skip-unless (locate-library "org-roam"))
  (require 'org-roam)
  (canvas-browser-test--in-page
    (canvas-browser-test--with-roam directory
      (cl-letf (((symbol-function 'org-roam-node-read)
                 (lambda (&rest _) (org-roam-node-create :title "Plots")))
                ((symbol-function 'canvas-browser-picture-bytes) #'ignore))
        (canvas-browser-send-to-org-roam
         (list :kind 'page :text "alice\n\nLook: [A plot]"
               :thread (canvas-browser-test--thread-with-picture "A plot")
               :url "https://example.org/a" :title "A page")))
      (should (string-search "Look: [A plot]"
                             (canvas-browser-test--note (expand-file-name "plots.org" directory))))
      (should-not (file-exists-p (expand-file-name "data" directory))))))

(ert-deftest canvas-browser-a-second-send-to-a-node-merges-the-thread ()
  ;; GIVEN a node that holds a thread from a first send
  ;; WHEN the thread is sent to that node again, with a new message and
  ;;      a new reply
  ;; THEN the note holds the thread once, with the new message and the
  ;;      new reply where they belong, under the one heading of the
  ;;      first send, AND a message counts what was added
  (skip-unless (locate-library "org-roam"))
  (require 'org-roam)
  (let* ((first (list (list :author "alice" :text "What is the role?")))
         (again (list (list :author "alice" :text "What is the role?"
                            :replies (list (list :author "bob" :text "The drawing layer.")))
                      (list :author "carol" :text "Nice.")))
         (content (lambda (thread)
                    (list :kind 'page :text "the text" :thread thread
                          :url "https://example.org/a" :title "A page")))
         (said nil))
    (canvas-browser-test--with-roam directory
      (cl-letf (((symbol-function 'org-roam-node-read)
                 (lambda (&rest _) (org-roam-node-create :title "Roles"))))
        (canvas-browser-send-to-org-roam (funcall content first)))
      (org-roam-db-sync)
      (cl-letf (((symbol-function 'org-roam-node-read)
                 (lambda (&rest _) (org-roam-node-from-title-or-alias "Roles")))
                ((symbol-function 'message)
                 (lambda (format &rest args) (setq said (apply #'format format args)))))
        (canvas-browser-send-to-org-roam (funcall content again)))
      (let ((note (canvas-browser-test--note (expand-file-name "roles.org" directory))))
        (should (= (cl-count-if (lambda (line) (string-prefix-p "* From " line))
                                (split-string note "\n"))
                   1))
        (should (string-search (canvas-browser-thread-org again) note)))
      (should (string-search "2 new" said)))))

(ert-deftest canvas-browser-the-org-roam-receiver-needs-no-name-that-a-package-may-take ()
  ;; GIVEN an Emacs where some package has a function called `body',
  ;;       which org-roam calls on the node when a template says ${body}
  ;; WHEN text is sent to org-roam with the template as it comes
  ;; THEN the note holds the text, where the capture failed with the
  ;;      error of that function
  (skip-unless (locate-library "org-roam"))
  (require 'org-roam)
  (canvas-browser-test--with-roam directory
    (cl-letf (((symbol-function 'body)
               (lambda (&rest _) (error "No applicable method: ws-body")))
              ((symbol-function 'org-roam-node-read)
               (lambda (&rest _) (org-roam-node-create :title "Reading notes"))))
      (canvas-browser-send-to-org-roam
       '(:kind text :text "the words\nin two lines" :url "https://example.org/a" :title "A page")))
    (should (string-search "#+begin_quote\nthe words\nin two lines\n#+end_quote"
                           (canvas-browser-test--note
                            (expand-file-name "reading_notes.org" directory))))))

(ert-deftest canvas-browser-the-org-roam-receiver-adds-to-a-node-that-exists ()
  ;; GIVEN org-roam with one node, which holds text sent from a page
  ;; WHEN text is sent from another page, and that node is named for it
  ;; THEN the node holds both texts, each under a link to its own page
  (skip-unless (locate-library "org-roam"))
  (require 'org-roam)
  (canvas-browser-test--with-roam directory
    (cl-letf (((symbol-function 'org-roam-node-read)
               (lambda (&rest _) (org-roam-node-create :title "Reading notes"))))
      (canvas-browser-send-to-org-roam
       '(:kind text :text "the words" :url "https://example.org/a" :title "A page")))
    (org-roam-db-sync)
    (cl-letf (((symbol-function 'org-roam-node-read)
               (lambda (&rest _) (org-roam-node-from-title-or-alias "Reading notes"))))
      (canvas-browser-send-to-org-roam
       '(:kind part :text "more words" :url "https://example.org/b" :title "B page")))
    (should (equal (directory-files directory nil "\\.org\\'") '("reading_notes.org")))
    (let ((note (canvas-browser-test--note (expand-file-name "reading_notes.org" directory))))
      (should (string-match-p
               (concat "\\[\\[https://example.org/a\\]\\[A page\\]\\]\\(.\\|\n\\)*the words"
                       "\\(.\\|\n\\)*\\[\\[https://example.org/b\\]\\[B page\\]\\]\\(.\\|\n\\)*more words")
               note)))))

(ert-deftest canvas-browser-the-send-key-is-bound-in-normal-and-caret-state ()
  ;; GIVEN a page buffer and its menu
  ;; WHEN > is looked up in normal state, in caret state and in the menu
  ;; THEN it sends the page to a receiver in all three
  (canvas-browser-test--in-page
    (should (eq (key-binding (kbd ">")) #'canvas-browser-send-to))
    (canvas-browser-test--answering '(:box (10 20 2 16) :text "")
      (canvas-browser-caret-mode))
    (should (eq (key-binding (kbd ">")) #'canvas-browser-send-to)))
  (should (eq (plist-get (canvas-browser-test--menu-entry ">") :command)
              'canvas-browser-send-to)))

;;;; What a page copies

(defmacro canvas-browser-test--with-clipboard (text &rest body)
  "Run BODY in a page where the clipboard of chromium holds TEXT.
TEXT is evaluated each time the page is asked.  The kill ring is empty
and its own, a timer runs at once, and no command is kept from before."
  (declare (indent 1))
  `(canvas-browser-test--in-page
     (let ((kill-ring nil)
           (kill-ring-yank-pointer nil)
           (interprogram-cut-function nil)
           (canvas-browser--clipboard nil)
           (send (symbol-function 'canvas-browser-cdp-send)))
       (setq canvas-browser-test--commands nil)
       (cl-letf (((symbol-function 'canvas-browser-cdp-send)
                  (lambda (method params &optional answer &rest rest)
                    (if (and (equal method "Runtime.evaluate")
                             (string-search "clipboard" (plist-get params :expression)))
                        (progn (push (cons method params) canvas-browser-test--commands)
                               (funcall answer (list :result (list :value ,text))))
                      (apply send method params answer rest))))
                 ((symbol-function 'run-at-time)
                  (lambda (_time _repeat function &rest args) (apply function args))))
         ,@body))))

(defun canvas-browser-test--clipboard-commands ()
  "What chromium was told about the clipboard, oldest first, each in a word."
  (delq nil
        (mapcar (lambda (command)
                  (pcase (car command)
                    ("Browser.setPermission"
                     (and (equal (plist-get (plist-get (cdr command) :permission) :name)
                                 "clipboard-read")
                          (list (plist-get (cdr command) :setting)
                                (plist-get (cdr command) :origin))))
                    ("Runtime.evaluate"
                     (and (string-search "clipboard" (plist-get (cdr command) :expression))
                          'read))))
                (reverse canvas-browser-test--commands))))

(ert-deftest canvas-browser-what-a-page-copies-goes-to-the-kill-ring ()
  ;; GIVEN a page whose button copies a link, as a button to copy does
  ;; WHEN the button is clicked
  ;; THEN the link is the newest kill, AND the reader is told, AND the
  ;;      site may read the clipboard only while Emacs reads it
  (canvas-browser-test--with-clipboard "https://example.org/a/link"
    (let ((said nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq said (apply #'format format-string args)))))
        (canvas-browser--click 10 10))
      (should (equal (current-kill 0) "https://example.org/a/link"))
      (should (string-search "https://example.org/a/link" said))
      (should (equal (canvas-browser-test--clipboard-commands)
                     '(("granted" "https://example.org") read
                       ("prompt" "https://example.org")))))))

(ert-deftest canvas-browser-a-click-that-copies-nothing-leaves-the-kill-ring ()
  ;; GIVEN a page that copied a link, which Emacs took
  ;; WHEN another click copies nothing, so the clipboard holds the same
  ;; THEN the kill ring holds the link once
  (canvas-browser-test--with-clipboard "https://example.org/a/link"
    (canvas-browser--click 10 10)
    (canvas-browser--click 20 20)
    (should (equal kill-ring '("https://example.org/a/link")))))

(ert-deftest canvas-browser-an-empty-clipboard-is-no-kill ()
  ;; GIVEN a page, and a clipboard that holds nothing
  ;; WHEN the page is clicked
  ;; THEN nothing is killed
  (canvas-browser-test--with-clipboard ""
    (canvas-browser--click 10 10)
    (should-not kill-ring)))

(ert-deftest canvas-browser-a-page-that-cannot-read-the-clipboard-is-no-kill ()
  ;; GIVEN a page that has no clipboard to read, as a page without https
  ;;       has none, so that the question gives nothing
  ;; WHEN the page is clicked
  ;; THEN nothing is killed, AND the site may not read the clipboard
  ;;      after the question
  (canvas-browser-test--with-clipboard nil
    (canvas-browser--click 10 10)
    (should-not kill-ring)
    (should (equal (car (last (canvas-browser-test--clipboard-commands)))
                   '("prompt" "https://example.org")))))

(ert-deftest canvas-browser-a-page-that-is-gone-is-not-asked ()
  ;; GIVEN a page that was clicked
  ;; WHEN its buffer is killed before Emacs looks at the clipboard
  ;; THEN chromium is told nothing about the clipboard
  (canvas-browser-test--with-clipboard "https://example.org/a/link"
    (let ((buffer (generate-new-buffer " *gone*")))
      (kill-buffer buffer)
      (canvas-browser--look-at-clipboard buffer)
      (should-not (canvas-browser-test--clipboard-commands))
      (should-not kill-ring))))

;;;; The notifications of a page

(defun canvas-browser-test--focus-told ()
  "What chromium was last told about the focus of the page: t, :json-false, or nil."
  (plist-get (canvas-browser-test--params "Emulation.setFocusEmulationEnabled") :enabled))

(ert-deftest canvas-browser-a-page-that-no-window-shows-is-looked-at-no-more ()
  ;; GIVEN a page that was told it has the focus, as the page you look at is
  ;; WHEN no window shows it any more
  ;; THEN chromium is told that the page has the focus no more, AND an
  ;;      empty tab is made, which is then in front of the page in
  ;;      chromium's window.  A site holds its notifications back while
  ;;      it believes that you look at it, and a page that was put away
  ;;      believed so for good: it had the focus, and it was the tab in
  ;;      front
  ;; WHEN it is looked at again
  ;; THEN it is told that it has the focus once more
  (let ((canvas-browser--rest-target nil))
    (canvas-browser-test--in-page
      (should canvas-browser--focused)
      (setq canvas-browser-test--commands nil)
      (canvas-browser--sleep)
      (should (eq (canvas-browser-test--focus-told) :json-false))
      (should-not canvas-browser--focused)
      (should (equal (canvas-browser-test--params "Target.createTarget") '(:url "about:blank")))
      (should (equal canvas-browser--rest-target "T1"))
      ;; The tab of the page was put in front before the empty tab came:
      ;; a page that was clicked gives up the focus of chromium only to
      ;; a tab that takes its place in front.
      (should (equal (mapcar #'car (last canvas-browser-test--commands 2))
                     '("Target.createTarget" "Target.activateTarget")))
      (should (equal (cdar (last canvas-browser-test--commands))
                     (list :targetId canvas-browser--target)))
      (setq canvas-browser-test--commands nil)
      (canvas-browser--wake t)
      (should (eq (canvas-browser-test--focus-told) t)))))

(ert-deftest canvas-browser-the-empty-tab-is-made-once-and-put-in-front-after ()
  ;; GIVEN chromium with the empty tab of an earlier page that was put away
  ;; WHEN a page that was looked at is put away
  ;; THEN the empty tab is put in front, AND no second one is made
  ;; WHEN the page is put away again, as each change of the windows does
  ;;      to a page that no window shows
  ;; THEN chromium is told nothing: the page was not looked at since
  (let ((canvas-browser--rest-target "REST"))
    (canvas-browser-test--in-page
      (setq canvas-browser-test--commands nil)
      (canvas-browser--sleep)
      ;; First the tab of the page, and then the empty tab.
      (should (equal (cl-loop for (method . params) in (reverse canvas-browser-test--commands)
                              when (equal method "Target.activateTarget")
                              collect (plist-get params :targetId))
                     (list canvas-browser--target "REST")))
      (should-not (canvas-browser-test--params "Target.createTarget"))
      (setq canvas-browser-test--commands nil)
      (canvas-browser--sleep)
      (should-not canvas-browser-test--commands))))

(ert-deftest canvas-browser-an-empty-tab-that-is-gone-is-made-again ()
  ;; GIVEN the id of an empty tab that chromium has no more, as after
  ;;       chromium started again
  ;; WHEN a page that was looked at is put away
  ;; THEN chromium refuses to put that tab in front, AND a new empty tab
  ;;      is made, AND the reader is told nothing
  (let ((canvas-browser--rest-target "GONE"))
    (canvas-browser-test--in-page
      (setq canvas-browser-test--commands nil)
      (canvas-browser-test--refusing "Target.activateTarget" 2 "No target with given id found"
        (canvas-browser--sleep)
        (should (equal (canvas-browser-test--params "Target.createTarget") '(:url "about:blank")))
        (should-not said)))))

(defun canvas-browser-test--scripts-before-load ()
  "The scripts that chromium was told to run before each page loads."
  (cl-loop for (method . params) in canvas-browser-test--commands
           when (equal method "Page.addScriptToEvaluateOnNewDocument")
           collect (plist-get params :source)))

(defun canvas-browser-test--page-tells (said &optional context)
  "Have the page of this buffer tell Emacs SAID about a notification.
SAID is a plist, and CONTEXT the place in the page that tells, 7 when nil."
  (canvas-browser-test--event "Runtime.bindingCalled"
                              (list :name "canvasBrowserNotify"
                                    :payload (json-encode said)
                                    :executionContextId (or context 7))))

(defconst canvas-browser-test--new-mail
  '(:event "show" :id 3 :title "New mail" :body "From alice" :tag "" :icon ""
    :origin "https://example.org")
  "What a page tells Emacs when it shows a notification.")

(defconst canvas-browser-test--notify-name
  '("Runtime.addBinding" :name "canvasBrowserNotify")
  "The command that gives a page the name to call Emacs by.")

(defmacro canvas-browser-test--deciding (answer &rest body)
  "Run BODY where the question about notifications is answered with ANSWER.
The question goes to `asked', and what is kept to `saved'."
  (declare (indent 1))
  `(let ((asked nil) (saved nil))
     (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) (setq asked prompt) ,answer))
               ((symbol-function 'customize-save-variable)
                (lambda (variable value &rest _) (setq saved (list variable value)))))
       ,@body)))

(defun canvas-browser-test--frame-goes-to (url)
  "Have chromium say that a frame of this page went to URL."
  (canvas-browser-test--event "Page.frameNavigated" (list :frame (list :id "F1" :url url))))

(ert-deftest canvas-browser-a-page-that-opens-is-given-nothing-to-call ()
  ;; GIVEN a page buffer that opens, with no site kept
  ;; THEN a script for notifications goes in before each page loads,
  ;;      which leaves a page alone unless it may show notifications,
  ;;      AND the page is given no name to call Emacs by, so that a site
  ;;      finds nothing put into its browser
  (canvas-browser-test--in-page
    (should (cl-some (lambda (script)
                       (and (string-search "canvasBrowserNotify" script)
                            (string-search "Notification.permission !== 'granted') return" script)))
                     (canvas-browser-test--scripts-before-load)))
    (should-not (member canvas-browser-test--notify-name canvas-browser-test--commands))))

(ert-deftest canvas-browser-a-frame-at-a-site-that-may-notify-is-given-the-name ()
  ;; GIVEN one site that may show notifications, and one that may not
  ;; WHEN a frame of the page goes to a site that nothing is kept about,
  ;;      and to the site that may not
  ;; THEN the page is given no name to call Emacs by
  ;; WHEN a frame goes to a page of the site that may
  ;; THEN the page is given the name, each time, since chromium keeps
  ;;      it only for the frames that are there
  (canvas-browser-test--in-page
    (let ((canvas-browser-notification-sites '(("https://mail.example" . t) ("https://no.example"))))
      (setq canvas-browser-test--commands nil)
      (canvas-browser-test--frame-goes-to "https://other.example/")
      (canvas-browser-test--frame-goes-to "https://no.example/inbox")
      (canvas-browser-test--frame-goes-to "about:blank")
      (should-not (member canvas-browser-test--notify-name canvas-browser-test--commands))
      (canvas-browser-test--frame-goes-to "https://mail.example/inbox?id=1")
      (canvas-browser-test--frame-goes-to "https://mail.example/sent")
      (should (= (cl-count canvas-browser-test--notify-name canvas-browser-test--commands
                           :test #'equal)
                 2)))))

(ert-deftest canvas-browser-a-notification-of-a-page-goes-to-the-hook ()
  ;; GIVEN a page with a title, AND a function on the hook of notifications
  ;; WHEN the page shows a notification
  ;; THEN the function gets the title and the body of the notification,
  ;;      the site, the address and the title of the page, and its buffer
  (canvas-browser-test--in-page
    (setq canvas-browser--title "Mail")
    (let* ((got nil)
           (canvas-browser-notification-functions
            (list (lambda (notification) (setq got notification)))))
      (canvas-browser-test--page-tells canvas-browser-test--new-mail)
      (should (equal (plist-get got :title) "New mail"))
      (should (equal (plist-get got :body) "From alice"))
      (should (equal (plist-get got :source) "example.org"))
      (should (equal (plist-get got :origin) "https://example.org"))
      (should (equal (plist-get got :url) "https://example.org"))
      (should (equal (plist-get got :page-title) "Mail"))
      (should (eq (plist-get got :buffer) (current-buffer))))))

(ert-deftest canvas-browser-a-notification-is-said-in-the-echo-area-as-it-comes ()
  ;; GIVEN the hook of notifications as the package comes
  ;; WHEN a page shows a notification
  ;; THEN the echo area names the site, the title and the body
  (canvas-browser-test--in-page
    (let ((canvas-browser-notification-functions
           (default-value 'canvas-browser-notification-functions))
          (said nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest args) (setq said (apply #'format format args)))))
        (canvas-browser-test--page-tells canvas-browser-test--new-mail))
      (should (equal said "example.org: New mail: From alice")))))

(ert-deftest canvas-browser-opening-a-notification-shows-its-page-and-clicks-it ()
  ;; GIVEN a notification that a page showed, from one of its frames
  ;; WHEN it is opened from another buffer
  ;; THEN the page buffer is shown, AND the page is told that the
  ;;      notification was clicked, in the frame that showed it
  (canvas-browser-test--in-page
    (let* ((page (current-buffer))
           (got nil)
           (shown nil)
           (canvas-browser-notification-functions
            (list (lambda (notification) (setq got notification)))))
      (canvas-browser-test--page-tells canvas-browser-test--new-mail 9)
      (setq canvas-browser-test--commands nil)
      (cl-letf (((symbol-function 'pop-to-buffer) (lambda (buffer &rest _) (setq shown buffer))))
        (with-temp-buffer
          (canvas-browser-notification-open got)))
      (should (eq shown page))
      (let ((params (canvas-browser-test--params "Runtime.evaluate")))
        (should (string-search "click(3)" (plist-get params :expression)))
        (should (equal (plist-get params :contextId) 9))))))

(ert-deftest canvas-browser-a-notification-of-a-page-that-is-gone-cannot-be-opened ()
  ;; GIVEN a notification whose page buffer was killed since
  ;; WHEN it is opened
  ;; THEN an error says that the page is gone
  (let ((got nil))
    (canvas-browser-test--in-page
      (let ((canvas-browser-notification-functions
             (list (lambda (notification) (setq got notification)))))
        (canvas-browser-test--page-tells canvas-browser-test--new-mail)))
    (should (string-search "is gone"
                           (cadr (should-error (canvas-browser-notification-open got)
                                               :type 'user-error))))))

(ert-deftest canvas-browser-a-site-that-is-allowed-shows-notifications-at-once ()
  ;; GIVEN a page of a site that nothing is kept about
  ;; WHEN the reader says in its buffer that the site may show
  ;;      notifications
  ;; THEN the question names the site, AND the answer is kept, AND
  ;;      chromium is told that the site may, AND the page that is there
  ;;      is given the name to call Emacs by and the script, so that it
  ;;      need not be loaded again
  (canvas-browser-test--in-page
    (let ((canvas-browser-notification-sites nil))
      (setq canvas-browser-test--commands nil)
      (canvas-browser-test--deciding t
        (canvas-browser-notifications)
        (should (string-search "https://example.org" asked))
        (should (equal saved '(canvas-browser-notification-sites
                               (("https://example.org" . t))))))
      (should (equal (canvas-browser-test--params "Browser.setPermission")
                     '(:permission (:name "notifications") :setting "granted"
                       :origin "https://example.org")))
      (should (member canvas-browser-test--notify-name canvas-browser-test--commands))
      (should (string-search "canvasBrowserNotify"
                             (plist-get (canvas-browser-test--params "Runtime.evaluate")
                                        :expression))))))

(defun canvas-browser-test--notify-entry ()
  "What the entry for notifications in the page menu reads as."
  (string-trim (funcall (plist-get (canvas-browser-test--menu-entry "N") :description))))

(ert-deftest canvas-browser-the-menu-says-whether-the-site-may-notify ()
  ;; GIVEN a page of a site that nothing is kept about
  ;; WHEN the entry for notifications in the menu is read
  ;; THEN it says off
  ;; GIVEN the site is kept as one that may show notifications
  ;; THEN it says on
  (canvas-browser-test--in-page
    (let ((canvas-browser-notification-sites nil))
      (should (string-match-p "\\`notify +off\\'" (canvas-browser-test--notify-entry))))
    (let ((canvas-browser-notification-sites '(("https://example.org" . t))))
      (should (string-match-p "\\`notify +on\\'" (canvas-browser-test--notify-entry))))))

(ert-deftest canvas-browser-the-menu-entry-lets-the-site-notify-and-stops-it ()
  ;; GIVEN a page of a site that nothing is kept about
  ;; WHEN the entry for notifications in the menu is taken
  ;; THEN the site is kept as one that may, with no question, AND chromium
  ;;      is told that it may, AND the page is given the script
  ;; WHEN the entry is taken again
  ;; THEN the site is kept as one that may not, AND chromium is told so
  (canvas-browser-test--in-page
    (let ((canvas-browser-notification-sites nil)
          (command (plist-get (canvas-browser-test--menu-entry "N") :command)))
      (setq canvas-browser-test--commands nil)
      (canvas-browser-test--deciding (error "The menu entry asks no question")
        (funcall command)
        (should-not asked)
        (should (equal saved '(canvas-browser-notification-sites
                               (("https://example.org" . t)))))
        (should (equal (plist-get (canvas-browser-test--params "Browser.setPermission") :setting)
                       "granted"))
        (should (string-search "canvasBrowserNotify"
                               (plist-get (canvas-browser-test--params "Runtime.evaluate")
                                          :expression)))
        (setq canvas-browser-test--commands nil)
        (funcall command)
        (should (equal saved '(canvas-browser-notification-sites (("https://example.org")))))
        (should (equal (plist-get (canvas-browser-test--params "Browser.setPermission") :setting)
                       "denied"))))))

(ert-deftest canvas-browser-the-menu-entry-needs-a-page ()
  ;; GIVEN a page buffer that holds no page yet
  ;; WHEN the entry for notifications in the menu is taken
  ;; THEN it is an error that says so, AND nothing is kept
  (with-temp-buffer
    (canvas-browser-mode)
    (let ((canvas-browser-notification-sites nil))
      (canvas-browser-test--deciding t
        (should-error (canvas-browser-toggle-notifications) :type 'user-error)
        (should-not saved)))))

(ert-deftest canvas-browser-a-site-that-is-refused-gets-nothing ()
  ;; GIVEN a page of a site that may show notifications
  ;; WHEN the reader says in its buffer that the site may not
  ;; THEN the answer is kept, AND chromium is told that the site may
  ;;      not, AND the page is given nothing
  (canvas-browser-test--in-page
    (let ((canvas-browser-notification-sites (list (cons "https://example.org" t))))
      (setq canvas-browser-test--commands nil)
      (canvas-browser-test--deciding nil
        (canvas-browser-notifications)
        (should (equal saved '(canvas-browser-notification-sites (("https://example.org"))))))
      (should (equal (plist-get (canvas-browser-test--params "Browser.setPermission") :setting)
                     "denied"))
      (should-not (member canvas-browser-test--notify-name canvas-browser-test--commands))
      (should-not (canvas-browser-test--params "Runtime.evaluate")))))

(ert-deftest canvas-browser-the-site-of-a-page-is-named-as-the-page-names-it ()
  ;; GIVEN a page at an address with a port and a path
  ;; WHEN the reader says that its site may show notifications
  ;; THEN the site is kept with its port and without the path, as a page
  ;;      names its own site
  (canvas-browser-test--in-page
    (setq canvas-browser--url "http://localhost:8080/mail/inbox?id=1")
    (let ((canvas-browser-notification-sites nil))
      (canvas-browser-test--deciding t
        (canvas-browser-notifications)
        (should (equal saved '(canvas-browser-notification-sites
                               (("http://localhost:8080" . t)))))))))

(ert-deftest canvas-browser-chromium-is-told-of-the-sites-that-are-kept ()
  ;; GIVEN two sites that are kept, one that may show notifications and
  ;;       one that may not
  ;; WHEN a page buffer opens
  ;; THEN chromium is told of both, so that a page of either finds its
  ;;      answer there
  (let ((canvas-browser-notification-sites '(("https://a.example" . t) ("https://b.example"))))
    (canvas-browser-test--in-page
      (should (member '("Browser.setPermission" :permission (:name "notifications")
                        :setting "granted" :origin "https://a.example")
                      canvas-browser-test--commands))
      (should (member '("Browser.setPermission" :permission (:name "notifications")
                        :setting "denied" :origin "https://b.example")
                      canvas-browser-test--commands)))))

;;;; Emacs as the notification service of the session

;; The functions of the bus are stubbed below.  They are loaded first:
;; a file that loads while its functions are stubbed puts the real ones
;; in their place, and a test would then reach the bus of the session.
(require 'dbus nil t)

(defmacro canvas-browser-test--with-bus (owner &rest body)
  "Run BODY with the session bus stubbed, where OWNER is its notification service.
OWNER is nil when no program is.  What Emacs registers goes to
`registered': (SERVICE) for the service, and (METHOD . HANDLER) for
each method.  The signals that Emacs sends go to `signals', each
\(TO NAME ARGUMENTS)."
  (declare (indent 1))
  `(let ((registered nil)
         (signals nil)
         (canvas-browser--dbus-served nil)
         (canvas-browser--dbus-count 0))
     (skip-unless (featurep 'dbus))
     (cl-letf (((symbol-function 'dbus-get-name-owner) (lambda (&rest _) ,owner))
               ((symbol-function 'dbus-register-service)
                (lambda (_bus service &rest _) (push (list service) registered) :primary-owner))
               ((symbol-function 'dbus-register-method)
                (lambda (_bus _service _path _interface method handler &rest _)
                  (push (cons method handler) registered)))
               ((symbol-function 'dbus-send-signal)
                (lambda (_bus to _path _interface name &rest arguments)
                  (push (list to name arguments) signals))))
       ,@body)))

(defmacro canvas-browser-test--serving (&rest body)
  "Run BODY where Emacs is the notification service, in a page at example.org.
`notify' is the function that takes a notification from the bus, and
`got' the notifications that the hook was given, newest first."
  (declare (indent 0))
  `(canvas-browser-test--with-bus nil
     (let* ((got nil)
            (canvas-browser-notification-service t)
            (canvas-browser-notification-functions
             (list (lambda (notification) (push notification got)))))
       (canvas-browser-test--in-page
         (let ((notify (cdr (assoc "Notify" registered))))
           ,@body)))))

(ert-deftest canvas-browser-is-the-notification-service-when-asked-and-none-is-there ()
  ;; GIVEN the option to be the notification service, AND a session bus
  ;;       where no program is that service
  ;; WHEN a page buffer opens, which starts chromium
  ;; THEN Emacs is the service, with the four methods that a program
  ;;      calls on it, so that chromium finds it when it starts
  ;; WHEN a second page opens
  ;; THEN nothing is registered again
  (canvas-browser-test--with-bus nil
    (let ((canvas-browser-notification-service t))
      (canvas-browser-test--in-page
        (should (member '("org.freedesktop.Notifications") registered))
        (should (equal (sort (delq nil (mapcar (lambda (entry) (and (cdr entry) (car entry)))
                                               registered))
                             #'string<)
                       '("CloseNotification" "GetCapabilities" "GetServerInformation" "Notify")))
        (let ((count (length registered)))
          (canvas-browser--serve-notifications)
          (should (= count (length registered))))))))

(ert-deftest canvas-browser-leaves-the-notification-service-to-a-program-that-is-it ()
  ;; GIVEN the option to be the notification service, AND a desktop
  ;;       whose own program is that service already
  ;; WHEN a page buffer opens
  ;; THEN Emacs registers nothing: the desktop shows the notifications
  (canvas-browser-test--with-bus ":1.42"
    (let ((canvas-browser-notification-service t))
      (canvas-browser-test--in-page
        (should-not registered)))))

(ert-deftest canvas-browser-is-no-notification-service-unless-asked ()
  ;; GIVEN the option as it comes, AND no notification service on the bus
  ;; WHEN a page buffer opens
  ;; THEN Emacs registers nothing: it takes that place in a session only
  ;;      when the reader said so
  (canvas-browser-test--with-bus nil
    (let ((canvas-browser-notification-service nil))
      (canvas-browser-test--in-page
        (should-not registered)))))

(ert-deftest canvas-browser-a-notification-from-chromium-goes-to-the-hook-with-its-page ()
  ;; GIVEN Emacs as the notification service, AND a page of example.org
  ;; WHEN chromium shows a notification of that site through the bus, as
  ;;      it does for one that a service worker shows: the text starts
  ;;      with the site, and the words follow after an empty line
  ;; THEN the hook gets the title, the words without the site, the site
  ;;      as where it comes from, and the page buffer of that site, AND
  ;;      chromium is answered with an id, a new one for each
  ;; WHEN chromium shows one in place of the first
  ;; THEN it is answered with the id of the first
  (canvas-browser-test--serving
    (should (equal (funcall notify "Chromium" 0 "" "New mail" "example.org\n\nFrom alice"
                            '("default" "Activate") nil -1)
                   '(:uint32 1)))
    (let ((notification (car got)))
      (should (equal (plist-get notification :title) "New mail"))
      (should (equal (plist-get notification :body) "From alice"))
      (should (equal (plist-get notification :source) "example.org"))
      (should (equal (plist-get notification :origin) "https://example.org"))
      (should (eq (plist-get notification :buffer) (current-buffer))))
    (should (equal (funcall notify "Chromium" 0 "" "More mail" "example.org\n\nFrom bob" nil nil -1)
                   '(:uint32 2)))
    (should (equal (funcall notify "Chromium" 1 "" "New mail" "example.org\n\nFrom alice, again"
                            nil nil -1)
                   '(:uint32 1)))))

(ert-deftest canvas-browser-a-notification-of-a-site-without-a-page-names-the-site ()
  ;; GIVEN Emacs as the notification service, AND no page of the site
  ;;       mail.example open
  ;; WHEN chromium shows a notification of that site, as a service
  ;;      worker does after its page was closed
  ;; THEN the hook gets it with the site as where it comes from, AND
  ;;      with no page buffer
  ;; WHEN it is opened
  ;; THEN the site is opened in a page buffer, since no page of it is
  ;;      there to show, AND the bus is told of the click
  (canvas-browser-test--serving
    (funcall notify "Chromium" 0 "" "New mail" "mail.example\n\nFrom alice" nil nil -1)
    (should (equal (plist-get (car got) :source) "mail.example"))
    (should (equal (plist-get (car got) :body) "From alice"))
    (should-not (plist-get (car got) :buffer))
    (let ((opened nil) (later nil))
      (cl-letf (((symbol-function 'canvas-browser) (lambda (url &rest _) (setq opened url)))
                ((symbol-function 'run-at-time)
                 (lambda (_seconds _repeat function &rest args)
                   (setq later (lambda () (apply function args))))))
        (canvas-browser-notification-open (car got))
        ;; The site hears of the click first, and may open a page itself.
        (should (equal (car signals) '(nil "ActionInvoked" (:uint32 1 "default"))))
        (should-not opened)
        (funcall later))
      (should (equal opened "https://mail.example")))))

(ert-deftest canvas-browser-a-site-that-opens-its-own-page-on-a-click-is-not-opened-twice ()
  ;; GIVEN a notification of a site with no page open, which is opened
  ;; WHEN the site has opened a page itself by the time canvas-browser
  ;;      looks, as a mail site opens the mail
  ;; THEN that page is shown, AND the site is not opened a second time
  (canvas-browser-test--serving
    (funcall notify "Chromium" 0 "" "New mail" "mail.example\n\nFrom alice" nil nil -1)
    (let ((opened nil) (later nil) (shown nil))
      (cl-letf (((symbol-function 'canvas-browser) (lambda (url &rest _) (setq opened url)))
                ((symbol-function 'pop-to-buffer) (lambda (buffer &rest _) (setq shown buffer)))
                ((symbol-function 'run-at-time)
                 ;; The page buffer below starts a timer of its own, so
                 ;; only the first one is the wait of the click.
                 (lambda (_seconds _repeat function &rest args)
                   (unless later
                     (setq later (lambda () (apply function args)))))))
        (canvas-browser-notification-open (car got))
        (with-temp-buffer
          (canvas-browser-mode)
          (setq canvas-browser--url "https://mail.example/mail/1")
          (funcall later)
          (should (eq shown (current-buffer)))))
      (should-not opened))))

(ert-deftest canvas-browser-a-notification-of-another-program-names-the-program ()
  ;; GIVEN Emacs as the notification service of the session
  ;; WHEN a program that is no browser shows a notification
  ;; THEN the hook gets it with the name of the program as where it
  ;;      comes from, AND with its words as they are, AND with no page
  ;; WHEN it is opened
  ;; THEN no page is opened: the program is no site
  (canvas-browser-test--serving
    (funcall notify "backup" 0 "" "Done" "All files are\n\ncopied" nil nil -1)
    (should (equal (plist-get (car got) :source) "backup"))
    (should (equal (plist-get (car got) :title) "Done"))
    (should (equal (plist-get (car got) :body) "All files are\n\ncopied"))
    (should-not (plist-get (car got) :buffer))
    (cl-letf (((symbol-function 'canvas-browser) (lambda (&rest _) (error "No page is to open"))))
      (canvas-browser-notification-open (car got)))))

(ert-deftest canvas-browser-opening-a-notification-of-the-bus-clicks-it-there ()
  ;; GIVEN a notification that chromium showed through the bus, for a
  ;;       site whose page is open
  ;; WHEN it is opened from another buffer
  ;; THEN the page buffer is shown, AND every program on the bus is told
  ;;      that the notification was clicked, which chromium hears and
  ;;      hands to the service worker
  (canvas-browser-test--serving
    (funcall notify "Chromium" 0 "" "New mail" "example.org\n\nFrom alice" nil nil -1)
    (let ((page (current-buffer))
          (shown nil))
      (cl-letf (((symbol-function 'pop-to-buffer) (lambda (buffer &rest _) (setq shown buffer))))
        (with-temp-buffer
          (canvas-browser-notification-open (car got))))
      (should (eq shown page))
      (should (equal (car signals) '(nil "ActionInvoked" (:uint32 1 "default")))))))

(ert-deftest canvas-browser-the-notification-service-answers-what-it-can-do ()
  ;; GIVEN Emacs as the notification service
  ;; WHEN a program asks what the service can do, and who it is
  ;; THEN it says that it takes a text and actions, which is what
  ;;      chromium needs to hand over a click, AND names itself
  ;; WHEN a program takes a notification back
  ;; THEN the programs on the bus are told that it is closed
  (canvas-browser-test--serving
    (should (equal (funcall (cdr (assoc "GetCapabilities" registered))) '(("body" "actions"))))
    (should (equal (car (funcall (cdr (assoc "GetServerInformation" registered))))
                   "canvas-browser"))
    (funcall (cdr (assoc "CloseNotification" registered)) 4)
    (should (equal (car signals) '(nil "NotificationClosed" (:uint32 4 :uint32 3))))))

