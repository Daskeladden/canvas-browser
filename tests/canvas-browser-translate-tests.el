;;; canvas-browser-translate-tests.el --- tests of the translators -*- lexical-binding: t -*-
(require 'ert)
(require 'cl-lib)
(require 'canvas-browser-translate)

(defconst canvas-browser-test--thread-html
  (concat
   "<main>"
   "<x-post title=\"A post about a canvas\" by=\"dana\" at=\"2026-10-01T17:11:23.133000+0000\">"
   "<span slot=\"button\"><button>Share</button></span>"
   "<div slot=\"text\"><p>The first paragraph.</p><p>The second one.</p></div>"
   "</x-post>"
   "<x-comment by=\"alice\" at=\"2026-10-02T13:03:45.915000+0000\">"
   "<details><summary><div slot=\"meta\">alice, 8h ago</div></summary>"
   "<div><div slot=\"comment\"><p>What is the role?</p></div>"
   "<div slot=\"actions\"><button>Reply</button><span>7</span></div>"
   "<div id=\"children\">"
   "<x-comment by=\"bob\" at=\"2026-10-02T14:10:00.000000+0000\">"
   "<details><div slot=\"comment\"><p>The drawing layer.</p><p>It uses cairo.</p>"
   "<script>var a = 1;</script><style>p { }</style></div>"
   "<div id=\"children\">"
   "<x-comment by=\"alice\" at=\"2026-10-02T16:00:00.000000+0000\">"
   "<details><div slot=\"comment\"><p>I   see.</p></div></details></x-comment>"
   "</div></details></x-comment>"
   "</div></div></details></x-comment>"
   "<x-comment by=\"carol\" at=\"2026-10-02T15:00:00.000000+0000\">"
   "<details><div slot=\"comment\"><p>Nice.</p></div></details></x-comment>"
   "</main><aside>Words of the sidebar</aside>")
  "The HTML of a page with a thread: a post, and comments that hold their
replies inside themselves, each with buttons and a count beside its text.")

(defconst canvas-browser-test--thread-items
  '((x-post :title (attr title) :author (attr by) :time (attr at) :body (inside slot "text"))
    (x-comment :author (attr by) :time (attr at) :body (inside slot "comment")))
  "What a post and a comment of the thread fixture are, for a translator.")

(defmacro canvas-browser-test--in-utc (&rest body)
  "Run BODY with the clock of Emacs at UTC, so that a time reads the same anywhere."
  (declare (indent 0))
  `(let ((process-environment (cons "TZ=UTC" process-environment)))
     (unwind-protect
         (progn (set-time-zone-rule "UTC") ,@body)
       (set-time-zone-rule nil))))

(ert-deftest canvas-browser-a-thread-translator-writes-who-said-what-under-whom ()
  ;; GIVEN the HTML of a thread, and a translator that is told which
  ;;       elements are a post and a comment, and where each keeps its
  ;;       title, its author, its time and its text
  ;; WHEN the HTML is translated
  ;; THEN the text names the post with its title, then each comment with
  ;;      its author and its time, a reply two spaces further in than
  ;;      what it answers, AND the paragraphs of a text stand apart, AND
  ;;      neither the buttons, the counts, the scripts nor the sidebar
  ;;      are in it
  (skip-unless (libxml-available-p))
  (canvas-browser-test--in-utc
    (let ((canvas-browser-translators
           (list (cons "example\\.org/thread"
                       (apply #'canvas-browser-thread-translator
                              canvas-browser-test--thread-items)))))
      (should (equal (plist-get (canvas-browser-translate "https://example.org/thread/1"
                                                          canvas-browser-test--thread-html)
                                :text)
                     (string-join
                      '("A post about a canvas"
                        "dana · 2026-10-01 17:11"
                        ""
                        "The first paragraph."
                        ""
                        "The second one."
                        ""
                        "alice · 2026-10-02 13:03"
                        ""
                        "What is the role?"
                        ""
                        "  bob · 2026-10-02 14:10"
                        ""
                        "  The drawing layer."
                        ""
                        "  It uses cairo."
                        ""
                        "    alice · 2026-10-02 16:00"
                        ""
                        "    I see."
                        ""
                        "carol · 2026-10-02 15:00"
                        ""
                        "Nice.")
                      "\n"))))))

(defun canvas-browser-test--thread ()
  "The thread of the fixture, as the translator gives it as data."
  (plist-get (let ((canvas-browser-translators
                    (list (cons "example\\.org"
                                (apply #'canvas-browser-thread-translator
                                       canvas-browser-test--thread-items)))))
               (canvas-browser-translate "https://example.org/thread/1"
                                         canvas-browser-test--thread-html))
             :thread))

(ert-deftest canvas-browser-a-thread-translator-gives-the-thread-as-data ()
  ;; GIVEN the HTML of a thread, and a translator that knows its items
  ;; WHEN the HTML is translated
  ;; THEN the translation holds the thread as Lisp data beside the text:
  ;;      a list of items, each with its title, author, time and text,
  ;;      and with the items inside it as its replies, AND a time is a
  ;;      time of Emacs
  (skip-unless (libxml-available-p))
  (let ((thread (canvas-browser-test--thread)))
    (should (equal (mapcar (lambda (item) (plist-get item :author)) thread)
                   '("dana" "alice" "carol")))
    (should (equal (plist-get (nth 0 thread) :title) "A post about a canvas"))
    (should (equal (plist-get (nth 0 thread) :text) "The first paragraph.\n\nThe second one."))
    (let* ((alice (nth 1 thread))
           (bob (car (plist-get alice :replies)))
           (again (car (plist-get bob :replies))))
      (should (equal (plist-get alice :text) "What is the role?"))
      (should (time-equal-p (plist-get alice :time)
                            (encode-time (iso8601-parse "2026-10-02T13:03:45.915000+0000"))))
      (should (equal (plist-get bob :author) "bob"))
      (should (equal (plist-get again :text) "I see."))
      (should-not (plist-get again :replies)))
    (should-not (plist-get (nth 2 thread) :replies))))

(ert-deftest canvas-browser-a-thread-is-written-as-an-org-list ()
  ;; GIVEN the thread of the fixture, as data
  ;; WHEN it is written for org
  ;; THEN each item is an item of a list with its title and its author
  ;;      in bold and its time as a time of org, and its text under it,
  ;;      AND a reply is an item of the list inside the item it answers,
  ;;      AND an empty line stands between two items, so that each reads
  ;;      on its own
  (skip-unless (libxml-available-p))
  (canvas-browser-test--in-utc
    (should (equal (canvas-browser-thread-org (canvas-browser-test--thread))
                   (string-join
                    '("- *A post about a canvas* · *dana* · [2026-10-01 Thu 17:11]"
                      ""
                      "  The first paragraph."
                      ""
                      "  The second one."
                      ""
                      "- *alice* · [2026-10-02 Fri 13:03]"
                      ""
                      "  What is the role?"
                      ""
                      "  - *bob* · [2026-10-02 Fri 14:10]"
                      ""
                      "    The drawing layer."
                      ""
                      "    It uses cairo."
                      ""
                      "    - *alice* · [2026-10-02 Fri 16:00]"
                      ""
                      "      I see."
                      ""
                      "- *carol* · [2026-10-02 Fri 15:00]"
                      ""
                      "  Nice.")
                    "\n")))))

(defconst canvas-browser-test--chat-html
  (concat
   "<div data-kind=\"list\">"
   "<div data-kind=\"item\"><div role=\"heading\">Hello there... by Ann</div>"
   "<span data-kind=\"name\">Ann</span><time datetime=\"2026-09-24T12:00:44.279Z\">2:00 PM</time>"
   "<div data-kind=\"item\"><span data-kind=\"avatar\"></span></div>"
   "<div data-kind=\"message\"><p>Hello there.</p><p>Second line.</p></div></div>"
   "<div data-kind=\"item\"><div>Yesterday</div></div>"
   "<div data-kind=\"item\"><span data-kind=\"name\">Ben</span>"
   "<time datetime=\"2026-09-24T12:01:24.510Z\">2:01 PM</time>"
   "<div data-kind=\"message\"><p>Hi.</p></div></div>"
   "</div>")
  "The HTML of a chat: items marked by an attribute and not by a tag, each
with a summary, a name, a time element and a message, an empty item for
a picture inside the first, and an item that is only a day.")

(ert-deftest canvas-browser-items-can-be-found-by-an-attribute-and-read-inside ()
  ;; GIVEN the HTML of a chat whose items are marked by an attribute, with
  ;;       the time in an attribute of an element inside each item
  ;; WHEN it is translated by a thread translator whose item is named by
  ;;      that attribute, with the time read from inside
  ;; THEN each message is an item with its author, its time and its
  ;;      text, AND an item that holds no author, no title and no text,
  ;;      as the one of a picture, is no item, AND the summary and the
  ;;      day are not in it
  (skip-unless (libxml-available-p))
  (canvas-browser-test--in-utc
    (let* ((canvas-browser-translators
            (list (cons "example\\.org"
                        (canvas-browser-thread-translator
                         '((data-kind "item")
                           :author (inside data-kind "name")
                           :time (tag time datetime)
                           :body (inside data-kind "message"))))))
           (translation (canvas-browser-translate "https://example.org/chat"
                                                  canvas-browser-test--chat-html)))
      (should (equal (mapcar (lambda (item) (plist-get item :author))
                             (plist-get translation :thread))
                     '("Ann" "Ben")))
      (should (equal (plist-get translation :text)
                     (string-join '("Ann · 2026-09-24 12:00" "" "Hello there." "" "Second line."
                                    "" "Ben · 2026-09-24 12:01" "" "Hi.")
                                  "\n"))))))

(ert-deftest canvas-browser-an-item-can-be-read-by-a-pattern-of-an-attribute ()
  ;; GIVEN a message whose text lies in an element whose id starts with
  ;;       content-, beside the reactions to it
  ;; WHEN the message is translated with its text read by that pattern
  ;; THEN the text is the message alone, without the reactions
  (skip-unless (libxml-available-p))
  (let ((canvas-browser-translators
         (list (cons "example\\.org"
                     (canvas-browser-thread-translator
                      '((data-kind "item") :author (inside data-kind "name")
                        :body (matching id "\\`content-")))))))
    (should (equal (plist-get (canvas-browser-translate
                               "https://example.org/chat"
                               (concat "<div data-kind=\"item\"><span data-kind=\"name\">Ann</span>"
                                       "<div><div id=\"content-17\"><p>Hello.</p></div>"
                                       "<div>1 Like reaction.</div></div></div>"))
                              :text)
                   "Ann\n\nHello."))))

(defconst canvas-browser-test--linked-html
  (concat "<div data-kind=\"item\" data-link=\"https://example.org/chat/m/17\">"
          "<span data-kind=\"name\">Ann</span><time datetime=\"2026-09-24T12:00:00Z\"></time>"
          "<div data-kind=\"message\"><p>Hello.</p></div></div>"
          "<div data-kind=\"item\"><span data-kind=\"name\">Ben</span>"
          "<time datetime=\"2026-09-24T12:01:00Z\"></time>"
          "<div data-kind=\"message\"><p>Hi.</p></div></div>")
  "The HTML of a chat of two messages.  The first one has an address.")

(defun canvas-browser-test--linked-thread ()
  "The thread of `canvas-browser-test--linked-html', its addresses read."
  (let ((canvas-browser-translators
         (list (cons "example\\.org"
                     (canvas-browser-thread-translator
                      '((data-kind "item")
                        :author (inside data-kind "name")
                        :time (tag time datetime)
                        :body (inside data-kind "message")
                        :link (attr data-link)))))))
    (plist-get (canvas-browser-translate "https://example.org/chat"
                                         canvas-browser-test--linked-html)
               :thread)))

(ert-deftest canvas-browser-an-item-can-have-an-address-of-its-own ()
  ;; GIVEN a chat whose first message has an address of its own, and
  ;;       whose second has none
  ;; WHEN it is translated by a thread translator that reads the address
  ;; THEN the first item has the address, AND the second has none
  (skip-unless (libxml-available-p))
  (let ((thread (canvas-browser-test--linked-thread)))
    (should (equal (plist-get (nth 0 thread) :link) "https://example.org/chat/m/17"))
    (should-not (plist-get (nth 1 thread) :link))))

(ert-deftest canvas-browser-an-item-with-an-address-links-to-it-in-org ()
  ;; GIVEN that thread
  ;; WHEN it is written for org
  ;; THEN the head of the first item ends with a link to its address,
  ;;      AND the head of the second is as it always was
  (skip-unless (libxml-available-p))
  (canvas-browser-test--in-utc
    (should (equal (canvas-browser-thread-org (canvas-browser-test--linked-thread))
                   (string-join
                    '("- *Ann* · [2026-09-24 Thu 12:00] · [[https://example.org/chat/m/17][link]]"
                      ""
                      "  Hello."
                      ""
                      "- *Ben* · [2026-09-24 Thu 12:01]"
                      ""
                      "  Hi.")
                    "\n")))))

(ert-deftest canvas-browser-the-text-to-read-names-no-address ()
  ;; GIVEN that thread
  ;; WHEN it is written as text to read
  ;; THEN no address stands in it: an address is to follow, not to read
  (skip-unless (libxml-available-p))
  (canvas-browser-test--in-utc
    (should-not (string-search "https://" (canvas-browser-thread-text
                                           (canvas-browser-test--linked-thread))))))

(ert-deftest canvas-browser-the-head-of-an-item-is-the-same-with-an-address ()
  ;; GIVEN an item, and the same item with an address
  ;; THEN both have one head: a note knows a message by its head, and a
  ;;      message is the same one when the page gives its address later
  (let ((item '(:author "Ann" :time "noon" :text "Hello.")))
    (should (equal (canvas-browser-org-head item)
                   (canvas-browser-org-head
                    (append item '(:link "https://example.org/chat/m/17")))))))

(ert-deftest canvas-browser-what-stands-for-a-picture-says-so ()
  ;; GIVEN a line of HTML with three pictures among its words: one with
  ;;       a description, one whose description is an emoji, and one
  ;;       with neither
  ;; WHEN its text is made
  ;; THEN each stretch that stands for a picture says so, the emoji that
  ;;      stands as itself too, AND the words of the page do not
  (skip-unless (libxml-available-p))
  (let* ((text (canvas-browser-dom-text
                (canvas-browser--parse-html
                 (concat "<p><img alt=\"point\" src=\"a.png\"> the words "
                         "<img alt=\"🙁\" src=\"b.png\"> more <img src=\"c.png\"></p>"))))
         (for-picture (lambda (word)
                        (get-text-property (string-search word text)
                                           'canvas-browser-for-picture text))))
    (should (equal (substring-no-properties text) "[point] the words 🙁 more [image]"))
    (should (funcall for-picture "[point]"))
    (should (funcall for-picture "🙁"))
    (should (funcall for-picture "[image]"))
    (should-not (funcall for-picture "the words"))
    (should-not (funcall for-picture "more"))))

(ert-deftest canvas-browser-an-address-without-a-translator-is-not-translated ()
  ;; GIVEN a translator for one address
  ;; WHEN the HTML of a page at another address is translated
  ;; THEN there is no translation, so the text goes as the page shows it
  (let ((canvas-browser-translators
         (list (cons "example\\.org/thread" (lambda (_dom) "translated")))))
    (should-not (canvas-browser-translate "https://example.org/other"
                                          canvas-browser-test--thread-html))))

(ert-deftest canvas-browser-a-translator-that-finds-nothing-gives-no-translation ()
  ;; GIVEN a thread translator, and the HTML of a part of the page that
  ;;       holds no post and no comment, as a sidebar
  ;; WHEN that HTML is translated
  ;; THEN there is no translation, so the text goes as the page shows it
  (skip-unless (libxml-available-p))
  (let ((canvas-browser-translators
         (list (cons "example\\.org" (apply #'canvas-browser-thread-translator
                                            canvas-browser-test--thread-items)))))
    (should-not (canvas-browser-translate "https://example.org/thread/1"
                                          "<aside>Words of the sidebar</aside>"))))

(ert-deftest canvas-browser-a-translator-can-be-any-function-of-the-page ()
  ;; GIVEN a translator that is a plain function
  ;; WHEN the HTML of a page at its address is translated
  ;; THEN the function gets the page as parsed HTML, AND the text it
  ;;      gives is the translation, with no data beside it
  (skip-unless (libxml-available-p))
  (let ((canvas-browser-translators
         (list (cons "example\\.org"
                     (lambda (dom) (format "%d comments" (length (dom-by-tag dom 'x-comment))))))))
    (should (equal (canvas-browser-translate "https://example.org/thread/1"
                                             canvas-browser-test--thread-html)
                   '(:text "4 comments")))))

(ert-deftest canvas-browser-a-translator-that-fails-says-which-one ()
  ;; GIVEN a translator whose function signals an error
  ;; WHEN the HTML of a page at its address is translated
  ;; THEN an error names the pattern of that translator and what went
  ;;      wrong, so that who wrote it finds it
  (skip-unless (libxml-available-p))
  (let ((canvas-browser-translators
         (list (cons "example\\.org" (lambda (_dom) (error "No such element"))))))
    (let ((message (cadr (should-error (canvas-browser-translate
                                        "https://example.org/a" "<p>x</p>")))))
      (should (string-search "example\\.org" message))
      (should (string-search "No such element" message)))))

(ert-deftest canvas-browser-adding-a-translator-replaces-the-one-of-its-pattern ()
  ;; GIVEN a translator for a pattern
  ;; WHEN another translator is added for the same pattern, as when a
  ;;      snippet is evaluated again after a change
  ;; THEN the pattern has one translator, the new one
  (let ((canvas-browser-translators nil))
    (canvas-browser-add-translator "example\\.org" #'ignore)
    (canvas-browser-add-translator "example\\.org" #'identity)
    (should (equal canvas-browser-translators '(("example\\.org" . identity))))))

(ert-deftest canvas-browser-the-text-of-html-keeps-lines-and-drops-what-is-no-text ()
  ;; GIVEN parsed HTML with paragraphs, a line break, a list, runs of
  ;;       white space, a script and a style
  ;; WHEN its text is read
  ;; THEN a paragraph stands apart from the next, a list item and a
  ;;      broken line have a line of their own, white space is one
  ;;      space, AND the script and the style are not in it
  (skip-unless (libxml-available-p))
  (with-temp-buffer
    (insert "<div><p>One   <b>bold</b>\n word.</p><p>Two<br>lines.</p>"
            "<ul><li>first</li><li>second</li></ul>"
            "<script>var a;</script><style>p { }</style></div>")
    (should (equal (canvas-browser-dom-text
                    (libxml-parse-html-region (point-min) (point-max)))
                   "One bold word.\n\nTwo\nlines.\n\nfirst\nsecond"))))

(ert-deftest canvas-browser-a-non-breaking-space-is-white-space-in-the-text ()
  ;; GIVEN parsed HTML with a paragraph that holds only a non-breaking
  ;;       space, as a chat writes an empty line, and a line that starts
  ;;       with one
  ;; WHEN its text is read
  ;; THEN the empty paragraph leaves no line of spaces, AND the line
  ;;      starts with its first word
  (skip-unless (libxml-available-p))
  (with-temp-buffer
    (insert "<div><p>One.</p><p>&nbsp;</p><p>&nbsp;Two&nbsp; words.</p></div>")
    (should (equal (canvas-browser-dom-text
                    (libxml-parse-html-region (point-min) (point-max)))
                   "One.\n\nTwo words."))))

(ert-deftest canvas-browser-a-picture-is-in-the-text-by-its-description ()
  ;; GIVEN parsed HTML with an emoji drawn as a picture, a picture with a
  ;;       description, one with a label alone, one with no description
  ;;       at all, and one that says it is only for the look
  ;; WHEN its text is read
  ;; THEN the emoji stands as the emoji, a description and a label stand
  ;;      in brackets, a picture with none stands as [image], AND the
  ;;      picture for the look is not there
  (skip-unless (libxml-available-p))
  (with-temp-buffer
    (insert "<p>Thanks <img alt=\"🙂\" src=\"a\"> and <img alt=\"A plot\" src=\"b\">"
            " and <img aria-label=\"image\" src=\"c\"> and <img src=\"d\">"
            " and <img alt=\"\" role=\"presentation\" src=\"e\">.</p>")
    (should (equal (canvas-browser-dom-text
                    (libxml-parse-html-region (point-min) (point-max)))
                   "Thanks 🙂 and [A plot] and [image] and [image] and ."))))

(ert-deftest canvas-browser-a-picture-in-the-text-keeps-its-address ()
  ;; GIVEN parsed HTML with a picture that has an address, and an emoji
  ;; WHEN its text is read
  ;; THEN the brackets that stand for the picture carry its address, so
  ;;      that a receiver can fetch the picture, AND the emoji carries none
  (skip-unless (libxml-available-p))
  (with-temp-buffer
    ;; Words between the pictures: libxml drops white space alone there.
    (insert "<p>See <img alt=\"A plot\" src=\"plot.png\"> and <img alt=\"🙂\" src=\"smile.png\"></p>")
    (let* ((text (canvas-browser-dom-text (libxml-parse-html-region (point-min) (point-max))))
           (at (string-search "[A plot]" text)))
      (should (equal text "See [A plot] and 🙂"))
      (should (equal (get-text-property at 'canvas-browser-picture text) "plot.png"))
      (should (equal (get-text-property (+ at 7) 'canvas-browser-picture text) "plot.png"))
      (should-not (get-text-property (string-search "🙂" text) 'canvas-browser-picture text)))))

(ert-deftest canvas-browser-the-text-of-html-leaves-out-what-the-html-hides ()
  ;; GIVEN parsed HTML where one element has the attribute hidden, and
  ;;       one says in its own style that it is not shown
  ;; WHEN its text is read
  ;; THEN neither of the two is in it
  (skip-unless (libxml-available-p))
  (with-temp-buffer
    ;; Chromium writes an attribute without a value as hidden="".
    (insert "<div><p>Shown.</p><p hidden=\"\">By the attribute.</p>"
            "<p style=\"color: red; display:none\">By its style.</p>"
            "<p style=\"display: none\">By its style too.</p></div>")
    (should (equal (canvas-browser-dom-text
                    (libxml-parse-html-region (point-min) (point-max)))
                   "Shown."))))

(ert-deftest canvas-browser-a-full-date-in-words-is-a-time-too ()
  ;; GIVEN a thread whose message gives its time as a day with its year,
  ;;       its month and a time of day, and one that gives no year
  ;; WHEN the HTML is translated
  ;; THEN the first is a time of Emacs, read as a time here, AND the
  ;;      second stays the string that the page gave
  (skip-unless (libxml-available-p))
  (canvas-browser-test--in-utc
    (let* ((canvas-browser-translators
            (list (cons "example\\.org"
                        (canvas-browser-thread-translator
                         '(x-comment :author (attr by) :time (attr at)
                                     :body (inside slot "comment"))))))
           (thread (plist-get
                    (canvas-browser-translate
                     "https://example.org/a"
                     (concat "<x-comment by=\"alice\" at=\"Wed 2026-09-30 17:32\"><div slot=\"comment\">Hi.</div></x-comment>"
                             "<x-comment by=\"bob\" at=\"9/23 7:57 PM\"><div slot=\"comment\">Ho.</div></x-comment>"))
                    :thread)))
      (should (equal (format-time-string "%F %R" (plist-get (nth 0 thread) :time))
                     "2026-09-30 17:32"))
      (should (equal (plist-get (nth 1 thread) :time) "9/23 7:57 PM")))))

(ert-deftest canvas-browser-a-time-that-is-no-iso-time-stays-as-it-is ()
  ;; GIVEN a thread whose comment gives its time in words
  ;; WHEN the HTML is translated
  ;; THEN the time is written as the page gave it, AND the data keeps it
  ;;      as the string it is
  (skip-unless (libxml-available-p))
  (let ((canvas-browser-translators
         (list (cons "example\\.org"
                     (canvas-browser-thread-translator
                      '(x-comment :author (attr by) :time (attr at)
                                  :body (inside slot "comment")))))))
    (let ((translation (canvas-browser-translate
                        "https://example.org/a"
                        "<x-comment by=\"alice\" at=\"8 hours ago\"><div slot=\"comment\">Hi.</div></x-comment>")))
      (should (equal (plist-get translation :text) "alice · 8 hours ago\n\nHi."))
      (should (equal (plist-get (car (plist-get translation :thread)) :time) "8 hours ago")))))
