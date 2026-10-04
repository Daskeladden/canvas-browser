;;; canvas-browser-merge-tests.el --- tests of merging a thread into a note -*- lexical-binding: t -*-
(require 'ert)
(require 'cl-lib)
(require 'canvas-browser-merge)

(defconst canvas-browser-test--now (encode-time (iso8601-parse "2026-10-03T11:00:00Z"))
  "The time of a send in these tests.")

(defun canvas-browser-test--item (author minute text &rest replies)
  "An item of a thread by AUTHOR at MINUTE past ten, with TEXT and REPLIES."
  (append (list :author author
                :time (encode-time (iso8601-parse (format "2026-10-02T10:%02d:00Z" minute))))
          (and text (list :text text))
          (and replies (list :replies replies))))

(defconst canvas-browser-test--first-send
  (list (canvas-browser-test--item "alice" 1 "What is the role?"
                                   (canvas-browser-test--item "bob" 2 "The drawing layer."))
        (canvas-browser-test--item "carol" 3 "Nice."))
  "A thread as it was sent the first time.")

(defmacro canvas-browser-test--in-note (thread &rest body)
  "Run BODY in a note that holds THREAD under a line, as the receiver wrote it.
A line of the reader follows the list.  The clock is at UTC."
  (declare (indent 1))
  `(let ((process-environment (cons "TZ=UTC" process-environment)))
     (set-time-zone-rule "UTC")
     (unwind-protect
         (with-temp-buffer
           (insert "#+title: Roles\nFrom [[https://example.org][A page]]:\n\n"
                   (canvas-browser-thread-org ,thread)
                   "\n\nMy own words about it.\n")
           ,@body)
       (set-time-zone-rule nil))))

(defun canvas-browser-test--note-of (thread)
  "What a note that holds THREAD alone says, at UTC, after its first lines."
  (canvas-browser-thread-org thread))

(ert-deftest canvas-browser-a-merge-adds-the-new-messages-where-they-belong ()
  ;; GIVEN a note that holds a thread as it was sent the first time,
  ;;       with a line of the reader after it
  ;; WHEN the thread is sent again with a new reply to alice and a new
  ;;      message at the end
  ;; THEN the reply stands under alice, after what was under her, AND
  ;;      the new message stands after the last one, before the line of
  ;;      the reader, AND nothing else changed, AND two new messages are
  ;;      counted
  (let ((again (list (canvas-browser-test--item
                      "alice" 1 "What is the role?"
                      (canvas-browser-test--item "bob" 2 "The drawing layer.")
                      (canvas-browser-test--item "dan" 4 "Cairo, mostly."))
                     (canvas-browser-test--item "carol" 3 "Nice.")
                     (canvas-browser-test--item "erin" 5 "Agreed."))))
    (canvas-browser-test--in-note canvas-browser-test--first-send
      (should (equal (canvas-browser-org-merge again canvas-browser-test--now) '(2 . 0)))
      (should (equal (buffer-string)
                     (concat "#+title: Roles\nFrom [[https://example.org][A page]]:\n\n"
                             (canvas-browser-test--note-of again)
                             "\n\nMy own words about it.\n"))))))

(ert-deftest canvas-browser-a-new-message-goes-where-the-page-has-it ()
  ;; GIVEN a note that holds a thread whose newest message comes first,
  ;;       as a mail program shows a conversation
  ;; WHEN the thread is sent again with a newer message at its top, and
  ;;      one between the two that were there
  ;; THEN each new message stands where the page has it: the first
  ;;      before the message that was the first, the other between the
  ;;      two, AND the note reads as the page does
  (let ((first (list (canvas-browser-test--item "bob" 3 "The later mail.")
                     (canvas-browser-test--item "alice" 1 "The first mail.")))
        (again (list (canvas-browser-test--item "carol" 5 "The newest mail.")
                     (canvas-browser-test--item "bob" 3 "The later mail.")
                     (canvas-browser-test--item "dan" 2 "One that was missed.")
                     (canvas-browser-test--item "alice" 1 "The first mail."))))
    (canvas-browser-test--in-note first
      (should (equal (canvas-browser-org-merge again canvas-browser-test--now) '(2 . 0)))
      (should (equal (buffer-string)
                     (concat "#+title: Roles\nFrom [[https://example.org][A page]]:\n\n"
                             (canvas-browser-test--note-of again)
                             "\n\nMy own words about it.\n"))))))

(ert-deftest canvas-browser-a-merge-of-the-same-thread-changes-nothing ()
  ;; GIVEN a note that holds a thread
  ;; WHEN the same thread is sent again
  ;; THEN the note is as it was, AND nothing is counted
  (canvas-browser-test--in-note canvas-browser-test--first-send
    (let ((before (buffer-string)))
      (should (equal (canvas-browser-org-merge canvas-browser-test--first-send
                                               canvas-browser-test--now)
                     '(0 . 0)))
      (should (equal (buffer-string) before)))))

(defun canvas-browser-test--with-links (thread)
  "THREAD with an address for each of its items, and for their replies."
  (mapcar (lambda (item)
            (let ((linked (append (list :link (format "https://example.org/m/%s"
                                                      (plist-get item :author)))
                                  item)))
              (if (plist-get item :replies)
                  (plist-put (copy-sequence linked) :replies
                             (canvas-browser-test--with-links (plist-get item :replies)))
                linked)))
          thread))

(ert-deftest canvas-browser-a-message-that-has-its-address-now-is-the-same-message ()
  ;; GIVEN a note that holds a thread, written when the page gave no
  ;;       address of a message
  ;; WHEN the same thread is sent again, each message with its address
  ;; THEN the note is as it was, AND nothing is counted: a message is
  ;;      known by who wrote it and when
  (canvas-browser-test--in-note canvas-browser-test--first-send
    (let ((before (buffer-string)))
      (should (equal (canvas-browser-org-merge
                      (canvas-browser-test--with-links canvas-browser-test--first-send)
                      canvas-browser-test--now)
                     '(0 . 0)))
      (should (equal (buffer-string) before)))))

(ert-deftest canvas-browser-a-merge-knows-a-message-that-links-to-its-address ()
  ;; GIVEN a note that holds a thread whose messages link to their
  ;;       addresses
  ;; WHEN the thread is sent again with one new message, and with the
  ;;      text of bob changed
  ;; THEN one new and one edited message are counted, AND the edited one
  ;;      stands under the old one with its link before the mark of the
  ;;      edit, AND a third send of the same adds nothing
  (let* ((first (canvas-browser-test--with-links canvas-browser-test--first-send))
         (again (canvas-browser-test--with-links
                 (list (canvas-browser-test--item
                        "alice" 1 "What is the role?"
                        (canvas-browser-test--item "bob" 2 "The drawing layer, in C."))
                       (canvas-browser-test--item "carol" 3 "Nice.")
                       (canvas-browser-test--item "erin" 5 "Agreed.")))))
    (canvas-browser-test--in-note first
      (should (equal (canvas-browser-org-merge again canvas-browser-test--now) '(1 . 1)))
      (should (string-search
               (concat "  - *bob* · [2026-10-02 Fri 10:02] · [[https://example.org/m/bob][link]]"
                       " · edited, as of [2026-10-03 Sat 11:00]\n\n"
                       "    The drawing layer, in C.")
               (buffer-string)))
      (should (string-search
               "- *erin* · [2026-10-02 Fri 10:05] · [[https://example.org/m/erin][link]]"
               (buffer-string)))
      (let ((before (buffer-string)))
        (should (equal (canvas-browser-org-merge again canvas-browser-test--now) '(0 . 0)))
        (should (equal (buffer-string) before))))))

(ert-deftest canvas-browser-an-edited-message-goes-under-the-old-one ()
  ;; GIVEN a note that holds a thread
  ;; WHEN the thread is sent again, with the text of bob changed
  ;; THEN the new text stands right under the old one, as a message of
  ;;      its own that says it was edited and when it was sent, AND the
  ;;      old text stays, AND one edited message is counted, AND a third
  ;;      send of the same adds nothing more
  (let ((edited (list (canvas-browser-test--item
                       "alice" 1 "What is the role?"
                       (canvas-browser-test--item "bob" 2 "The drawing layer, in C."))
                      (canvas-browser-test--item "carol" 3 "Nice."))))
    (canvas-browser-test--in-note canvas-browser-test--first-send
      (should (equal (canvas-browser-org-merge edited canvas-browser-test--now) '(0 . 1)))
      (should (string-search
               (concat "  - *bob* · [2026-10-02 Fri 10:02]\n\n    The drawing layer.\n\n"
                       "  - *bob* · [2026-10-02 Fri 10:02] · edited, as of [2026-10-03 Sat 11:00]\n\n"
                       "    The drawing layer, in C.\n\n- *carol*")
               (buffer-string)))
      (let ((before (buffer-string)))
        (should (equal (canvas-browser-org-merge edited canvas-browser-test--now) '(0 . 0)))
        (should (equal (buffer-string) before))))))

(ert-deftest canvas-browser-pictures-and-white-space-are-no-edit ()
  ;; GIVEN a note whose message links a picture that was attached
  ;; WHEN the thread is sent again, where the picture stands in brackets
  ;;      and the lines break elsewhere
  ;; THEN the message counts as the same, and nothing is added
  (let ((first (list (canvas-browser-test--item "alice" 1 "Look:\n\n[[attachment:picture-1.jpg]]")))
        (again (list (canvas-browser-test--item
                      "alice" 1 (concat "Look:  " (propertize "[image]" 'canvas-browser-picture "a.jpg"))))))
    (canvas-browser-test--in-note first
      (should (equal (canvas-browser-org-merge again canvas-browser-test--now) '(0 . 0))))))

(ert-deftest canvas-browser-messages-of-one-minute-are-told-apart-by-their-order ()
  ;; GIVEN a note with two messages of one author in one minute
  ;; WHEN the thread is sent again with a third one in that minute
  ;; THEN one message is added, after the two
  (let* ((two (list (canvas-browser-test--item "bob" 5 "One.")
                    (canvas-browser-test--item "bob" 5 "Two.")))
         (three (append two (list (canvas-browser-test--item "bob" 5 "Three.")))))
    (canvas-browser-test--in-note two
      (should (equal (canvas-browser-org-merge three canvas-browser-test--now) '(1 . 0)))
      (should (string-search (canvas-browser-test--note-of three) (buffer-string))))))

(ert-deftest canvas-browser-a-note-without-the-thread-is-not-merged ()
  ;; GIVEN a note that holds none of the messages of a thread
  ;; WHEN the thread is merged into it
  ;; THEN nothing happens, and the caller hears so, to add it whole
  (with-temp-buffer
    (insert "#+title: Other\n\n- a list of my own\n")
    (should-not (canvas-browser-org-merge canvas-browser-test--first-send canvas-browser-test--now))
    (should (equal (buffer-string) "#+title: Other\n\n- a list of my own\n"))))
