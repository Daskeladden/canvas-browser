;;; canvas-browser-merge.el --- Merge a thread into an org note -*- lexical-binding: t -*-

;; Copyright (C) 2026 canvas-browser contributors

;; Author: Daskeladden
;; Keywords: hypermedia, outlines

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; A thread that a translator gave is written into an org note as a
;; list, by `canvas-browser-thread-org'.  When the thread is sent to
;; that note again, the note is not replaced, and the thread is not
;; added a second time.  The messages are merged one by one:
;;
;; - A message that the note does not hold is added where the page has
;;   it: after the message that comes before it there, under the
;;   message it answers.  A thread whose newest message comes first, as
;;   a mail program shows one, gets a new message at its top.
;; - A message that the note holds with the same text is left alone.
;;   Pictures and white space do not count.
;; - A message whose text differs keeps its text in the note, and the
;;   new text goes right under it, as a message of its own that says it
;;   was edited and when it was sent.  The note cannot tell an edit on
;;   the page from an edit of the reader, so nothing is overwritten.
;;
;; A message is known in the note by its head: its title, its author and
;; its time, as `canvas-browser-org-head' writes it.  Two messages with
;; one head are told apart by their order.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'canvas-browser-translate)

(defconst canvas-browser--org-item-regexp "^\\( *\\)- \\(.+\\)$"
  "A line that starts an item of an org list, with its indent and its head.")

(defconst canvas-browser--edited-regexp " · edited, as of \\[[^]\n]+\\]\\'"
  "The end of the head of a new version of a message.")

;;;; The items of the note

(defun canvas-browser--item-line-p ()
  "Whether the line at point starts an item of an org list."
  (looking-at-p canvas-browser--org-item-regexp))

(defun canvas-browser--inside-p (depth)
  "Whether the line at point belongs inside an item at DEPTH.
A blank line does, and so does a line further in than the item."
  (or (looking-at-p "[ \t]*$") (> (current-indentation) (* 2 depth))))

(defun canvas-browser--last-line-end (depth stop)
  "The end of the last line that holds text inside an item at DEPTH.
Point is on the head line of the item.  The lines after it count as
long as they are inside it, and STOP, a function, does not hold."
  (let ((end (line-end-position)))
    (save-excursion
      (while (and (zerop (forward-line 1)) (not (eobp))
                  (canvas-browser--inside-p depth) (not (funcall stop)))
        (unless (looking-at-p "[ \t]*$")
          (setq end (line-end-position)))))
    end))

(defun canvas-browser--undent (text depth)
  "TEXT, the lines of an item at DEPTH, without the indent of its text."
  (let ((margin (* 2 (1+ depth))))
    (string-trim
     (mapconcat (lambda (line) (substring line (min margin (length (string-trim-right line)))))
                (split-string text "\n")
                "\n"))))

(defun canvas-browser--org-entry ()
  "The item of an org list whose head line point is on, as a plist.
:key is the head without the mark of an edit, :edited says whether the
item is a new version, :depth how far in it stands, :start where it
starts, :text its own text, :text-end where its own text ends, and :end
where the items under it end."
  (looking-at canvas-browser--org-item-regexp)
  (let* ((depth (/ (length (match-string 1)) 2))
         (head (match-string-no-properties 2))
         (edited (string-match canvas-browser--edited-regexp head))
         (text-end (canvas-browser--last-line-end depth #'canvas-browser--item-line-p)))
    (list :key (if edited (substring head 0 edited) head)
          :edited (and edited t)
          :depth depth
          :start (line-beginning-position)
          :text (canvas-browser--undent
                 (buffer-substring-no-properties (min (1+ (line-end-position)) text-end) text-end)
                 depth)
          :text-end text-end
          :end (canvas-browser--last-line-end depth #'ignore))))

(defun canvas-browser--org-entries ()
  "The items of the org lists of this buffer, in their order."
  (save-excursion
    (goto-char (point-min))
    (let ((entries nil))
      (while (not (eobp))
        (when (canvas-browser--item-line-p)
          (push (canvas-browser--org-entry) entries))
        (forward-line 1))
      (nreverse entries))))

(defun canvas-browser--entry-index (entries key n)
  "The place in ENTRIES of the Nth message with KEY that is no new version."
  (let ((seen 0))
    (cl-position-if (lambda (entry)
                      (and (equal (plist-get entry :key) key)
                           (not (plist-get entry :edited))
                           (= (cl-incf seen) n)))
                    entries)))

(defun canvas-browser--latest-version (entries index)
  "The newest version in ENTRIES of the message at INDEX.
The versions follow the message itself, each with its key."
  (let ((key (plist-get (nth index entries) :key))
        (latest (nth index entries)))
    (cl-loop for entry in (nthcdr (1+ index) entries)
             while (and (plist-get entry :edited) (equal (plist-get entry :key) key))
             do (setq latest entry))
    latest))

;;;; Comparing and counting

(defun canvas-browser--comparable (text)
  "TEXT as it is compared: without pictures, and with one space for white space.
A picture is an attachment link in a note, and a name in brackets in a
thread that was sent, so neither counts."
  (string-trim
   (replace-regexp-in-string
    "[ \t\n\u00a0]+" " "
    (replace-regexp-in-string "\\[\\[attachment:[^]\n]*\\]\\]\\|\\[[^]\n]*\\]" "" (or text "")))))

(defun canvas-browser--thread-keys (thread)
  "The heads of the messages of THREAD, replies among them."
  (mapcan (lambda (item)
            (cons (canvas-browser-org-head item)
                  (canvas-browser--thread-keys (plist-get item :replies))))
          thread))

(defun canvas-browser--count-seen (thread seen)
  "Count the messages of THREAD, replies among them, in SEEN, by their heads."
  (dolist (item thread)
    (cl-incf (gethash (canvas-browser-org-head item) seen 0))
    (canvas-browser--count-seen (plist-get item :replies) seen)))

;;;; Merging

(defun canvas-browser--insert-item (item depth position &optional before)
  "Insert ITEM, with its replies, as an item of an org list at DEPTH at POSITION.
POSITION is the end of a line that holds text, and ITEM goes after it.
With BEFORE, POSITION is the start of the line of an item, and ITEM
goes before that item.  An empty line comes between."
  (save-excursion
    (goto-char position)
    (if before
        (insert (canvas-browser--org-item item depth) "\n\n")
      (insert "\n\n" (canvas-browser--org-item item depth)))))

(defun canvas-browser--merge-edit (item entries index counts now)
  "Add the text of ITEM under the message at INDEX of ENTRIES, if it is new.
The new text goes right under the newest version, as a version of its
own, edited as of NOW.  COUNTS is (ADDED . EDITED)."
  (let ((latest (canvas-browser--latest-version entries index)))
    (unless (equal (canvas-browser--comparable (plist-get latest :text))
                   (canvas-browser--comparable (plist-get item :text)))
      (let ((version (copy-sequence item)))
        (setq version (plist-put version :replies nil))
        (setq version (plist-put version :edited now))
        (canvas-browser--insert-item version (plist-get (nth index entries) :depth)
                                     (plist-get latest :text-end))
        (cl-incf (cdr counts))))))

(defun canvas-browser--message-end (entries index)
  "Where the message at INDEX of ENTRIES ends, with all that lies under it.
The replies to a message lie under its newest version."
  (plist-get (canvas-browser--latest-version entries index) :end))

(defun canvas-browser--next-known (later entries seen)
  "The entry of ENTRIES for the first message of LATER that the note holds.
LATER are the messages that follow a new one, and SEEN counts the heads
up to the new one."
  (let ((counts (copy-hash-table seen)))
    (cl-some (lambda (item)
               (let* ((key (canvas-browser-org-head item))
                      (index (canvas-browser--entry-index
                              entries key (cl-incf (gethash key counts 0)))))
                 (and index (nth index entries))))
             later)))

(defun canvas-browser--new-place (previous later parent entries keys seen)
  "Where a message that the note does not hold goes: (POSITION DEPTH BEFORE).
It goes where the page has it.  That is after PREVIOUS, (KEY . N) of
the message before it, or else before the first message of LATER that
the note holds.  With neither, it is the last under PARENT, (KEY . N)
of the message it answers, or the last message of the thread, whose
heads are KEYS.  See `canvas-browser--insert-item\\=' for BEFORE."
  (let ((before-it (and previous
                        (canvas-browser--entry-index entries (car previous) (cdr previous))))
        (after-it (canvas-browser--next-known later entries seen))
        (above (and parent (canvas-browser--entry-index entries (car parent) (cdr parent)))))
    (cond (before-it (list (canvas-browser--message-end entries before-it)
                           (plist-get (nth before-it entries) :depth) nil))
          (after-it (list (plist-get after-it :start) (plist-get after-it :depth) t))
          (above (list (canvas-browser--message-end entries above)
                       (1+ (plist-get (nth above entries) :depth)) nil))
          (t (list (cl-loop for entry in entries
                            when (member (plist-get entry :key) keys)
                            maximize (plist-get entry :end))
                   0 nil)))))

(defun canvas-browser--merge-new (item place seen counts)
  "Add ITEM, a message the note does not hold, with its replies, at PLACE.
PLACE is what `canvas-browser--new-place\\=' gives.  SEEN counts the
heads, and COUNTS is (ADDED . EDITED)."
  (canvas-browser--insert-item item (nth 1 place) (nth 0 place) (nth 2 place))
  (canvas-browser--count-seen (plist-get item :replies) seen)
  (cl-incf (car counts) (1+ (length (canvas-browser--thread-keys (plist-get item :replies))))))

(defun canvas-browser--merge-items (items parent keys seen counts now)
  "Merge ITEMS, the replies to PARENT or the whole thread, into this note.
PARENT is (KEY . N) of the message that ITEMS answer, or nil.  KEYS are
the heads of the whole thread, SEEN counts the heads so far, COUNTS is
\\(ADDED . EDITED), and NOW is the time of the send."
  (let ((previous nil))
    (while items
      (let* ((item (pop items))
             (key (canvas-browser-org-head item))
             (n (cl-incf (gethash key seen 0)))
             (entries (canvas-browser--org-entries))
             (index (canvas-browser--entry-index entries key n)))
        (if (not index)
            (canvas-browser--merge-new
             item (canvas-browser--new-place previous items parent entries keys seen)
             seen counts)
          (canvas-browser--merge-edit item entries index counts now)
          (canvas-browser--merge-items (plist-get item :replies) (cons key n)
                                       keys seen counts now))
        (setq previous (cons key n))))))

(defun canvas-browser-org-merge (thread &optional now)
  "Merge THREAD, as a translator gives it, into the org note in this buffer.
See the commentary of canvas-browser-merge.el for how.  NOW is the time
of the send, the current time when nil.  Return (ADDED . EDITED), the
counts of the messages that were added and of the new versions, or nil
when the note holds no message of THREAD and nothing was merged."
  (let ((keys (canvas-browser--thread-keys thread)))
    (when (cl-some (lambda (entry) (member (plist-get entry :key) keys))
                   (canvas-browser--org-entries))
      (let ((counts (cons 0 0)))
        (canvas-browser--merge-items thread nil keys (make-hash-table :test #'equal)
                                     counts (or now (current-time)))
        counts))))

(provide 'canvas-browser-merge)
;;; canvas-browser-merge.el ends here
