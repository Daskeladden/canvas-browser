;;; canvas-browser-translate.el --- Translators of pages into text to read -*- lexical-binding: t -*-

;; Copyright (C) 2026 canvas-browser contributors

;; Author: Daskeladden
;; Keywords: hypermedia, tools

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; canvas-browser sends the text of a page as the page shows it, and
;; reads nothing into it.  A translator is the one place that may know
;; a site.  It is a small snippet in the configuration of whoever wants
;; it: a pattern for the address of a page, and a function that makes
;; text to read from the HTML of that page.
;;
;; Most pages worth a translator are threads: a post, and comments with
;; replies under them.  `canvas-browser-thread-translator' makes the
;; function for those from a description of which elements are the
;; items and where each keeps its author, its time and its text:
;;
;;   (canvas-browser-add-translator
;;    "example\\.org/thread/"
;;    (canvas-browser-thread-translator
;;     '(x-comment :author (attr by) :time (attr at)
;;                 :body (inside slot "comment"))))
;;
;; A page that is no thread takes any function of the parsed HTML,
;; which is what `libxml-parse-html-region' gives and dom.el reads.
;;
;; This file names no site.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'dom)
(require 'iso8601)

(declare-function libxml-parse-html-region "xml.c"
                  (start end &optional base-url discard-comments))

(defcustom canvas-browser-translators nil
  "The translators of pages, each (REGEXP . FUNCTION).
A page whose address matches REGEXP is sent as the text that FUNCTION
makes of it, and not as the text the page shows.  FUNCTION is called
with the HTML of what is sent, parsed as `libxml-parse-html-region\='
gives it.  It returns the text, a thread as Lisp data, as
`canvas-browser-thread-translator\=' makes one, or nil when it finds
nothing to translate there.  The first entry that matches is the one
that counts.  Add one with `canvas-browser-add-translator\='."
  :type '(alist :key-type regexp :value-type function)
  :group 'canvas-browser)

(defun canvas-browser-add-translator (regexp function)
  "Translate the pages whose address matches REGEXP with FUNCTION.
A translator that REGEXP has already is replaced, so a snippet can be
evaluated again after a change.  See `canvas-browser-translators\='."
  (setf (alist-get regexp canvas-browser-translators nil nil #'equal) function))

(defun canvas-browser-translator-for (url)
  "The entry of `canvas-browser-translators\=' for URL, or nil."
  (cl-find-if (lambda (entry) (string-match-p (car entry) url))
              canvas-browser-translators))

;;;; The text of parsed HTML

(defconst canvas-browser--no-text-tags '(script style svg noscript template comment)
  "The elements of HTML that hold no text of a page.")

(defconst canvas-browser--block-tags
  '(address article aside blockquote dd details div dl dt figcaption figure
    footer form h1 h2 h3 h4 h5 h6 header hr li main nav ol pre section
    summary table tr ul)
  "The elements of HTML whose text has lines of its own.")

(defun canvas-browser--break-of (tag)
  "How the text of an element with TAG stands apart: `paragraph', `line' or nil."
  (cond ((eq tag 'p) 'paragraph)
        ((memq tag canvas-browser--block-tags) 'line)))

(defun canvas-browser--hidden-p (node)
  "Whether NODE, an element, says itself that the page does not show it.
That is the attribute hidden, or a style of its own with no display.
What a style sheet hides is not known here: the HTML does not say."
  (or (assq 'hidden (dom-attributes node))
      (string-match-p "display:[ \t]*none" (or (dom-attr node 'style) ""))))

(defun canvas-browser--picture-text (node)
  "The text that stands for NODE, a picture, or nil for none.
A picture for the look alone, with an empty description or the role
presentation, stands for nothing.  An emoji stands as itself, a
description or a label in brackets, and a picture with neither as
[image]."
  (let ((alt (dom-attr node 'alt))
        (label (dom-attr node 'aria-label)))
    (cond ((or (equal alt "") (equal (dom-attr node 'role) "presentation")) nil)
          ((and alt (not (string-match-p "[[:alnum:]]" alt))) alt)
          (t (format "[%s]" (or alt label "image"))))))

(defun canvas-browser--dom-pieces (node)
  "The text of NODE as a list of strings, with `line' and `paragraph' between.
A symbol stands where the text after it starts a new line, or a new
paragraph."
  (cond ((stringp node) (list node))
        ((memq (dom-tag node) canvas-browser--no-text-tags) nil)
        ((canvas-browser--hidden-p node) nil)
        ((eq (dom-tag node) 'br) (list 'line))
        ((eq (dom-tag node) 'img) (delq nil (list (canvas-browser--picture-text node))))
        (t (let ((break (canvas-browser--break-of (dom-tag node)))
                 (inside (mapcan #'canvas-browser--dom-pieces (dom-children node))))
             (if break
                 (append (list break) inside (list break))
               inside)))))

(defun canvas-browser--insert-piece (text break)
  "Insert TEXT at point, after BREAK when the buffer holds text already.
The white space of TEXT, a non-breaking space among it, is one space,
and none at the start of a line."
  ;; A paragraph of a non-breaking space alone is how a chat writes an
  ;; empty line, so it counts as white space too.
  (let ((text (replace-regexp-in-string "[ \t\n\r\u00a0]+" " " text t t)))
    (when (or break (bobp))
      (setq text (string-trim-left text)))
    (unless (string-empty-p text)
      (unless (bobp)
        (insert (pcase break ('paragraph "\n\n") ('line "\n") (_ ""))))
      (insert text)
      t)))

(defun canvas-browser--join-pieces (pieces)
  "PIECES of `canvas-browser--dom-pieces\=' as one text."
  (with-temp-buffer
    (let ((break nil))
      (dolist (piece pieces)
        (cond ((eq piece 'paragraph) (setq break 'paragraph))
              ((eq piece 'line) (setq break (or break 'line)))
              ((canvas-browser--insert-piece piece break) (setq break nil)))))
    (string-trim (replace-regexp-in-string " +$" "" (buffer-string)))))

(defun canvas-browser-dom-text (node)
  "The text of NODE, parsed HTML, with its lines and its paragraphs.
A paragraph stands apart by an empty line, a block and a broken line
have a line of their own, and white space is one space.  A picture
stands as its description.  Scripts, styles and drawings hold no text,
and neither does an element that says itself that it is hidden."
  (canvas-browser--join-pieces (canvas-browser--dom-pieces node)))

(defun canvas-browser--dom-find (node predicate)
  "The first element inside NODE for which PREDICATE holds, or nil.
The elements are looked at in the order of the page."
  (cl-some (lambda (child)
             (and (consp child)
                  (if (funcall predicate child)
                      child
                    (canvas-browser--dom-find child predicate))))
           (dom-children node)))

(defun canvas-browser-dom-with (node attribute value)
  "The first element inside NODE whose ATTRIBUTE is VALUE, or nil.
The elements are looked at in the order of the page."
  (canvas-browser--dom-find node (lambda (child) (equal (dom-attr child attribute) value))))

;;;; A thread: items, and the items inside them
;;
;; A thread is Lisp data: a list of items.  An item is a plist with
;; :title, :author, :time, :text and :replies, each left out when the
;; page has none.  :time is a time of Emacs when the page gives an ISO
;; 8601 time, and else the string it gives.  :replies is a thread too.

(defun canvas-browser--read-of (element attribute)
  "ELEMENT, or its ATTRIBUTE when ATTRIBUTE is given; nil without ELEMENT."
  (and element (if attribute (dom-attr element attribute) element)))

(defun canvas-browser--access (accessor node)
  "What ACCESSOR reads of NODE, an item of a thread: a string, an element or nil.
ACCESSOR is one of these, or nil for nothing:
  (attr NAME)                    the attribute NAME of NODE
  (inside ATTRIBUTE VALUE [READ]) the first element inside NODE whose
                                 ATTRIBUTE is VALUE
  (matching ATTRIBUTE REGEXP [READ]) the first element inside NODE whose
                                 ATTRIBUTE matches REGEXP
  (tag NAME [READ])              the first element inside NODE with tag NAME
  a function                     what it gives for NODE
With READ, it is the attribute READ of the element that was found."
  (pcase accessor
    ('nil nil)
    (`(attr ,name) (dom-attr node name))
    (`(inside ,attribute ,value . ,read)
     (canvas-browser--read-of (canvas-browser-dom-with node attribute value) (car read)))
    (`(matching ,attribute ,regexp . ,read)
     (canvas-browser--read-of
      (canvas-browser--dom-find
       node (lambda (child)
              (let ((value (dom-attr child attribute)))
                (and value (string-match-p regexp value)))))
      (car read)))
    (`(tag ,name . ,read)
     (canvas-browser--read-of
      (canvas-browser--dom-find node (lambda (child) (eq (dom-tag child) name)))
      (car read)))
    ((pred functionp) (funcall accessor node))
    (_ (error "canvas-browser: %S reads nothing of an item of a thread" accessor))))

(defun canvas-browser--string (value)
  "VALUE as text: a string as it is, an element as its text; nil when empty."
  (let ((text (cond ((stringp value) (string-trim value))
                    ((consp value) (canvas-browser-dom-text value)))))
    (and text (not (string-empty-p text)) text)))

(defun canvas-browser--read (field spec node)
  "The FIELD of NODE that SPEC says where to find, as text, or nil."
  (canvas-browser--string (canvas-browser--access (plist-get spec field) node)))

(defun canvas-browser--full-date (time)
  "TIME, a string, as a time of Emacs when it names a full date, or nil.
A full date has a year, a month, a day and a time of day.  It is read
as a time here."
  (pcase (ignore-errors (parse-time-string time))
    (`(,_ ,minute ,hour ,day ,month ,year . ,_)
     (and minute hour day month year
          (encode-time (list 0 minute hour day month year nil -1 nil))))))

(defun canvas-browser--time-of (time)
  "TIME, a string, as a time of Emacs, or as it is when it is no clear time.
A clear time is an ISO 8601 time, or a full date in words."
  (or (ignore-errors (encode-time (iso8601-parse time)))
      (canvas-browser--full-date time)
      time))

(defun canvas-browser--item (node spec body replies)
  "NODE as an item of a thread that SPEC describes.
BODY is what holds its text, and REPLIES are the items inside it."
  (let ((time (canvas-browser--read :time spec node)))
    (cl-loop for (field value) on (list :title (canvas-browser--read :title spec node)
                                        :author (canvas-browser--read :author spec node)
                                        :time (and time (canvas-browser--time-of time))
                                        :text (canvas-browser--string body)
                                        :replies replies)
             by #'cddr
             when value append (list field value))))

(defun canvas-browser--item-key-p (key node)
  "Whether NODE is an item that KEY names: a tag, or (ATTRIBUTE VALUE)."
  (if (symbolp key)
      (eq (dom-tag node) key)
    (equal (dom-attr node (car key)) (cadr key))))

(defun canvas-browser--item-spec (node items)
  "What ITEMS say of NODE when it is one of their items, or nil."
  (cdr (cl-find-if (lambda (item) (canvas-browser--item-key-p (car item) node)) items)))

(defun canvas-browser--thread-items (node items skip)
  "The items of a thread in NODE and inside it, in their order.
ITEMS is ((KEY . SPEC)...).  SKIP is the element that holds the text of
the item around NODE: what lies in a text is no item.  An item with no
title, no author and no text is none, as the frame of a picture is
that a page marks as an item: the items inside it take its place."
  (let ((spec (and (consp node) (canvas-browser--item-spec node items))))
    (cond ((or (stringp node) (eq node skip)) nil)
          (spec
           (let* ((body (canvas-browser--access (plist-get spec :body) node))
                  (replies (canvas-browser--items-inside node items body))
                  (item (canvas-browser--item node spec body replies)))
             (if (cl-some (lambda (field) (plist-get item field)) '(:title :author :text))
                 (list item)
               replies)))
          (t (canvas-browser--items-inside node items skip)))))

(defun canvas-browser--items-inside (node items skip)
  "The items inside NODE; see `canvas-browser--thread-items\='."
  (mapcan (lambda (child) (canvas-browser--thread-items child items skip))
          (dom-children node)))

(defun canvas-browser-thread-translator (&rest items)
  "A translator for a page that is a thread, whose items ITEMS describe.
Each of ITEMS is (KEY :title T :author A :time W :body B).  KEY names
the element that is one item, as a post or a comment is: its tag, or
\(ATTRIBUTE VALUE) for an element whose ATTRIBUTE is VALUE.  The others
say where the item keeps its title, its author, its time and its text,
and each may be left out.  Each is a way to read a part of the item,
as `canvas-browser--access\=' takes it: (attr NAME), (inside ATTRIBUTE
VALUE), (tag NAME), or a function of the element.

The translator gives the thread as Lisp data: see the top of this part
of canvas-browser-translate.el.  An item inside another one, as a reply
inside the comment that it answers, is one of its :replies.  A page
with no such item gives nil."
  (lambda (dom) (canvas-browser--thread-items dom items nil)))

;;;; A thread written out

(defun canvas-browser--indent (text depth)
  "TEXT with each line that holds something two spaces further in a DEPTH."
  (let ((margin (make-string (* 2 depth) ?\s)))
    (mapconcat (lambda (line) (if (string-empty-p line) line (concat margin line)))
               (split-string text "\n")
               "\n")))

(defun canvas-browser--time-text (time)
  "TIME to read: a time of Emacs as a day and a time of day here."
  (if (stringp time) time (format-time-string "%Y-%m-%d %H:%M" time)))

(defun canvas-browser--item-text (item)
  "ITEM to read: its title, its author and its time, then its text."
  (let* ((time (plist-get item :time))
         (who (delq nil (list (plist-get item :author)
                              (and time (canvas-browser--time-text time)))))
         (head (delq nil (list (plist-get item :title)
                               (and who (string-join who " · "))))))
    (string-join (delq nil (list (and head (string-join head "\n")) (plist-get item :text)))
                 "\n\n")))

(defun canvas-browser-thread-text (thread &optional depth)
  "THREAD, as a translator gives it, as text to read.
Each item names its author and its time, then gives its text, and a
reply stands two spaces further in than what it answers.  DEPTH is how
far in the items of THREAD stand, 0 when nil."
  (let ((depth (or depth 0)))
    (mapconcat (lambda (item)
                 (string-join
                  (delq nil (list (canvas-browser--indent (canvas-browser--item-text item) depth)
                                  (and (plist-get item :replies)
                                       (canvas-browser-thread-text
                                        (plist-get item :replies) (1+ depth)))))
                  "\n\n"))
               thread "\n\n")))

(defun canvas-browser--org-time (time)
  "TIME for org: a time of Emacs as an inactive timestamp of org."
  (if (stringp time) time (format-time-string "[%Y-%m-%d %a %H:%M]" time)))

(defun canvas-browser--org-bold (text)
  "TEXT in bold for org, or nil without TEXT."
  (and text (format "*%s*" text)))

(defun canvas-browser-org-head (item)
  "The head of ITEM in an org list: its title, its author and its time.
Two items with one head are one message, as far as a note can tell."
  (let ((time (plist-get item :time)))
    (string-join (delq nil (list (canvas-browser--org-bold (plist-get item :title))
                                 (canvas-browser--org-bold (plist-get item :author))
                                 (and time (canvas-browser--org-time time))))
                 " · ")))

(defun canvas-browser--org-item (item depth)
  "ITEM as an item of an org list at DEPTH, with its replies inside it.
An ITEM with :edited, a time, is a new version of a message, and its
head says so."
  (let* ((edited (plist-get item :edited))
         (head (concat (canvas-browser-org-head item)
                       (if edited
                           (format " · edited, as of %s" (canvas-browser--org-time edited))
                         "")))
         (text (plist-get item :text)))
    (string-join
     (delq nil (list (canvas-browser--indent (concat "- " head) depth)
                     (and text (canvas-browser--indent text (1+ depth)))
                     (and (plist-get item :replies)
                          (canvas-browser-thread-org (plist-get item :replies) (1+ depth)))))
     "\n\n")))

(defun canvas-browser-thread-org (thread &optional depth)
  "THREAD, as a translator gives it, as an org list.
Each item is an item of the list, with its title and its author in
bold, its time as an inactive timestamp, and its text under it.  A
reply is an item of the list inside the item it answers.  An empty line
stands between two items, so that each reads on its own.  DEPTH is how
far in the items of THREAD stand, 0 when nil."
  (mapconcat (lambda (item) (canvas-browser--org-item item (or depth 0))) thread "\n\n"))

;;;; Translating

(defun canvas-browser--parse-html (html)
  "HTML as `libxml-parse-html-region\=' reads it."
  (with-temp-buffer
    (insert html)
    (libxml-parse-html-region (point-min) (point-max))))

(defun canvas-browser--translation-of (result)
  "What a translator gave, RESULT, as a translation, or nil when it is none.
A string is the text.  A list is a thread, which is written out as the
text, and kept as the data beside it."
  (cond ((stringp result)
         (when-let* ((text (canvas-browser--string result)))
           (list :text text)))
        ((consp result)
         (list :text (canvas-browser-thread-text result) :thread result))))

(defun canvas-browser-translate (url html)
  "What the translator of URL makes of HTML, or nil.
It is a plist: :text is the text to read, and :thread is the thread as
Lisp data, when the translator gave one.  It is nil when URL has no
translator, when the translator finds nothing to translate, and in an
Emacs that cannot read HTML.  A translator that fails is an error that
names its pattern."
  (when-let* ((entry (and (libxml-available-p) (canvas-browser-translator-for url))))
    (condition-case failure
        (canvas-browser--translation-of
         (funcall (cdr entry) (canvas-browser--parse-html html)))
      (error (error "canvas-browser: the translator for %s failed: %s"
                    (car entry) (error-message-string failure))))))

(provide 'canvas-browser-translate)
;;; canvas-browser-translate.el ends here
