# canvas-browser

A web browser in an Emacs buffer, drawn on a canvas. A headless chromium
lays out the page, and Emacs speaks its DevTools protocol itself: no
helper process stands between them. Each page is a buffer of its own, so
your buffer keys are the tabs.

![A page with its hint labels, in an Emacs frame](screenshots/hints.png)

## Why

My Emacs runs on a remote machine, and I reach it over X. A link that I
open there must open there too: in a buffer, with the keys of Emacs, and
logged in to the sites I use. And I like to do as much as I can in
Emacs.

That includes the web applications of work, such as Outlook, Teams and
Confluence. They need JavaScript and a login, which eww cannot give
them, and here they are buffers.

## Other browsers in Emacs

eww draws the HTML as text in the buffer and runs no JavaScript. Use eww
where it is enough: `e` opens the same address there.

xwidget-webkit puts a WebKitGTK widget in the window. It needs an Emacs
that was built with xwidgets, which only the GTK build and the macOS
build can be.

EAF runs a Python process that draws the page with QtWebEngine in a Qt
window, and keeps that window over the Emacs window. The page is a
window of another program.

canvas-browser works in any build of Emacs 32 that has modules, and no
process stands between Emacs and chromium. The page is an image in the
buffer, which Emacs paints. So Emacs draws the hint labels on it, cuts a
part of it to copy a picture, and shows a page inside another buffer.

## What it needs

- Emacs 32 with canvas images and the cairo module of
  [canvas-diagram](https://github.com/Daskeladden/canvas-diagram).
- [canvas-keys](https://github.com/Daskeladden/canvas-keys), the keys
  that every canvas buffer shares.
- The `websocket` package, and `transient` for the menu.
- Chromium. On Ubuntu that is `sudo snap install chromium`.
- `Xvfb`, the X server that chromium draws on, out of sight. On Ubuntu
  that is `sudo apt install xvfb`.

A snap writes only outside the hidden directories of your home, so the
profile of a snap chromium goes to
`~/snap/chromium/common/canvas-browser-profile`.
Another chromium keeps its profile under your cache directory. The
profile holds the cookies, so a site stays logged in between sessions.
A snap also has a `/tmp` of its own and reads no hidden directory of your
home, so a local page it cannot read, such as one another package wrote
to `/tmp`, is copied to `~/snap/chromium/common/canvas-browser-files`
before it opens. Only that file is copied.

## Install

No archive carries canvas-browser. Clone it and the two canvas packages
it needs, and build the module of canvas-diagram. The README of
canvas-diagram says what that build needs.

```sh
git clone https://github.com/Daskeladden/canvas-keys.git
git clone https://github.com/Daskeladden/canvas-diagram.git
git clone https://github.com/Daskeladden/canvas-browser.git
make -C canvas-diagram        # builds canvas-cairo.so
```

Install `websocket` from an archive, with `M-x package-install`. Then
put the three directories on the load path, here with the clones in
`~/src`:

```elisp
(dolist (name '("canvas-keys" "canvas-diagram" "canvas-browser"))
  (add-to-list 'load-path (expand-file-name name "~/src")))
(autoload 'canvas-browser "canvas-browser" "Open a URL in a page buffer." t)
(autoload 'canvas-browser-browse-url "canvas-browser")
```

## Use

`M-x canvas-browser` asks for a URL and opens it in a buffer. The page is
laid out for the window, and chromium sends a picture whenever the page
changes. The picture is a JPEG, painted on the canvas.

### As the browser of Emacs

To have every link Emacs opens open here, from any package, set:

```elisp
(setq browse-url-browser-function #'canvas-browser-browse-url)
```

Where no canvas can show a page, as in a terminal frame, the URL goes to
`canvas-browser-fallback-browser`, which is eww unless you set it.

### A page in another buffer

`canvas-browser-embed` opens a page whose picture shows inside another
buffer, such as a video in the description of an issue. It returns the
text to insert there. The page is drawn while a window shows that buffer.
A click on the picture reaches the page, and `RET` clicks its middle,
which starts or stops a video. The page goes when the buffer is killed,
or once the buffer no longer holds the text.

If the page goes fullscreen, as a video does from its fullscreen button,
the page fills a fullscreen frame of its own. When the page leaves
fullscreen, the frame goes and the picture is back in the buffer. If you
delete the frame, the page leaves fullscreen.

### Bookmarks

A page is an Emacs bookmark. `B` keeps the page as one, named by its
title unless you give another name, and `J` picks one of the pages you
kept and opens it. The names come as bookmarks, so marginalia, consult
and embark treat them as they treat any bookmark. `C-x r m`,
`bookmark-jump` and the bookmark list work with pages too. In
`consult-bookmark` the pages are in the Web group, next to those of
eww, so `w SPC` narrows to them. A bookmark of a page that a buffer
shows already goes to that buffer.

### Blocking ads

`M-x canvas-browser-install-ublock` downloads the newest uBlock Origin
Lite from its releases on GitHub and installs it for canvas-browser. Run
it again to update. It blocks ads and trackers with the lists of uBlock
Origin, and it swaps a blocked ad script for a harmless stand-in, so a
page that waits for the script still works. It needs `unzip`.

Chromium loads extensions only when it starts. If chromium runs, the
command offers a restart, and `M-x canvas-browser-restart-chromium` does
the same at any time. The restart opens again every page that a window
shows, and the other pages open again when they are next shown.

Every directory with a `manifest.json` in
`canvas-browser-extension-directory` is an unpacked extension, which
chromium loads. By default that is
`~/snap/chromium/common/canvas-browser-extensions` for a snap chromium,
which can read no hidden directory of your home, and
`~/.local/share/canvas-browser/extensions` for another chromium.

## The keys

Normal state keeps the keys of Emacs:

| key | what it does |
|---|---|
| `o` | opens a URL in this buffer, or searches for the words you type |
| `B`, `J` | keep this page as a bookmark, and open a page you kept |
| `O` | the same in a page buffer of its own, as the `O` of Vimium and qutebrowser opens a tab |
| `g` | reads the page again |
| `M-p`, `M-n` | go back and forward in the history |
| `n`, `p` | scroll a line further down and back |
| `C-v`, `M-v`, the arrows | scroll a screen and a line |
| `<prior>`, `<next>` | scroll a screen back and further down |
| `<home>`, `<end>`, `C-<home>`, `C-<end>`, `M-<`, `M->` | go to the top and the foot of the page |
| `f` | labels what a click can reach, fields inside web components included, and clicks the one you name; the labels stay until you name one or press `ESC` |
| `M-w` | labels the blocks of the page, and copies the picture of the one you name; `C-u M-w` copies the whole window |
| `S` | labels the parts of the page that scroll on their own, and sends the scroll keys to the one you name |
| `v` | moves a caret through the text of the page with the motions of Emacs |
| `M-j` | puts the caret on text you type, as `avy-goto-char-timer` puts point |
| `TAB`, `S-TAB` | go to the next and the previous field, and type there |
| `C-s`, `C-r` | search the page, and step to the next hit and the one before |
| `d` | turns dark mode on or off, for this page and the next ones |
| `i` | sends the keys to the page, as a click in a field does |
| `t` | puts the text of the page in an ordinary buffer |
| `M-s M-l` | searches that text with `consult-line` |
| `e` | opens the same URL in eww |
| a click | clicks the page at that pixel, and types there if it is a field |
| the wheel | scrolls what lies under the pointer, over a link as well |

The scroll keys move the page itself, which happens at once. The wheel
goes to the page as a wheel, at the pixel the pointer is over, so that it
scrolls the part of the page that lies there, as it does in any browser.

A page is often more than one thing that scrolls: a list beside the text,
a pane of its own. `S` labels every such part with the letters of the
hints and hands the scroll keys to the one you name, the way `ace-window`
hands the keys to the window you name. The whole page carries the first
letter itself, in the top corner, so the keys go back to it by a letter
as well as by `ESC`; so does a part that the page has thrown away, which
it says when it happens.

The menu shows how the settings stand, with a star for a value that a
fresh Emacs would not have, and `C-x C-s` in the menu keeps the starred
ones through Customize.

The common canvas keys come from canvas-keys: `SPC` opens the menu,
`q` buries the buffer, `m` shows and hides the map, `W` writes the
picture, `M-w` copies it, `C` opens Customize, and `+`, `-` and `0` zoom
the page. The pointer becomes
a hand over anything that can be clicked.

Insert state sends every key to the page, so that you can type in a
form. A click in a box you can type in enters it by itself, and a click
anywhere else leaves it, so a form works as it does in any browser.
`ESC` comes back to normal state, and `C-g` as well, since another
package may have taken `ESC`. The header line says `insert` while it
lasts. A field inside a web component, as on the login page of Reddit,
counts as a field: the focus is followed into the component.

A hint goes only where a click in the middle of what shows of a thing
reaches it, or reaches its label. Whatever lies outside the window,
under a dialog, or around something else that can be clicked, as a link
around a button does, gets none. On the front page of Reddit that took
the hints from 388 to 35, and with the login dialog open the page under
it gets none at all. A click is followed up through the slot a web
component puts its text in, so a link built that way, as Reddit's
"Continue with Phone Number" is, counts.

A role counts only where it says the thing does something, as Vimium has
it: an icon or a heading marked as only looking takes no hint, and
inside a button it would crowd the button. Nor does a thing of a few
pixels, as a tracker is. A frame the size of a button counts, since a
click in it reaches what it holds, as Google's button to sign in is,
and a taller one is an advertisement and does not. On the page of a
subreddit that took 9 of 69 labels away, among them the five crowded
onto the bar that sorts the posts. Past the hints of two letters come
hints of three, so no box is left without one.

A region the page shows the pointer over counts too, since a script
that listens for clicks marks one so: the outermost element of it,
unless it fills half the window. A picture in a post of Reddit opens
this way, and so does a heading that opens a list. And a thing covered
by another thing that can be clicked gives its place to that one: the
link over a post of Reddit covers its title, and takes its hint there.
Finding the regions costs about 7 ms on the front page of Reddit, whose
web components hold 13,538 elements, because the search stops inside
anything that already counts.

A label goes at the top left corner of its box. When two would meet
there, as a post of Reddit that is a link meets the link of its author,
the small box keeps the corner and the label of the large one goes in
its middle, where a click on it lands anyway.

A key pressed before the letters of a hint picks what the hint does, as
the dispatch keys of avy do, and the prompt names it:

| key | the hint then |
|---|---|
| none | clicks it, or with `M-w` copies its picture |
| `y` | copies its address |
| `w` | copies its text |
| `c` | copies its picture, as much of it as shows |
| `o` | opens its address in a page buffer of its own |
| `e` | opens its address in eww |

`?` before the letters lists these keys in the prompt, as it does in
avy.

`f` labels what can be clicked, and `M-w` labels the blocks of the page:
a header, a sidebar, an article, a dialog, a table, a piece of code, a
picture. So `f y` and a hint copies the address of a link, and `M-w w`
and a hint copies the text of an article. None of these keys is a letter
of the hints, and the code refuses one that is.

When a click or a tab moves the focus of the page, smear-cursor flies
its cursor from the box that had the focus to the box that has it, as it
flies when point moves in text: from the name to the password of a form,
say. `canvas-browser-focus-function` names what flies, and nil turns it
off; without smear-cursor nothing flies.

Whatever a hint copies pulses: smear-cursor plays its copy effect over
the box of the address, the text or the picture, as it does over text
copied anywhere in Emacs. `canvas-browser-pulse-function` names what
pulses, and nil turns it off; without smear-cursor nothing pulses.

A picture is cut in Emacs, with cairo, from a picture of the whole
window, as much of the block as shows, and goes to the clipboard through
kill-ring-images. Chromium can cut a part itself, but it does so by
moving the view of the page to the part, and the view stays moved for a
while after: every frame and every picture of that while shows the page
shifted into a corner, or a part of it repeated across the window.

The editing keys of Emacs edit the field, as the Emacs key theme of GTK
and the insert mode of Surfingkeys have them do:

| key | in the field |
|---|---|
| `C-a`, `C-e` | the start and the end of the line |
| `C-f`, `C-b`, `C-n`, `C-p` | a character right and left, a line down and up |
| `M-f`, `M-b` | a word right and left |
| `DEL`, `C-d` | delete the character before and after the cursor |
| `M-DEL`, `M-d` | delete the word before and after the cursor |
| `C-k` | delete to the end of the line |
| `C-y` | type the newest kill of Emacs |
| `TAB`, `S-TAB` | the next and the previous field |
| `C-SPC` | set the mark: the motions after it mark a region of the field |
| `M-w`, `C-w` | copy the region, and cut it |
| `C-g` | drop the mark, and then give the keys back to Emacs |

A form is filled from the keyboard: `f` and the hint of the first field,
the text, `TAB`, the next text, and `RET`. `TAB` goes from field to
field only, as the `gi` of Vimium does, because a page puts buttons
between its fields: Reddit puts the one that shows the password between
the name and the password, and a tab of the browser's own stops there.
The first and the last field follow one another. A button is reached
with `f`.

A letter goes to the page as a key that types, so it reaches whatever
has the focus, as a key pressed in a browser does. Text put in without a
key lands at the caret instead, and the caret stays behind in a field
that the focus has left.

A key reaches chromium with the number Windows gives it as well as its
name, because chromium edits a field by that number: sent by its name
alone, backspace arrives as an event that deletes nothing.

The search paints every hit in the page itself, with the highlight API of
CSS, and scrolls to the one you are on. `window.find` answers in a
headless chromium but leaves nothing to see.

The buffer that `t` fills is an ordinary buffer, so isearch,
`consult-line` and the kill ring work on the text of the page.

## The caret of the page

`v` gives the page a caret, a point that the motions of Emacs move
through its text: `C-f` and `C-b` a character, `M-f` and `M-b` a word,
`C-n` and `C-p` a line, `C-a` and `C-e` to the ends of the line, `M-<`
and `M->` to the ends of the page, and the arrows as well. `C-SPC` sets
the mark and the motions then mark a region, which chromium draws as it
draws any selection; `M-w` copies it and pulses it. `C-g` drops the mark,
and then leaves the caret, as `ESC` and `v` do.

The caret starts after what has the focus, so that from a field you go
on reading where you were: `C-g` gives the keys back, and `v` starts
there. With nothing focused it starts at the first text in view, as the
caret mode of Vimium does. The page scrolls to keep the caret in view,
and smear-cursor flies from each place it stands to the next.

The caret is the selection of the page, moved as the Selection API of
the browser moves it, a character, a word or a line at a time. A page
that takes no typing draws no caret of its own, so a bar stands where it
is: the colour of the cursor of Emacs, with an edge of black or white,
whichever stands out, since a white bar alone vanishes on a white page.

## Jumping to text

`M-j` puts the caret on text you type, as `avy-goto-char-timer` puts
point. The key that runs `avy-goto-char-timer` does the same in a page,
even when `bind-key*` binds it. You type until you pause for `avy-timeout-seconds`, or for 0.5
seconds without avy. `RET` ends the text at once, `DEL` takes a
character back, and `ESC` gives up. Every place in view that shows the
text then takes a label, drawn and named as the labels of `f` are.
Naming a label puts the caret there, and smear-cursor flies to it.

A single place takes no label, as `avy-single-candidate-jump` has it.
Text in lower case matches either case, and text with a capital matches
only itself, as in avy. With the mark set, the region reaches to the
place instead, so `C-SPC` and a second `M-j` mark from one word to
another. From normal state, `M-j` also starts the caret.

A place counts only where its text shows. Invisible text takes no
label, and neither does text that a clipping box cuts off, such as a
heading kept for screen readers only. Text under something that paints,
such as the veil of a dialog or a banner, takes no label either. A clear
link that a page lays over a card paints nothing, so the title of a
Reddit post under it takes a label. Text that runs across two elements,
into a bold word for example, is not found, and neither is text inside a
frame.

## The name of a page buffer

A page buffer is named after the address it shows, and takes the new
one whenever the page moves, by a link, by `o` or by the page itself, so
that `C-x b` says what each buffer holds. The header line shows the
title the page gives itself once it has loaded: until then chromium
names a page after its file. A page embedded in another buffer keeps the
name its host gave it, since the host finds it by that name.

## Windows a page opens

A window a page opens gets a page buffer of its own, shown beside the
page that opened it, as a browser shows a new window: the buttons to
sign in with Google or with Apple open one, and so does a link that
opens a tab. When such a window closes itself, as the one to sign in
does once you have, its buffer goes with it. Chromium tells of every
page that opens and closes; only a page that one of these buffers
opened is taken, and a frame or a worker is left alone.

## The map of canvas-minimap

No map opens beside a page: canvas-browser puts `canvas-browser-mode`
in `canvas-minimap-exclude-modes`. A page is a picture, not lines of
text, and a map of it told the reader nothing the window does not. Take
the mode out of that list to have the strip back, as an empty block.

## While it moves, and once it stops

A page that is moving is painted from the JPEG frames of chromium's
screencast, which are cheap to make and show their workings around
small text. `canvas-browser-crisp-delay` after the last key or click the
window is asked for a still picture as a PNG, which loses nothing, and
that is what you read. Remote desktops do the same thing under the name
of a lossless refresh.

The keys decide the moment, not the frames. A page with a spinner on it
never falls quiet, and waiting for quiet would leave it lossy for as long
as you looked at it; waiting on the keys instead means the page is crisp
about four tenths of a second after you stop scrolling, measured at 0.39
and 0.43 seconds on a GitHub Actions page. A still picture costs chromium
a fifth of a second, so none is taken while you are working the keys, one
asked for before a key and arriving after it is dropped as a picture of
the page as it was, and no more than one is taken every
`canvas-browser-live-interval`.

A page is seldom either moving or still: a spinner beside a running job
turns while the text around it stands. Every still picture is therefore
followed by a question to the page about what is moving on it, which it
answers from `document.getAnimations()` and from its videos, canvases
and GIFs. While those parts cover less than `canvas-browser-live-share`
of the window, each frame is drawn into them alone and the rest of the
canvas keeps the still picture, with a fresh still every
`canvas-browser-live-interval`. Measured on a GitHub Actions page with
five spinners: 77 frames in 8.3 seconds, every one of them drawn into
the spinners alone, and four still pictures in between.

The page may of course change outside those parts, and that change waits
for the next still picture. Any command in the buffer forgets the moving
parts at once, so a scroll, a click or a key draws the whole window
again.

Chromium sends a screencast frame after every picture it is asked for,
the picture being a draw like any other. Painted, that frame would ask
for the next still picture, and the two would take turns on the canvas
for ever, which the reader sees as a pulse around small text. A frame
that is the picture the canvas already holds is therefore dropped, which
is told by the md5 of its bytes.

The numbers of one window of 1874 by 921 pixels: a JPEG at quality 70 is
120 kB and takes 0.10 s, the same picture at quality 95 is 235 kB, and
the PNG is 228 kB and takes 0.22 s. The PNG is therefore too slow for
twelve frames a second and cheap for a page standing still. Drawing a
frame into the moving parts costs about three milliseconds, and drawing
the whole window about four, so the parts are clipped in one path and the
picture read once: read once for each of five parts, the same frame cost
sixteen milliseconds.

## A window of its own

Chromium runs with a window, on an X display of its own: `Xvfb` is
started on `canvas-browser-display` (`:98` by default) the first time a
page is opened, and nothing is ever shown there. The page is read
through the screencast, so none of it travels to your own display, which
matters when that display is forwarded over ssh.

This is about what the sites see. A headless chromium says
`HeadlessChrome` in its user agent, keeps `navigator.webdriver` true and
has no WebGL at all, and a site behind a bot check reads all three: it
then asks you to pick out traffic lights rather than to tick a box. With
a window, and with WebGL drawn in software, the same four questions
answer as they do in any Chrome.

It is also faster. On the same page, the time from a scroll key to the
new picture is 5 ms with a window and 26 ms without it, a still picture
costs 0.18 s against 0.22 s, and the memory is the same to within half a
percent, plus 87 MB for `Xvfb`. Software WebGL is for WebGL alone:
drawing the whole window that way, which `--use-angle=swiftshader` does,
cost two whole cores.

Set `canvas-browser-headless` on a machine with no X server to give
chromium a window on.

## Settings

| setting | default | what it is |
|---|---|---|
| `canvas-browser-chromium` | chromium, chromium-browser, google-chrome | the names looked for |
| `canvas-browser-profile-directory` | by the chromium found | where the profile goes |
| `canvas-browser-cdp-timeout` | 10 | seconds to wait for chromium |
| `canvas-browser-headless` | nil | whether chromium runs without a window of its own |
| `canvas-browser-display` | :98 | the X display chromium draws its window on |
| `canvas-browser-quality` | 70 | the quality of a moving frame, from 1 to 100 |
| `canvas-browser-crisp-delay` | 0.4 | seconds of quiet before the page is drawn again without loss |
| `canvas-browser-pulse-function` | smear-cursor's copy effect | what draws the eye to what a hint copied, or nil |
| `canvas-browser-focus-function` | smear-cursor's flight | what draws the eye from the old focus to the new, or nil |
| `canvas-browser-live-interval` | 2.0 | seconds between two still pictures while a part keeps moving |
| `canvas-browser-live-share` | 0.3 | how much of the window may move and still count as parts of it |
| `canvas-browser-live-pad` | 4 | pixels a moving part grows by |
| `canvas-browser-frame-interval` | 0.08 | the shortest time between two drawings of the page |
| `canvas-browser-hidden-interval` | 2.0 | the same for a page no window shows |
| `canvas-browser-fresh-interval` | 2.0 | seconds between two looks at whether the window is fresh |
| `canvas-browser-second-try` | 0.5 | seconds before a window still empty is asked again |
| `canvas-browser-hint-keys` | asdfghjkl | the letters a hint is made of |
| `canvas-browser-hint-font` | Sans Bold 11 | the font a hint is written in |
| `canvas-browser-zoom-step` | 1.2 | what one zoom key changes |
| `canvas-browser-line-height` | 40 | pixels that `n` and `p` scroll |
| `canvas-browser-search-url` | DuckDuckGo | where words are searched for |
| `canvas-browser-spots-delay` | 0.3 | seconds of quiet before the pointer areas are read |

A page that stops answering for a few seconds says so in the echo area,
and `g` reads it again.

A window that has just changed size is filled as soon as chromium has
laid the page out for it, and a window that paints nothing for
`canvas-browser-fresh-interval` seconds asks for a picture of itself, so
a window left black by a missed frame fills itself again.

A page that no window shows is left to chromium, which throttles it as
it throttles any tab behind another: its frames are stopped until you
come back to it, and a handful of news sites no longer fight for the
machine.

## What it does not do

- Video and animation: a frame reaches the screen after what you do, and
  a full frame costs about 3.5 MB on a display without shared memory.
- Tabs of its own, downloads, printing and developer tools.
- A second engine.

## The tests

`make test` runs against a stubbed websocket and needs no chromium.
`make live` opens `tests/fixtures/page.html` in a real chromium, waits
for a picture, and reads the text of the page.
