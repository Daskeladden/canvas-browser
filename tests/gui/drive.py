# Send real events to the Emacs of gui-browser.el, through the XTest extension.
# The helpers are the ones of canvas-keys/tests/gui/drive.py.
import ctypes, os, sys, time
d = os.path.dirname(os.path.abspath(__file__))
for _ in range(150):
    if os.path.exists(os.path.join(d, "ready")): break
    time.sleep(0.1)
else:
    sys.exit("no ready file")
time.sleep(0.5)
ox, oy = map(int, open(os.path.join(d, "ready")).read().split())
X = ctypes.CDLL("libX11.so.6"); T = ctypes.CDLL("libXtst.so.6")
X.XOpenDisplay.restype = ctypes.c_void_p
dpy = ctypes.c_void_p(X.XOpenDisplay(None))
def flush(): X.XFlush(dpy); time.sleep(0.15)
def move(x, y): T.XTestFakeMotionEvent(dpy, -1, ox + x, oy + y, 0); flush()
def button(n, down): T.XTestFakeButtonEvent(dpy, n, down, 0); flush()
def click(n): button(n, True); button(n, False)
def key(sym, down): T.XTestFakeKeyEvent(dpy, X.XKeysymToKeycode(dpy, sym), down, 0); flush()
CTRL, SHIFT = 0xffe3, 0xffe1
def gestures():
    click(5)                                          # the wheel toward the reader: further down
    click(4)                                          # and back
    key(SHIFT, True); click(5); key(SHIFT, False)     # with shift, sideways
    key(CTRL, True); click(4); click(5); key(CTRL, False)   # with control, a step in and a step out
move(100, 100)                                        # beside the link
gestures()
move(350, 230)                                        # over the link, which is at 300, 200
time.sleep(0.5)
gestures()
time.sleep(0.5)
def text_window():                                    # a click in the text beside the page selects that window again
    move(-300, 100); click(1); time.sleep(0.6)
move(100, 100); click(1)                              # a click on the page, whose window is not selected
text_window()
move(100, 100); button(1, True); move(140, 100); move(180, 100); button(1, False)   # a drag on it
text_window()
move(100, 100); click(2)                              # the middle button on it
time.sleep(0.5)
open(os.path.join(d, "done"), "w").close()
