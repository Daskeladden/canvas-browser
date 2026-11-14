# Send real mouse events to the X display, through the XTest extension.
# Each argument is one step, at whole pixels of the screen:
#   move:X,Y     put the pointer at X Y
#   glide:X,Y    move the pointer to X Y in steps of about 8 pixels
#   down:N up:N  press and release the button N
#   wait:S       wait S seconds
#   say:WORD     print WORD on a line of its own, for the Emacs that waits
import ctypes, sys, time
X = ctypes.CDLL("libX11.so.6"); T = ctypes.CDLL("libXtst.so.6")
X.XOpenDisplay.restype = ctypes.c_void_p
dpy = ctypes.c_void_p(X.XOpenDisplay(None))
if not dpy:
    sys.exit("xtest.py: no X display")
at = [0, 0]
def flush(pause=0.15): X.XFlush(dpy); time.sleep(pause)
def move(x, y, pause=0.15):
    T.XTestFakeMotionEvent(dpy, -1, x, y, 0); at[:] = [x, y]; flush(pause)
def glide(x, y):
    steps = max(1, round(max(abs(x - at[0]), abs(y - at[1])) / 8))
    x0, y0 = at
    for i in range(1, steps + 1):
        move(round(x0 + (x - x0) * i / steps), round(y0 + (y - y0) * i / steps), 0.02)
    flush()
for step in sys.argv[1:]:
    verb, _, rest = step.partition(":")
    if verb == "move": move(*map(int, rest.split(",")))
    elif verb == "glide": glide(*map(int, rest.split(",")))
    elif verb == "down": T.XTestFakeButtonEvent(dpy, int(rest), True, 0); flush()
    elif verb == "up": T.XTestFakeButtonEvent(dpy, int(rest), False, 0); flush()
    elif verb == "wait": time.sleep(float(rest))
    elif verb == "say": print(rest, flush=True)
    else: sys.exit("xtest.py: no step " + step)
