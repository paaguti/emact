#if !defined(lint)
static auto rcsid("$Id: x11.cpp,v 1.27 2018/09/09 07:21:10 jullien Exp $");
#endif

/*
 * This  program  is  free  software;  you can redistribute it and/or
 * modify  it  under  the  terms of the GNU General Public License as
 * published  by  the  Free  Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This  program  is  distributed in the hope that it will be useful,
 * but  WITHOUT ANY WARRANTY;  without  even the implied  warranty of
 * MERCHANTABILITY  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You  should have received a copy of the GNU General Public License
 * along  with  this  program;  if  not,  write  to the Free Software
 * Foundation,  Inc.,  59  Temple  Place  -  Suite  330,  Boston,  MA
 * 02111-1307, USA.
 */

/*
 * The routines in this file provide support for X11.
 */

#include "./emacs.h"

#if defined(_X11) && !defined(X_DISPLAY_MISSING)

#if defined(sun) && defined(__GNUC__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunknown-pragmas"
#endif /* __GNUC__ */

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/keysym.h>
#include <X11/cursorfont.h>

#if defined(sun) && defined(__GNUC__)
#pragma GCC diagnostic pop
#endif /* __GNUC__ */

#include <array>
#include <string>

#include "./x11.ico"

static void    X11clipcopy();
static void    X11clippaste();
static EMCHAR* X11title(EMCHAR* buf, EMCHAR* fname);

class X11Terminal final : public Terminal {
 public:
  /* Open terminal at the start. */
  X11Terminal();
  /* Close terminal at end. */
  ~X11Terminal();
  /* Get character from keyboard. */
  int
  get() override;
  /* Put character to display. */
  void
  insert(int c) override;
  /* Put a string to display. */
  void
  insert(const EMCHAR* s, int n) override;
  /* Flush output buffers. */
  void
  flush() override;
  /* Move the cursor, origin 0. */
  void
  move(int x, int y) override;
  /* Erase to end of line. */
  void
  eeol() override;
  /* Erase to end of page. */
  void
  eeop() override;
  /* Beep. */
  void
  beep() override;
  /* Start inverse video */
  void
  si() override;
  /* End   inverse video */
  void
  ei() override;
  /* Show/Hide the cursor */
  void
  cshow(bool flag) override;
  /* Check event */
  bool
  check() override;
  /* Put in raw mode */
  void
  rawmode() override;

 public:
  void
  cursor(bool flag) {
    char ascii{(char)DISPLAY::_curchar};

    XDrawImageString(_dpy,
                     _win,
                     (flag ? _gcinv : _gcstd),
                     _col * _wfnt,
                     _row * _hfnt + _fntinfo->ascent,
                     &ascii,
                     1);
  }

 private:
  enum ColorIndex {
    IBorder     = 0,
    IBackground = 1,
    IForeground = 2
  };

  static const size_t KEYSIZE{80};

  int
  getEvent();

  bool
  nearestcolor(Display* dpy, const char* name, XColor* rgb);

  void
  movecursor(int x, int y) {
    XWarpPointer(_dpy,
                 _win,
                 _win,
                 None,
                 None,
                 None,
                 None,
                 x * _wfnt,
                 y * _hfnt + _fntinfo->ascent);
  }

  static constexpr int MAXCOLOR{3};
  static constexpr int COLOR_TABLE_SIZE{8};

  Display*       _dpy{nullptr};
  Window         _win;
  XFontStruct*   _fntinfo{nullptr};
  GC             _gcstd;
  GC             _gcinv;

  std::array<unsigned long, MAXCOLOR> _color; // NOLINT(runtime/int)
  int            _row;
  int            _col;
  int            _x;
  int            _y;
  unsigned int   _width;
  unsigned int   _height;
  int            _char;
  int            _hfnt;
  int            _wfnt;

  int            _shift{0};
  int            _ctrl{0};
  int            _meta{0};

  static const char* X11rgbcolors[COLOR_TABLE_SIZE];

 public:
  static Display*     X11dpy;
  static Window       X11win;

  static bool         X11debug;
  static bool         X11expose;
  static bool         X11term;
  static const char*  X11foreground;
  static const char*  X11background;
  static const char*  X11border;
  static const char*  X11font;
  static const char*  X11geometry;
  static const char*  X11display;
  static const char*  X11name[MAXCOLOR];
  static int          X11argc;
  static char**       X11argv;
};

Display*     X11Terminal::X11dpy{nullptr};
Window       X11Terminal::X11win;
bool         X11Terminal::X11term{true};
bool         X11Terminal::X11debug{false};
bool         X11Terminal::X11expose{false};
const char*  X11Terminal::X11foreground{nullptr};
const char*  X11Terminal::X11background{nullptr};
const char*  X11Terminal::X11border{nullptr};
const char*  X11Terminal::X11font{nullptr};
const char*  X11Terminal::X11geometry{nullptr};
const char*  X11Terminal::X11display{nullptr};
const char*  X11Terminal::X11name[MAXCOLOR];
int          X11Terminal::X11argc{0};
char**       X11Terminal::X11argv{nullptr};

const char* X11Terminal::X11rgbcolors[COLOR_TABLE_SIZE] = {
  "rgb:0000/0000/0000",   /* black        */
  "rgb:0000/0000/7fff",   /* blue         */
  "rgb:0000/ffff/0000",   /* green        */
  "rgb:0000/dfff/ffff",   /* cyan         */
  "rgb:ffff/0000/0000",   /* red          */
  "rgb:ffff/0000/ffff",   /* magenta      */
  "rgb:ffff/ffff/0000",   /* yellow       */
  "rgb:ffff/ffff/ffff"    /* white        */
};

/*
 * A replacement for XAllocColor.  This function should never fail to
 * allocate  a color.  When XAllocColor fails,  we return the nearest
 * matching  color.  If we have to allocate many colors this function
 * isn't  a  great  solution;  the  XQueryColors() could be done just
 * once.
 */

bool
X11Terminal::nearestcolor(Display* dpy, const char* name, XColor* rgb) {
  auto screen(XDefaultScreen(dpy));
  auto cmap(XDefaultColormap(dpy, screen));
  auto depth(XDisplayPlanes(dpy, screen));

  if (!XParseColor(dpy, cmap, name, rgb)) {
    return false;
  }

  /*
   * First try just using XAllocColor.
   */

  if (XAllocColor(dpy, cmap, rgb)) {
    return true;
  }

  if (X11debug) {
    (void)std::fprintf(stderr, "Can't allocate color %s\n\r", name);
  }

  {
    XColor exact;
    if (XLookupColor(dpy, cmap, name, &exact, rgb) == 0) {
      (void)std::fprintf(stderr, "XLookupColor Fails for %s\r\n", name);
      return false;
    }
  }

  /*
   * Retrieve color table entries.
   */

  size_t cmapsize;
  if (depth > 16) {
    cmapsize = 8192;                 /* first 8192 entries   */
  } else {
    cmapsize = (size_t)(1 << depth); /* compute real entries */
  }

  auto ctable = new XColor[cmapsize];

  for (decltype(cmapsize) i{0}; i < cmapsize; ++i) {
    ctable[i].pixel = i;
  }

  XQueryColors(dpy, cmap, ctable, (int)cmapsize);

  /*
   * Find best match.
   */

  int bestmatch = -1;
  double mindist = 0.0; /* 3*2^16^2 exceeds long int precision. */

  for (decltype(cmapsize) i{0}; i < cmapsize; ++i) {
    double dr   = (double)rgb->red   - (double)ctable[i].red;
    double dg   = (double)rgb->green - (double)ctable[i].green;
    double db   = (double)rgb->blue  - (double)ctable[i].blue;
    double dist = dr * dr + dg * dg + db * db;

    if (bestmatch < 0 || dist < mindist) {
      bestmatch = (int)i;
      mindist   = dist;
    }
  }

  /*
   * Return result.
   */

  {
    XColor subcolor;

    subcolor.red   = ctable[bestmatch].red;
    subcolor.green = ctable[bestmatch].green;
    subcolor.blue  = ctable[bestmatch].blue;

    delete[] ctable;

    if (!XAllocColor(dpy, cmap, &subcolor)) {
      subcolor.pixel = (unsigned long)bestmatch;  // NOLINT(runtime/int)
    }

    *rgb = subcolor;
  }

  return true;
}

extern void makeCursesTerminal();

X11Terminal::X11Terminal() {
  XInitThreads();
  if ((X11term == false) || (X11dpy = XOpenDisplay(X11display)) == nullptr) {
    /*
     *      Switch to curses term and try again.
     */
    throw true;
  } else {
    _dpy = X11dpy;
  }

  XSynchronize(_dpy, 1);

  auto screen = XDefaultScreen(_dpy);
  auto depth  = XDisplayPlanes(_dpy, screen);
  auto height = XDisplayHeight(_dpy, screen);

  static auto DEFAULTBDR("Green");
  static auto DEFAULTBGD4("Blue");
  static auto DEFAULTFGD4("Yellow");

  // static auto DEFAULTFNT("8x13");
  static auto DEFAULTFNT("10x20");
  static auto DEFAULTGEO("80x30+0+0");

  if ((X11font == nullptr) &&
      ((X11font = XGetDefault(_dpy, X11argv[0], "font")) == nullptr)) {
    X11font = DEFAULTFNT;
  }

  if (((X11name[0] = X11border) == nullptr) &&
      ((X11name[0] = XGetDefault(_dpy, X11argv[0], "border")) == nullptr)) {
    X11name[0] = DEFAULTBDR;
  }

  auto DEFAULTBGD8(X11rgbcolors[1]);       /* Blue */
  auto DEFAULTFGD8(X11rgbcolors[3]);       /* Cyan */

#if defined(_CDE12) || defined(__linux__) || defined(__APPLE__)
  /*
   * CDE  1.2  has  a  default color for all application so the
   * two following tests allways match.
   */

  if (((X11name[1] = X11background) == nullptr) &&
      ((X11name[1] = XGetDefault(_dpy, X11argv[0], "background")) == nullptr)) {
    X11name[1] = ((depth > 4) ? DEFAULTBGD8 : DEFAULTBGD4);
  }

  if (((X11name[2] = X11foreground) == nullptr) &&
      ((X11name[2] = XGetDefault(_dpy, X11argv[0], "foreground")) == nullptr)) {
    X11name[2] = ((depth > 4) ? DEFAULTFGD8 : DEFAULTFGD4);
  }
#else
  X11name[1] = ((depth > 4) ? DEFAULTBGD8 : DEFAULTBGD4);
  X11name[2] = ((depth > 4) ? DEFAULTFGD8 : DEFAULTFGD4);
#endif

  if ((X11geometry == nullptr) &&
      ((X11geometry = XGetDefault(_dpy, X11argv[0], "geometry")) == nullptr)) {
    X11geometry = DEFAULTGEO;
  }

  XParseGeometry(X11geometry, &_x, &_y, &_width, &_height);

  if (X11debug) {
    (void)std::printf("Depth       = %i\r\n", depth);
    (void)std::printf("X11geometry = %s\r\n", X11geometry);
    (void)std::printf("X11name[0]  = %s\r\n", X11name[0]);
    (void)std::printf("X11name[1]  = %s\r\n", X11name[1]);
    (void)std::printf("X11name[2]  = %s\r\n", X11name[2]);
  }

  while (((size_t)_y * (size_t)_height) > (size_t)height) {
    --_y;
  }

  if (depth > 1) {
    static unsigned long X11pixelcolors[8];  // NOLINT(runtime/int)

    /*
     *      Read standard color scheme
     */

    for (int i{0}; i < COLOR_TABLE_SIZE; ++i) {
      XColor rgb;
      (void)nearestcolor(_dpy, X11rgbcolors[i], &rgb);

      X11pixelcolors[i] = rgb.pixel;

      if (X11debug) {
        (void)std::printf("color table %d %s: %lx\n\r",
                          i,
                          X11rgbcolors[i],
                          X11pixelcolors[i]);
      }
    }

    _color[IBorder]     = X11pixelcolors[1];
    _color[IForeground] = X11pixelcolors[opt::foreground_color % 8];
    _color[IBackground] = X11pixelcolors[opt::background_color % 8];

    if (X11debug) {
      (void)std::printf("Default foreground (index %d) = %lx\n\r",
                        opt::foreground_color,
                        _color[IForeground]);
      (void)std::printf("Default background (index %d) = %lx\n\r",
                        opt::background_color,
                        _color[IBackground]);
    }

    /*
     *      Set user preferences
     */

    for (size_t i(0); i < _color.size(); ++i) {
      XColor rgb;
      (void)nearestcolor(_dpy, X11name[i], &rgb);
      _color[i] = rgb.pixel;

      if (X11debug) {
        (void)std::printf("User color %s: %lx\n\r", X11name[i], _color[i]);
      }
    }
  } else {
    /*
     *      depth = 1 (Black & White)
     */
    _color[IBorder]     = BlackPixel(_dpy, screen);
    _color[IForeground] = BlackPixel(_dpy, screen);
    _color[IBackground] = WhitePixel(_dpy, screen);
  }

  if ((_fntinfo = XLoadQueryFont(_dpy, X11font)) == nullptr) {
    (void)std::fprintf(stderr, "Invalid font name %s\n", X11font);
    exit(1);
  } else {
    _wfnt = XTextWidth(_fntinfo, "_", 1);
    _hfnt = _fntinfo->ascent + _fntinfo->descent;
  }

  _x       = 0;
  _y       = 0;
  _width  *= (unsigned int)_wfnt;
  _height *= (unsigned int)_hfnt;
  _win     = XCreateSimpleWindow(_dpy,
                                 XRootWindow(_dpy, screen),
                                 _x, _y, _width, _height,
                                 2,
                                 _color[IBorder],
                                 _color[IBackground]);
  X11win = _win; /* global copy */

  auto icon = XCreateBitmapFromData(_dpy,
                                    _win,
                                    (char*)x11_bits,
                                    x11_width,
                                    x11_height);

  {
    XSizeHints sizehints;

    sizehints.flags      = PPosition | PSize | PMinSize;
    sizehints.x          = _x;
    sizehints.y          = _y;
    sizehints.width      = (int)_width;
    sizehints.height     = (int)_height;
    sizehints.min_width  = 100;
    sizehints.min_height = 100;

    XSetStandardProperties(_dpy,
                           _win,
                           X11argv[0],
                           X11argv[X11argc],
                           icon, X11argv, X11argc,
                           &sizehints);
  }

  {
    XWMHints wmhints;
    wmhints.flags         = IconPixmapHint | InputHint | StateHint;
    wmhints.input         = True;
    wmhints.initial_state = NormalState;
    wmhints.icon_pixmap   = icon;

    XSetWMHints(_dpy, _win, &wmhints);
  }

  XSelectInput(_dpy,
               _win,
               ExposureMask    |
               KeyPressMask    | KeyReleaseMask      |
               ButtonPressMask | StructureNotifyMask |
               EnterWindowMask | LeaveWindowMask     |
               KeymapStateMask);

  {
    XGCValues val;
    val.foreground = _color[IForeground];
    val.background = _color[IBackground];
    _gcstd = XCreateGC(_dpy, _win, (GCForeground|GCBackground), &val);
    XSetFont(_dpy, _gcstd, _fntinfo->fid);
  }
  {
    XGCValues val;
    val.foreground = _color[IBackground];
    val.background = _color[IForeground];
    _gcinv = XCreateGC(_dpy, _win, (GCForeground|GCBackground), &val);
    XSetFont(_dpy, _gcinv, _fntinfo->fid);
  }

  XDefineCursor(_dpy, _win, XCreateFontCursor(_dpy, XC_xterm));

  {
    auto delwin   = XInternAtom(_dpy, "WM_DELETE_WINDOW", 0);
    auto protocol = XInternAtom(_dpy, "WM_PROTOCOLS",     0);

    XChangeProperty(_dpy,
                    _win,
                    protocol,
                    XA_ATOM,
                    32,
                    PropModeReplace,
                    (unsigned char*)&delwin,
                    1);
  }

  _col = 0;
  _row = 0;

  this->setNbRows((_height / _hfnt) - 1);
  this->setNbCols(_width  / _wfnt);
  this->setInitialized();
  term = this;

  DISPLAY::_mouse = true;

  widget.w_clipcopy  = X11clipcopy;
  widget.w_clippaste = X11clippaste;
  widget.w_title     = X11title;

  XMapWindow(_dpy, _win);

  while (getEvent() != Expose) {
    continue;
  }

  this->flush();
}

X11Terminal::~X11Terminal() {
  XFlush(_dpy);
  XUnloadFont(_dpy, _fntinfo->fid);
  XFreeGC(_dpy, _gcstd);
  XFreeGC(_dpy, _gcinv);
  XDestroyWindow(_dpy, _win);
  XCloseDisplay(X11dpy);
}

void
X11Terminal::eeop() {
  XClearWindow(_dpy, _win);
}

void
X11Terminal::eeol() {
  XClearArea(_dpy,
             _win,
             _col * _wfnt,
             _row * _hfnt,
             0,
             (unsigned int)_hfnt,
             False);
}

void
X11Terminal::move(int row, int col) {
  _row = row;
  _col = col;
}

int
X11Terminal::get() {
  static int strokes = 0;

  _char = 0;

  while (_char == 0) {
    (void)getEvent();
  }

  if (opt::mouse_avoidance_mode && ++strokes >= opt::mouse_avoidance_nudge) {
    strokes = 0;

    /*
     * every N (default = 5) strokes, move mouse to the bottom
     */

    this->movecursor(this->getNbCols() + 1, this->getNbRows() + 1);
  }

  return _char;
}

int
X11Terminal::getEvent() {
  XEvent         event;
  XRectangle     clip;
  char           buf[KEYSIZE + 80]; /* magic, it's a X11 server BUG */
  KeySym         keysym;
  XComposeStatus cmp;
  int            nbkeys;

  X11Terminal::cursor(true);
  XNextEvent(_dpy, &event);
  X11Terminal::cursor(false);

  switch (event.type) {
  case Expose:
    if (X11expose == false) {
      X11expose = true;
      XSetInputFocus(_dpy, _win, RevertToParent, CurrentTime);
      return Expose;
    }

    {
      auto region = XCreateRegion();

      do {
        clip.x      = (short)event.xexpose.x;  // NOLINT(runtime/int)
        clip.y      = (short)event.xexpose.y;  // NOLINT(runtime/int)
        clip.width  = (unsigned short)event.xexpose.width;
        clip.height = (unsigned short)event.xexpose.height;
        XUnionRectWithRegion(&clip, region, region);
      } while (XCheckTypedEvent(_dpy, Expose, &event));

      XSetRegion(_dpy, _gcstd, region);
      DISPLAY::exposed();
      display->update();
      XDestroyRegion(region);
    }

    clip.x          = (short)_x;
    clip.y          = (short)_y;
    clip.width      = (unsigned short)_width;
    clip.height     = (unsigned short)_height;
    XSetClipRectangles(_dpy, _gcstd, 0, 0, &clip, 1, Unsorted);
    break;
  case ConfigureNotify:
    if (this->isInitialized() &&
        _width  == (unsigned int)event.xconfigure.width &&
        _height == (unsigned int)event.xconfigure.height) {
      /*
       * no changes !
       */
      break;
    }

    if (X11expose) {
      delete display;
    }

    _width  = (unsigned int)event.xconfigure.width;
    _height = (unsigned int)event.xconfigure.height;
    this->setNbRows((_height / _hfnt) - 1);
    this->setNbCols(_width / _wfnt);

    if (X11expose) {
      display = new DISPLAY;
      (void)WINSCR::resize();
    }

    break;
  case MappingNotify:
    XRefreshKeyboardMapping(&event.xmapping);
    break;
  case ButtonPress:
    XRaiseWindow(_dpy, _win);
    mevent.x      = event.xbutton.x / _wfnt;
    mevent.y      = event.xbutton.y / _hfnt;
    mevent.button = event.xbutton.button;

    if (_shift) {
      mevent.button |= MEvent::SHIFTBUTTON;
    }

    if (_ctrl) {
      mevent.button |= MEvent::CTRLBUTTON;
    }

    return _char = MEVT;
  case KeyRelease:
    (void)XLookupString((XKeyEvent*)&event.xkey,
                        buf,
                        KEYSIZE,
                        (KeySym*)&keysym,
                        (XComposeStatus*)&cmp);

    if (keysym == XK_Shift_L || keysym == XK_Shift_R) {
      _shift = 0;
    }
    if (keysym == XK_Control_L || keysym == XK_Control_R) {
      _ctrl  = 0;
    }
    if (keysym == XK_Meta_L) {
      _meta  = 0;
    }
    break;
  case EnterNotify:
    term->cshow(true);
    break;
  case LeaveNotify:
    term->cshow(false);
    break;
  case KeyPress:
    nbkeys = XLookupString((XKeyEvent*)&event.xkey,
                           buf,
                           KEYSIZE,
                           (KeySym*)&keysym,
                           (XComposeStatus*)&cmp);

    if (keysym == XK_Shift_L || keysym == XK_Shift_R) {
      _shift = 1;
    }

    if (keysym == XK_Control_L || keysym == XK_Control_R) {
      _ctrl = 1;
    }

    if (keysym == XK_Meta_L) {
      _meta = 1;
    }

    if ((keysym >= XK_Shift_L && keysym <= XK_Hyper_R) ||
        (keysym == XK_Mode_switch)) {
      break;
    }

    switch (keysym) {
    case XK_Down:
      _char = Ctrl|'N';
      break;
    case XK_Up:
      _char = Ctrl|'P';
      break;
    case XK_Right:
      if (_ctrl) {
        _char = META|'F';
      } else {
        _char = Ctrl|'F';
      }
      break;
    case XK_Left:
      if (_ctrl) {
        _char = META|'B';
      } else {
        _char = Ctrl|'B';
      }
      break;
    case XK_Home:
      if (_ctrl) {
        _char = CTLX|Ctrl|'P';
      } else {
        _char = META|'<';
      }
      break;
    case XK_End:
      if (_ctrl) {
        _char = CTLX|Ctrl|'N';
      } else {
        _char = META|'>';
      }
      break;
    case XK_Insert:
      _char = META|'I';
      break;
    case XK_Delete:
      _char = Ctrl|'H';
      break;
    case XK_Prior:
      _char = META|'V';
      break;
    case XK_Next:
      _char = Ctrl|'V';
      break;
    case XK_F1:
      _char = META|'?';
      break;
    case XK_F2:
      _char = Ctrl|'S';
      break;
    case XK_F3:
      _char = META|Ctrl|'F';
      break;
    case XK_F4:
      _char = CTLX|Ctrl|'I';
      break;
    case XK_F5:
      _char = METACH;
      break;
    case XK_F6:
      _char = CTLX|METACH;
      break;
    case XK_F7:
      _char = CTLX|'`';
      break;
    case XK_F8:
      _char = CTLX|Ctrl|'S';
      break;
    case XK_F9:
      _char = CTLX|'E';
      break;
    case XK_F11:
      _char = META|'M';
      break;
    case XK_space:
      _char = (_ctrl? (Ctrl|'@') :' ');
      break;
    default:
#if defined(AIX)
      {
        char* key = (char*)XLookupMapping(&event.xkey, &cmp);
        _char = (int)(*key & MAX_EMCHAR);
      }
#else
      _char = (int)(*buf & MAX_EMCHAR);
#endif

      if (X11debug) {
        (void)std::printf("nbkey=%d, key=%04x, mcs=%d%d%d\n\r",
                          nbkeys,
                          (int)keysym,
                          _meta,
                          _ctrl,
                          _shift);
      }

      if (nbkeys == 0) {
        int keychar = (int)keysym;

        /*
         * No characters in buffer, use keysym
         */

        if (_ctrl) {
          if (std::islower(keychar)) {
            _char = std::toupper(keychar);
          }
          _char -= '@';
        } else {
          _char  = keychar;
        }

        if (X11debug) {
          (void)std::printf("_char = %x\n\r", _char);
        }
      }

      if (_meta) {
        if (std::isalpha(_char)) {
          _char = (int)(META|std::toupper(_char));
        } else {
          _char = (int)(META|_char);
        }
      }
    }
    break;
  default:
    /*
     *      Ignore all other events
     */
    break;
  }

  return event.type;
}

void
X11Terminal::insert(int c) {
  unsigned char ascii = (unsigned char)c;

  switch (ascii) {
  case '\n' :
    if (_row < this->getNbRows()) {
      _row++;
      _col = 0;
    }
    break;
  case '\r' :
    _col = 0;
    break;
  case '\b' :
    if (_col > 0) {
      _col--;
    }
    break;
  default:
    XDrawImageString(_dpy,
                     _win,
                     _gcstd,
                     _col++ * _wfnt,
                     _row   * _hfnt + _fntinfo->ascent,
                     (char*)&ascii,
                     1);
  }
}

void
X11Terminal::insert(const EMCHAR* s, int n) {
  static constexpr size_t MAXLINESIZE{256};
  char ascii[MAXLINESIZE];
  emutoa(s, ascii, MAXLINESIZE);

  XDrawImageString(_dpy,
                   _win,
                   _gcstd,
                   _col * _wfnt,
                   _row * _hfnt + _fntinfo->ascent,
                   ascii,
                   n);
  _col += n;
}

void
X11Terminal::ei() {
  XSetForeground(_dpy, _gcstd, _color[IForeground]);
  XSetBackground(_dpy, _gcstd, _color[IBackground]);
}

void
X11Terminal::si() {
  XSetForeground(_dpy, _gcstd, _color[IBackground]);
  XSetBackground(_dpy, _gcstd, _color[IForeground]);
}

void
X11Terminal::beep() {
  /*
   * Volume range from -100 to 100
   */

  XBell(_dpy, -75);
}

void
X11Terminal::flush() {
  XFlush(_dpy);
}

void
X11Terminal::cshow(bool flag) {
  X11Terminal::cursor(flag);
  XSync(_dpy, False);
}

bool
X11Terminal::check() {
  _char = 0;

  XSync(_dpy, False);
  while (XEventsQueued(_dpy, QueuedAlready)) {
    (void)getEvent();
    if (_char == 0x07) {
      return true;
    }
  }

  return false;
}

void
X11Terminal::rawmode() {
}

static void
X11clipcopy() {
#if defined(XlibSpecificationRelease) && (XlibSpecificationRelease > 4)
  auto kbuf = kget();
  /*
   * Copy kill-buffer to Clipboard.
   */

  if (kbuf.first && kbuf.second) {
    XStoreBytes(X11Terminal::X11dpy, (char*)kbuf.first, (int)kbuf.second);
  }
#endif
}

static void
X11clippaste() {
#if defined(XlibSpecificationRelease) && (XlibSpecificationRelease > 4)
  int len;
  auto buf(XFetchBytes(X11Terminal::X11dpy, &len));

  /*
   * Copy Clipboard to kill-buffer.
   */

  kdelete();

  for (int i(0); i < len; ++i) {
    (void)kinsert((int)buf[i]);
  }

  XFree(buf);
#endif
}

static EMCHAR*
X11title(EMCHAR* buf, EMCHAR* fname) {
  static  EMCHAR  title[64] = { 0 };

  (void)fname;

  if (emstrcmp(buf, title) != 0) {
    char ascii[64];
    int  i;
    (void)emsprintf(title,
                     ECSTR("%s (%dx%d)"),
                     buf,
                     term->getNbRows(), term->getNbCols());
    for (i = 0; title[i]; ++i) {
      ascii[i] = (char)title[i];
    }
    ascii[i] = 0;
    XStoreName(X11Terminal::X11dpy, X11Terminal::X11win, ascii);
  }

  return buf;
}

int
X11emacs(int argc, char* argv[]) {
  int arg;
  X11Terminal::X11argc = argc;
  X11Terminal::X11argv = argv;

  /*
   *      First, read from command line the X values.
   */

  for (arg = 1; arg < argc; ++arg) {
    const std::string opt{argv[arg]};
    if (opt == "-bg" || opt == "-background") {
      X11Terminal::X11background = argv[++arg];
      continue;
    }
    if (opt == "-fg" || opt == "-foreground") {
      X11Terminal::X11foreground = argv[++arg];
      continue;
    }
    if (opt == "-fn" || opt == "-font") {
      X11Terminal::X11font = argv[++arg];
      continue;
    }
    if (opt == "-bd" || opt == "-border") {
      X11Terminal::X11border = argv[++arg];
      continue;
    }
    if (opt == "-t" || opt == "-terminal" || opt == "-nw") {
      X11Terminal::X11term = false;
      continue;
    }
    if (opt == "-geometry") {
      X11Terminal::X11geometry = argv[++arg];
      continue;
    }
    if (opt == "-display") {
      X11Terminal::X11display = argv[++arg];
      continue;
    }
    if (opt == "-debug") {
      X11Terminal::X11debug = true;
      continue;
    }
    if (opt[0] == '-') {
      (void)std::printf("Usage: %s\n", argv[0]);
      (void)std::printf("\t[-background <background-color>]\n");
      (void)std::printf("\t[-border     <border-color>]\n");
      (void)std::printf("\t[-display    <disaply-name>]\n");
      (void)std::printf("\t[-foreground <foreground-color>]\n");
      (void)std::printf("\t[-font       <font-name>]\n");
      (void)std::printf("\t[-geometry   <initial-position>]\n");
      (void)std::printf("\t[-terminal]\n");
      (void)std::printf("\t[-debug]\n");
      return 1;
    }
    break;
  }

  auto argcount = (argc - arg) + 1;
  char* arglist[32];
  arglist[0] = argv[0];

  /*
   * copy arg list
   */

  for (int i = 1; i < argcount; ++i) {
    arglist[i] = argv[arg++];
  }
  arglist[argcount] = nullptr;

  /*
   * call the real entry point
   */

  Editor emacs{argcount, arglist};
  emacs.engine();

  return 0;
}

Terminal*
makeX11Terminal() {
  return new X11Terminal;
}
#endif
