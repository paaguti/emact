#if !defined(lint)
static auto rcsid("$Id: xpterm.cpp,v 1.41 2018/09/09 07:25:14 jullien Exp $");
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
 * xpterm.c : The routines in this file provide support for WINDOWS.
 */

#if     !defined(STRICT)
#define STRICT          /* Strict type checking for Windows */
#endif

#define DEBUG_ON_CONSOLE

#define _CRT_SECURE_NO_DEPRECATE        1
#define _CRT_NONSTDC_NO_DEPRECATE       1
#define _CRT_NON_CONFORMING_SWPRINTFS   1

#define _WIN32_WINNT 0x0501

#if     defined(_WIDECHARS)
#if     !defined(_UNICODE)
#define _UNICODE
#endif
#if     !defined(UNICODE)
#define UNICODE
#endif
#endif

#include <windows.h>
#include <tchar.h>
#include "emacs.h"
#include "xpterm.h"

#if     defined(_UNICODE)
#define system  _wsystem
#endif

/*
 * Constant definitions
 */

#define XP_EMACS_CS    CS_BYTEALIGNCLIENT|CS_HREDRAW|CS_VREDRAW

static constexpr auto XP_EMACS_ALTERNATE_FONT_SIZE(12);
static constexpr auto XP_EMACS_STANDARD_FONT_SIZE(10);
static constexpr auto XP_EMACS_DEFAULT_FONT_SIZE(XP_EMACS_STANDARD_FONT_SIZE);
static constexpr auto XP_EMACS_APPNAME(_T("emacs"));
static constexpr auto XP_EMACS_LOAD_MESSAGE(_T("EmACTLoadMessage"));
static constexpr auto XP_EMACS_FIND_MESSAGE(_T("EmACTFindMessage"));
static constexpr auto XP_EMACS_CLASS(_T("EmACTWClass"));
static constexpr auto XP_EMACS_MAXPATH(256);

/*
 * Private variables
 */

static HWND xpcurdlg{nullptr};  /* Current dialog box      */
static bool xpprocess_pending{false};

#if     defined(_UNICODE)
#define XP_DRAW_X       (_drawxpos)
#define XP_DRAW_Y       (_drawy * _charheight)
#else
#define XP_DRAW_X       (_drawx * _charwidth)
#define XP_DRAW_Y       (_drawy * _charheight)
#endif

/*
 * Shared memory
 */

#define XP_EMACS_SHMEM
#pragma data_seg(".shared")
_declspec(allocate(".shared")) TCHAR  xpshared[XP_EMACS_MAXPATH];
#pragma data_seg()
#pragma comment(linker, "/SECTION:.shared,RWS")

/*
 * User color definitions
 */

static constexpr auto XPCOLOR_BLACK     = RGB(  0,   0,   0);
static constexpr auto XPCOLOR_BLUE      = RGB(  0,   0, 128);
static constexpr auto XPCOLOR_GREEN     = RGB(  0, 255,   0);
static constexpr auto XPCOLOR_CYAN      = RGB(128, 255, 255);
static constexpr auto XPCOLOR_RED       = RGB(255,   0,   0);
static constexpr auto XPCOLOR_MAGENTA   = RGB(255,   0, 255);
static constexpr auto XPCOLOR_YELLOW    = RGB(255, 255,   0);
static constexpr auto XPCOLOR_WHITE     = RGB(255, 255, 255);

/*
 * Other color definitions
 */

static constexpr auto XPCOLOR_GRAY      = RGB(128, 128, 128);
static constexpr auto XPCOLOR_LIGHTGRAY = RGB(192, 192, 192);
static constexpr auto XPCOLOR_DARK_CYAN = RGB( 96, 192, 192);
static constexpr auto XPCOLOR_DARK_BLUE = RGB(  0,  36,  86);

/*
 * Other constants
 */

static constexpr auto XP_NO_CHAR(-1);

/*
 * Structure for GetOpenFileName
 */

static OPENFILENAME xpofn;

/*
 * Standard terminal interface dispatch table.
 */

static void   xpcutcopy(WPARAM wParam);
static void   xphelp(HWND hwnd);
static void   xpclippaste();
static void   xpclipcopy();
static TCHAR* xptitle(TCHAR* buf, TCHAR* fname);
static void   xpprint();

/*
 * Tools
 */

static HDC     xpgetprinterdc();

#if     defined(_UNICODE)
static EMCHAR  xpunicodegen(); // For tests
#endif

/*
 * Windows entry point and callbacks
 */

int     WINAPI   _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int);
static  LRESULT  APIENTRY xpmainwndproc(HWND, UINT, WPARAM, LPARAM);
static  LRESULT  APIENTRY xpabout(HWND, UINT, WPARAM, LPARAM);

class XpTerminal final : public Terminal {
 public:
  static void xpchangefont(int size);
  static void xpsettextattrib();
  static int  xpsystemspawn(const TCHAR *cmd);
  static int  xpdecodechar(int c);
  static void xpfindreplace(bool replacep);
  static int  xpfindmessage(LPFINDREPLACE lpfr);

 private:
  static void xpmousebottom();
  static void xpsetfontsize(int size);
  static void xpprinterror(TCHAR *txt , BOOL fexit);
  static int  xpquit();
  static void xpshowlines(TCHAR *chBuf, int nLen);

 private:
  /* Showing cursor */
  bool _cursoron{false};
  /* X cursor bit position */
  int _drawxpos{0};
  /* X cursor */
  int _drawx{0};
  /* Y cursor */
  int _drawy{0};

 public:
  /* Open terminal at the start. */
  XpTerminal();
  /* Close terminal at end. */
  ~XpTerminal();
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
  /* End inverse video */
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
  rawmode() override {}

 public:
  /* Width of a char */
  static int _charwidth;
  /* Height of a char */
  static int _charheight;
  /* TTy DC */
  static HDC _dc;
  /* Current Window */
  static HWND _wnd;
  /* Current background color */
  static DWORD _bgcolor;
  /* Current foreground color */
  static DWORD _fgcolor;
  /* Current brush */
  static HBRUSH _brush;
  /* Current font */
  static HFONT _font;
  /* hInstance */
  static HINSTANCE _inst;
  /* Accelerator table */
  static HACCEL _acctable;
  /* Application Menu */
  static HMENU _menu;
  /* Previous instance */
  static HANDLE _prev;
  /* Command line */
  static int _cmdshow;
  /* Registered load message */
  static UINT _loadmsg;
  /* Registered find message */
  static UINT _findmsg;
  /* to check for mouse move*/
  static int _strokes;
  /* Open flag */
  static bool _openp;
  /* Next char */
  static int _char;
  /* color table */
  static COLORREF _colortable[8];
};

int       XpTerminal::_charwidth;
int       XpTerminal::_charheight;
HDC       XpTerminal::_dc;
HWND      XpTerminal::_wnd;
DWORD     XpTerminal::_bgcolor;
DWORD     XpTerminal::_fgcolor;
HBRUSH    XpTerminal::_brush;
HFONT     XpTerminal::_font;
HINSTANCE XpTerminal::_inst;
HACCEL    XpTerminal::_acctable;
HMENU     XpTerminal::_menu;
HANDLE    XpTerminal::_prev;
int       XpTerminal::_cmdshow{0};
UINT      XpTerminal::_loadmsg;
UINT      XpTerminal::_findmsg;
int       XpTerminal::_strokes{0};
bool      XpTerminal::_openp{false};
int       XpTerminal::_char{-1};

COLORREF  XpTerminal::_colortable[8] = {
  XPCOLOR_BLACK,
  XPCOLOR_DARK_BLUE,
  XPCOLOR_GREEN,
  XPCOLOR_CYAN,
  XPCOLOR_RED,
  XPCOLOR_MAGENTA,
  XPCOLOR_YELLOW,
  XPCOLOR_WHITE
};

void
TTYopen() {
  tt = new XpTerminal;
}

/*
 * Process message while waiting.
 */

static void
xpwait() {
  MSG msg;

  PeekMessage(&msg, XpTerminal::_wnd, 0, 0, PM_NOREMOVE);
}

void
XpTerminal::xpsettextattrib() {
  TEXTMETRIC txMetric;
  RECT       rcClient;
  ABC        abc;

  GetClientRect(_wnd, (LPRECT)&rcClient);

  auto nParentWidth  = rcClient.right  - rcClient.left;
  auto nParentHeight = rcClient.bottom - rcClient.top;

  GetTextMetrics(_dc, &txMetric);
  _charwidth  = txMetric.tmAveCharWidth;
  _charheight = txMetric.tmHeight + txMetric.tmExternalLeading;

  if ((txMetric.tmPitchAndFamily & TMPF_TRUETYPE) == 0) {
    GetCharWidth32(_dc, (UINT)'M', (UINT)'M', &_charwidth);
  } else if (GetCharABCWidths(_dc, (UINT)'M', (UINT)'M', &abc)) {
    _charwidth = (int)(abc.abcA + abc.abcB + abc.abcC);
  } else {
    GetCharWidth32(_dc, (UINT)'M', (UINT)'M', &_charwidth);
  }

  TTYncol = (int)(nParentWidth  / _charwidth);
  TTYnrow = (int)(nParentHeight / _charheight) - 1;
  TTYinit = true;

  opt::line_number_mode = true;
}

void
XpTerminal::xpchangefont(int font) {

  if (_openp && display->running()) {
    delete display;
    xpsetfontsize(font);
    xpsettextattrib();
    if (TTYnrow <= 1) {
      TTYnrow = 2;
    }
    display = new DISPLAY;
    (void)WINSCR::resize();
    InvalidateRect(_wnd, nullptr, TRUE);
  } else {
    xpsetfontsize(font);
    xpsettextattrib();
  }
}

XpTerminal::XpTerminal() {
  static TCHAR  filename[XP_EMACS_MAXPATH];
  static TCHAR* filefilter =
    _T("C/C++ Files (*.c, *.cpp, *.cc)\0*.c;*.cpp;*.cc;*.h;*.hpp\0")
    _T("C# Files (*.cs)\0*.cs\0")
    _T("Java Files (*.java*)\0*.l*\0")
    _T("Lisp Files (*.lsp, *.lisp, *.l*)\0*.lsp;*.lisp;*.l*\0")
    _T("Shell Files (*.sh)\0*.sh\0")
    _T("Text Files (*.txt)\0*.txt\0")
    _T("TDB Files (*.tdb)\0*.tdb\0")
    _T("All Files (*.*)\0*.*\0");

  tt = this;

//#if !defined(DEBUG_ON_CONSOLE)
  (void)AttachConsole(ATTACH_PARENT_PROCESS);
  (void)std::freopen("CON", "w", stdout);
//#endif

  /*
   * Set the Insert key of the keyboard to untoggle
   */

  {
    BYTE bKeyState[256]; /* MS says 256, so 256! */

    (void)GetKeyboardState((LPBYTE)&bKeyState);
    bKeyState[VK_INSERT] &= ~(-1);
    (void)SetKeyboardState((LPBYTE)&bKeyState);
  }

  if (opt::system_colors) {
    _bgcolor = GetSysColor(COLOR_WINDOW);
    _fgcolor = GetSysColor(COLOR_WINDOWTEXT);
  } else {
    _bgcolor = XpTerminal::_colortable[opt::background_color];
    _fgcolor = XpTerminal::_colortable[opt::foreground_color];
  }

  _brush = CreateSolidBrush(_bgcolor);

  auto hIcon = LoadIcon(_inst, MAKEINTRESOURCE(EMACTICON));

  if (_prev == nullptr) {
    WNDCLASS wc;

    wc.style         = XP_EMACS_CS;
    wc.lpfnWndProc   = (WNDPROC)xpmainwndproc;
    wc.cbClsExtra    = 0;
    wc.cbWndExtra    = 0;
    wc.hInstance     = _inst;
    wc.hIcon         = hIcon;
    wc.hCursor       = LoadCursor(0, IDC_ARROW);
    wc.lpszMenuName  = nullptr; /* XP_EMACS_MENU */
    wc.hbrBackground = _brush;
    wc.lpszClassName = XP_EMACS_CLASS;

    if (RegisterClass(&wc) == 0) {
      xpprinterror(_T("Can't register class"), TRUE);
      return;
    }
  } else {
    xpprinterror(_T("xpopen1"), FALSE);
  }

  _menu = LoadMenu(_inst, MAKEINTRESOURCE(EMACTMENU));

  if (_menu == nullptr) {
    xpprinterror(_T("Can't load menu"), TRUE);
    return;
  }

  _wnd = CreateWindowEx(WS_EX_ACCEPTFILES,
                        XP_EMACS_CLASS,
                        XP_EMACS_APPNAME,
                        WS_OVERLAPPEDWINDOW,
                        CW_USEDEFAULT,
                        CW_USEDEFAULT,
                        CW_USEDEFAULT,
                        CW_USEDEFAULT,
                        (HWND)0,
                        (HMENU)0,
                        _inst,
                        nullptr);

  if (_wnd == nullptr) {
    xpprinterror(_T("Can't create Window"), TRUE);
    return;
  }

  if (opt::show_menu) {
    SetMenu(_wnd, _menu);
  }

  _dc = GetDC(_wnd);

  /*
   *      Create a fixed font.
   */

  xpsetfontsize(XP_EMACS_DEFAULT_FONT_SIZE);

  (void)SelectObject(_dc, _brush);
  (void)SelectObject(_dc, _font);

  xpsettextattrib();

  if (_wnd) {
    SetClassLongPtr(_wnd, GCLP_HICON, (LONG)(LONG_PTR)hIcon);
  }

  _drawx    = 0;
  _drawy    = 0;
  _drawxpos = 0;

  SetBkColor(_dc, _bgcolor);
  SetTextColor(_dc, _fgcolor);

  /*
   *      fill in non-variant fields of OPENFILENAME struct.
   */

  xpofn.lStructSize       = sizeof(OPENFILENAME);
  xpofn.hwndOwner         = nullptr;
  xpofn.hInstance         = nullptr;
  xpofn.lpstrFilter       = filefilter;
  xpofn.lpstrCustomFilter = nullptr;
  xpofn.nMaxCustFilter    = 0;
  xpofn.nFilterIndex      = 1;
  xpofn.lpstrFile         = filename;
  xpofn.nMaxFile          = XP_EMACS_MAXPATH;
  xpofn.lpstrFileTitle    = nullptr;
  xpofn.nMaxFileTitle     = 0;
  xpofn.lpstrTitle        = nullptr;
  xpofn.lpstrDefExt       = _T("lsp");
  xpofn.Flags             = OFN_EXPLORER     | OFN_ENABLESIZING    |
                            OFN_HIDEREADONLY | OFN_FORCESHOWHIDDEN;

  /*
   * Set clipborad functions.
   */

  widget.w_clipcopy       = xpclipcopy;
  widget.w_clippaste      = xpclippaste;
  widget.w_title          = xptitle;
  widget.w_wait           = xpwait;
  widget.w_print          = xpprint;

  xpsettextattrib();

  /*
   * Set the window on the left, taking care of icons
   */

  {
    RECT rc;
    SystemParametersInfo(SPI_GETWORKAREA, 0, &rc, 0);

    SetWindowPos(_wnd,
                 HWND_TOP,
                 GetSystemMetrics(SM_CXICONSPACING),
                 0,
                 _charwidth * (80 + 1),
                 rc.bottom - rc.top,
                 SWP_NOZORDER | SWP_NOACTIVATE);
  }

  xpsettextattrib();

  (void)ShowWindow(_wnd, _cmdshow);

  if (UpdateWindow(_wnd) == 0) {
    xpprinterror(_T("Can't update window"), TRUE);
  }

  DISPLAY::_mouse = true;
  _openp = true;
}

void
XpTerminal::xpfindreplace(bool replacep) {
  static FINDREPLACE find;
  static bool firstinit{false};
  static EMCHAR szFind[NLINE];
  static EMCHAR szReplace[NLINE];

  if (firstinit == false) {
    (void)std::memset(&find, 0, sizeof(FINDREPLACE));
    find.lStructSize      = sizeof(FINDREPLACE);
    find.hwndOwner        = _wnd;
    find.hInstance        = _inst;
    find.lpstrFindWhat    = szFind;
    find.wFindWhatLen     = NLINE;
    find.lpstrReplaceWith = szReplace;
    find.wReplaceWithLen  = NLINE;
    find.Flags            = FR_HIDEWHOLEWORD|FR_DOWN|FR_MATCHCASE;
    firstinit               = true;
  }

  if (replacep) {
    xpcurdlg = ReplaceText(&find);
  } else {
    xpcurdlg = FindText(&find);
  }
}

int
XpTerminal::xpfindmessage(LPFINDREPLACE lpfr) {
  extern Point found;

  auto dwFlags   = lpfr->Flags;
  auto replaced  = 0;
  auto nFindFlag = true;
  auto szFind    = lpfr->lpstrFindWhat;
  auto szReplace = lpfr->lpstrReplaceWith;

  Editor::_lastflag = CFFSRC;
  opt::case_sensitivity = (dwFlags & FR_MATCHCASE) != 0;

  tt->cshow(false);
  (void)emstrcpy(Editor::searchBuffer(), szFind);

  if (dwFlags & FR_DIALOGTERM) {
    xpcurdlg = nullptr;
    return 0;
  }

  if ((dwFlags & FR_FINDNEXT) && nFindFlag) {
    if (dwFlags & FR_DOWN) {
      nFindFlag = (forwsearch() == T);
    } else {
      nFindFlag = (backsearch() == T);
    }
  }

  if ((dwFlags & FR_REPLACE) && nFindFlag) {
    curwp->setDot(found);
    curwp->setFlags(WINSCR::WFHARD);
    subst((int)emstrlen(szFind), szReplace);
    replaced++;
    if (dwFlags & FR_DOWN) {
      nFindFlag = (forwsearch() == T);
    } else {
      nFindFlag = (backsearch() == T);
    }
  }

  if ((dwFlags & FR_REPLACEALL) && nFindFlag) {
    do {
      int len = (int)emstrlen(szFind);

      curwp->setDot(found);
      curwp->setFlags(WINSCR::WFHARD);
      subst(len, szReplace);
      replaced++;
    } while (ffindstring());
  }

  display->update();
  if (replaced) {
    WDGwrite(ECSTR("Replaced %d occurence(s)"), replaced);
  }
  tt->cshow(true);
  return 0;
}

XpTerminal::~XpTerminal() {
  DeleteObject(_brush);
  DeleteObject(_font);
  ReleaseDC(_wnd, _dc);
  DestroyWindow(_wnd);
  PostQuitMessage(0);
}

void
XpTerminal::insert(const TCHAR* szText, int nSize) {
  SIZE size;

  ExtTextOut(_dc,
             XP_DRAW_X,
             XP_DRAW_Y,
             ETO_CLIPPED,
             nullptr,
             &szText[0],
             nSize,
             nullptr);

  GetTextExtentPoint32(_dc, szText, nSize, &size);

  _drawx    += nSize;
  _drawxpos += size.cx;
}

void
XpTerminal::insert(int aChar) {
  TCHAR   c[] = { (TCHAR)'?', (TCHAR)0 };

  c[0] = (TCHAR)aChar;

  insert((TCHAR *)&c, 1);
}

/*
 *      Find actual buffer position from mouse pointer.
 */

static int
xpposfrompoint(int x, int y) {
#if     defined(_UNICODE)
  auto lpString = display->text(y);

  for (int i = 0; i < TTYncol; i++) {
    SIZE size;
    GetTextExtentPoint32(XpTerminal::_dc, lpString, i, &size);
    if (size.cx > (x * XpTerminal::_charwidth)) {
      return i - 1;
    }
  }
#endif
  (void)y;
  return x;
}

void
XpTerminal::move(int row, int col) {
#if     defined(_UNICODE)
  auto lpString(display->text(row));
  SIZE size;

  GetTextExtentPoint32(_dc, lpString, col, &size);

  _drawxpos = size.cx;
#endif
  _drawx    = col;
  _drawy    = row;
}

#define CTRLP   (GetKeyState(VK_CONTROL) < 0)

int
XpTerminal::xpdecodechar(int c) {
  int aChar = XP_NO_CHAR;

  switch (c) {
  case VK_UP    : aChar = Ctrl|'P'; break;
  case VK_DOWN  : aChar = Ctrl|'N'; break;
  case VK_DELETE: aChar = Ctrl|'D'; break;
  case VK_NEXT  : aChar = Ctrl|'V'; break;
  case VK_INSERT: aChar = META|'I'; break;
  case VK_END   : aChar = CTRLP?(CTLX|Ctrl|'N'):(META|'>'); break;
  case VK_HOME  : aChar = CTRLP?(CTLX|Ctrl|'P'):(META|'<'); break;
  case VK_PRIOR : aChar = META|'V'; break;
  case VK_LEFT  : aChar = CTRLP?(META|'B'):(Ctrl|'B'); break;
  case VK_RIGHT : aChar = CTRLP?(META|'F'):(Ctrl|'F'); break;
  case VK_F1    : xphelp(_wnd); break;
  case VK_F2    : aChar = Ctrl|'S'; break;
  case VK_F3    : aChar = META|Ctrl|'F'; break;
  case VK_F4    : aChar = CTLX|Ctrl|'I'; break;
  case VK_F5    : break; /* switchscreen(); */
  case VK_F6    : aChar = CTLX|METACH; break;
  case VK_F7    : aChar = CTLX|'`'; break;
  case VK_F8    : aChar = CTLX|Ctrl|'S'; break;
  case VK_F9    : aChar = CTLX|'E'; break;
#if     defined(_UNICODE)
  case VK_F12   : aChar = xpunicodegen(); break;
#endif
  }

  return aChar;
}

int
XpTerminal::get() {
  int aChar = XP_NO_CHAR;
  MSG msg;

  /*
   *      Check if a char is in clipboard
   */

  if (_char != XP_NO_CHAR) {
    return _char;
  }

  /*
   *      Wait for a char (or a mouse event)
   */

  while (aChar == XP_NO_CHAR && _openp) {
    this->cshow(true);

    if (GetMessage(&msg, (HWND)0, 0, 0) == 0) {
      return (int)msg.wParam;
    }

    if (xpcurdlg && IsDialogMessage(xpcurdlg, &msg)) {
      /*
       * Process Dialog Box
       */
      continue;
    }

    if (TranslateAccelerator(msg.hwnd, _acctable, &msg)) {
      continue;
    }

    TranslateMessage(&msg);
    DispatchMessage(&msg);

    if (_char != XP_NO_CHAR) {
      aChar = _char;
      _char = XP_NO_CHAR;
    }
  }

  this->cshow(false);

  if (opt::mouse_avoidance_mode) {
    if ((aChar != MEVT) && (++_strokes % opt::mouse_avoidance_nudge) == 0) {
      /*
       * every 3 strokes, move mouse to the bottom
       */
      xpmousebottom();
    }
  }

  return aChar;
}

void
XpTerminal::eeop() {
  RECT rcClear;

  GetClientRect(_wnd, (LPRECT)&rcClear);
  FillRect(_dc, (LPRECT)&rcClear, _brush);
}

void
XpTerminal::eeol() {
  RECT rcClear;

  rcClear.top    = _drawy * _charheight;
  rcClear.bottom = (_drawy + 1) * _charheight;
  rcClear.left   = XP_DRAW_X;
  rcClear.right  = t_ncol * _charwidth;

  FillRect(_dc, (LPRECT)&rcClear, _brush);
}

void
XpTerminal::si() {
  if (_fgcolor == XPCOLOR_CYAN) {
    SetBkColor(_dc, XPCOLOR_WHITE);
  } else {
    SetBkColor(_dc, _fgcolor);
  }
  SetTextColor(_dc, _bgcolor);
}

void
XpTerminal::ei() {
  SetBkColor(_dc, _bgcolor);
  SetTextColor(_dc, _fgcolor);
}

void
XpTerminal::flush() {
}

void
XpTerminal::beep() {
  /*
   *      Beep(Frequecy, Duration).
   *      Parameters are ignored on Windows 95.
   */

#if     defined(XP_EMACS_SOUND_CARD_BEEP)
  MessageBeep((UINT)-1);
#else
  Beep(0x333, 0x10);
#endif
}

void
XpTerminal::cshow(bool nFlag) {
#if defined(XP_HARDWARE_CARET)
  if (nFlag != NIL) {
    CreateCaret(_wnd, (HBITMAP)nullptr, _charwidth, _charheight);
    (void)SetCaretPos(XP_DRAW_X, XP_DRAW_Y);
    ShowCaret(_wnd);
  } else {
    DestroyCaret();
    HideCaret(_wnd);
  }
  _cursoron = nFlag;
#else
  if (nFlag != _cursoron) {
    _cursoron = nFlag;
  } else {
    return;
  }

  if ((IsIconic(_wnd) != FALSE)) {
    return;
  }

  auto x = XP_DRAW_X;
  auto y = XP_DRAW_Y;

  SetBkColor(_dc, nFlag ? _fgcolor : _bgcolor);
  SetTextColor(_dc, nFlag ? _bgcolor : _fgcolor);
  ExtTextOut(_dc, x, y, ETO_CLIPPED, nullptr, &DISPLAY::_curchar, 1, nullptr);
  SetBkColor(_dc, _bgcolor);
  SetTextColor(_dc, _fgcolor);
#endif
}

void
XpTerminal::xpsetfontsize(int size) {
#if     defined(XP_EMACS_CREATE_FONT)
  LOGFONT lf;

  /*
   *      Create a fixed font.
   */

  if (_font) {
    DeleteObject(_font);
  }

  (void)std::memset((char *)&lf, 0, sizeof(lf));
  lf.lfPitchAndFamily = FIXED_PITCH | FF_MODERN;
  lf.lfHeight         = -MulDiv(size, GetDeviceCaps(_dc, LOGPIXELSY), 72);
  lf.lfWeight         = FW_BOLD;
  lf.lfOutPrecision   = OUT_DEFAULT_PRECIS;
  lf.lfClipPrecision  = CLIP_DEFAULT_PRECIS;
  _font              = CreateFontIndirect(&lf); /* SYSTEM_FONT */

  if (_font == nullptr) {
    xpprinterror(_T("Can't create Font"), FALSE);
  }

  if (_dc != nullptr) {
    (void)SelectObject(_dc, _font);
  }
#else
  if (size == XP_EMACS_DEFAULT_FONT_SIZE) {
    _font = (HFONT)GetStockObject(SYSTEM_FIXED_FONT);
  } else {
    _font = (HFONT)GetStockObject(ANSI_FIXED_FONT);
  }

  if (_dc != nullptr) {
    (void)SelectObject(_dc, _font);
  }
#endif
}


void
XpTerminal::xpprinterror(TCHAR *txt, BOOL bexit) {
  DWORD err = GetLastError();
  TCHAR msg[256];

  (void)wsprintf(msg, _T("ERROR ID # %d, %s"), err, txt);
  MessageBox(nullptr, msg, nullptr, MB_OK);

  if (bexit) {
    exit(0);
  }
}

bool
XpTerminal::check() {
  MSG msg;

  /*
   *      Process messages intended for the abort dialog box
   */

  while (PeekMessage(&msg, nullptr, FALSE, FALSE, TRUE)) {
    if (TranslateAccelerator(msg.hwnd, _acctable, &msg)) {
      continue;
    }
    TranslateMessage(&msg);
    switch (msg.message) {
    case WM_CHAR :
      if (msg.wParam == 0x07) {
        /*
         * Ctrl-G
         */
        return false;
      }
    }
    DispatchMessage(&msg);
  }

  return true;
}

CMD
switchscreen() {
  return T;
}

static int
xpquit() {
  static auto warning(_T("Modified buffer exist, do you really want to exit?"));

  if (opt::confirm_unsaved_buffer) {
    int ret;

    if (!BUFFER::anycb(BUFFER::ANYCB::CHECK)) {
      (void)killemacs();
      return TRUE;
    }

    ret = MessageBox(nullptr,
                     warning,
                     _T("EmACT"),
                     MB_ICONQUESTION | MB_APPLMODAL | MB_TOPMOST |MB_YESNO);

    if (ret == IDYES) {
      XpTerminal::_openp = false;
      (void)killemacs();
      return TRUE;
    } else {
      return FALSE;
    }
  } else {
    XpTerminal::_openp = false;
    (void)killemacs();
    return TRUE;
  }
}

static LRESULT APIENTRY
xpmainwndproc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam) {
  PAINTSTRUCT ps;

  switch (message) {
  case WM_COMMAND:
    switch (wParam) {
    case IDM_LOAD:
      xpofn.lpstrInitialDir = nullptr;
      if (GetOpenFileName((LPOPENFILENAME)&xpofn) != 0) {
        (void)newfile(xpofn.lpstrFile);
        display->update();
      }
      break;
    case IDM_PRINT:
      xpprint();
      break;
    case IDM_QUIT:
      return xpquit();
    case IDM_STANDARDFONT:
      XpTerminal::xpchangefont(XP_EMACS_STANDARD_FONT_SIZE);
      break;
    case IDM_ALTERNATEFONT:
      XpTerminal::xpchangefont(XP_EMACS_ALTERNATE_FONT_SIZE);
      break;
    case IDM_UTF8ENCODING:
      utf8encoding();
      display->update();
      break;
    case IDM_UTF16ENCODING:
      utf16encoding();
      display->update();
      break;
    case IDM_DEFAULTENCODING:
      systemencoding();
      display->update();
      break;
    case IDM_HELPEMACS:
      xphelp(XpTerminal::_wnd);
      break;
    case IDM_ABOUT:
      DialogBox(XpTerminal::_inst, _T("AboutBox"), hWnd, (DLGPROC)xpabout);
      break;
    case IDM_CUT:
      killregion();
      display->update();
      break;
    case IDM_COPY:
      copyregion();
      break;
    case IDM_PASTE:
      yank();
      display->update();
      break;
    case IDM_FIND:
      XpTerminal::xpfindreplace(false);
      break;
    case IDM_REPLACE:
      XpTerminal::xpfindreplace(true);
      break;
    default :
      return DefWindowProc(hWnd, message, wParam, lParam);
    }
    return TRUE;
  case WM_ACTIVATE:
    if (hWnd != XpTerminal::_wnd) {
      break;
    }

    if (opt::system_colors) {
      break;
    }

    if (XpTerminal::_bgcolor == XPCOLOR_DARK_BLUE) {
      if (LOWORD(wParam) == WA_INACTIVE) {
        XpTerminal::_fgcolor = XPCOLOR_DARK_CYAN;
      } else {
        XpTerminal::_fgcolor = XpTerminal::_colortable[opt::foreground_color];
      }

      display->update(DISPLAY::Mode::REFRESH);
      InvalidateRect(XpTerminal::_wnd, (LPRECT)nullptr, TRUE);
      UpdateWindow(XpTerminal::_wnd);
    }

    break;
  case WM_SIZE:
    if (XpTerminal::_openp && display->running()
        && !IsIconic(XpTerminal::_wnd)) {
      delete display;
      XpTerminal::xpsettextattrib();
      if (TTYnrow <= 1) {
        TTYnrow = 2;
      }
      display = new DISPLAY;
      (void)WINSCR::resize();
      display->update(DISPLAY::Mode::REFRESH);
      InvalidateRect(XpTerminal::_wnd, nullptr, TRUE);
      UpdateWindow(XpTerminal::_wnd);
    }
    break;
  case WM_CREATE:
    /*
     *      Create the objects
     */
    break;
  case WM_CLOSE :
    return xpquit();
  case WM_QUERYENDSESSION :
    return xpquit();
  case WM_PAINT :
    tt->cshow(FALSE);
    BeginPaint(hWnd, &ps);
    if (XpTerminal::_openp) {
      if (DISPLAY::_sgarbf != DISPLAY::Sync::GARBAGE) {
        DISPLAY::exposed();
      }
      display->update();
    }
    EndPaint(hWnd, &ps);
    break;
  case WM_KEYDOWN :
    XpTerminal::_char = XpTerminal::xpdecodechar((int)wParam);
    break;
  case WM_CHAR :
    if (CTRLP && ((int)wParam) == ' ') {
      /* CTRL-SPACE: it's a mark set */
      XpTerminal::_char = 0;
    } else {
      XpTerminal::_char = (int)wParam;
    }
    break;
  case WM_LBUTTONDOWN:
  case WM_RBUTTONDOWN:
  case WM_MBUTTONDOWN:
    mevent.x      = LOWORD(lParam) / XpTerminal::_charwidth;
    mevent.y      = HIWORD(lParam) / XpTerminal::_charheight;
    mevent.x      = xpposfrompoint(mevent.x, mevent.y);
    mevent.button = (message == WM_LBUTTONDOWN)
                    ? MEvent::MButton1
                    : MEvent::MButton2;
    if (GetKeyState(VK_SHIFT) < 0) {
      mevent.button |= MEvent::SHIFTBUTTON;
    }
    if (GetKeyState(VK_CONTROL) < 0) {
      mevent.button |= MEvent::CTRLBUTTON;
    }
    XpTerminal::_char = MEVT;
    break;
  case WM_MOUSEWHEEL :
    if ((short)HIWORD(wParam) < 0) { /* zDelta */
      (void)forwpage();
    } else {
      (void)backpage();
    }
    display->update();
    break;
  case WM_DROPFILES: {
    TCHAR szDragFile[NFILEN];
    DragQueryFile((HDROP)wParam, 0, (TCHAR *)&szDragFile[0], NFILEN);
    (void)newfile(szDragFile);
    display->update();
    break;
  }
  default:
    if (message == XpTerminal::_loadmsg) {
      if (lParam) {
        (void)newfile((TCHAR *)lParam);
        display->update();
      }
      return (LRESULT)0;
    } else if (message == XpTerminal::_findmsg) {
      FINDREPLACE* lpfr = (FINDREPLACE *)lParam;
      XpTerminal::xpfindmessage(lpfr);
      return (LRESULT)0;
    }
    return DefWindowProc(hWnd, message, wParam, lParam);
  }

  return (LRESULT)0;
}

static LRESULT APIENTRY
xpabout(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam) {
  RECT    aRec;

  (void)lParam;

  switch (message) {
  case WM_INITDIALOG:
#if defined(XP_EMACS_CENTERED_ABOUT)
    GetWindowRect(hDlg, &aRec);
    OffsetRect(&aRec, -((int)aRec.left), -((int)aRec.top));
    MoveWindow(hDlg,
               ((GetSystemMetrics(SM_CXSCREEN)-aRec.right)/2+4)&~7,
               (GetSystemMetrics(SM_CYSCREEN)-aRec.bottom)/2,
               (int)aRec.right,
               (int)aRec.bottom,
               0);
#else
    (void)aRec;
#endif
    return (LRESULT)TRUE;

  case WM_COMMAND:
    if ((LOWORD(wParam) == IDOK) || (LOWORD(wParam) == IDCANCEL)) {
      EndDialog(hDlg, LOWORD(wParam));
      return TRUE;
    }
    break;
  }

  return FALSE;
}

void
XpTerminal::xpmousebottom() {
  RECT rect;

  GetWindowRect(_wnd, &rect);
  SetCursorPos(rect.right - _charwidth, rect.bottom - _charheight);
}

static  void
xphelp(HWND hwnd) {
  WinHelp(hwnd, _T("emacs.hlp"), HELP_INDEX, 0L);
}

static TCHAR*
xptitle(TCHAR* buf, TCHAR* fname) {
  static TCHAR title[64] = { 0 };

  /*
   * The next two lines remove warning on args not used !!
   */

  if (buf != fname) {
    fname = buf;
  }

  if (_tcscmp(buf, title) != 0) {
    (void)_stprintf(title, _T("%s (%dx%d)"), fname, TTYnrow, TTYncol);
    SetWindowText(XpTerminal::_wnd, title);
  }

  return buf;
}

static void
xpcutcopy(WPARAM wParam) {
  UINT    AllocFlags = GMEM_MOVEABLE|GMEM_DDESHARE;
  HANDLE  hClipData;
  LPTSTR  lpClipData;
  auto    kbuf = kget();
  size_t  size = (kbuf.second + 1) * sizeof (EMCHAR);

  /*
   * After  SetClipboardData  is  called,  the  system owns the
   * object   identified   by   the  hClipData  parameter.  The
   * application  can  read  the  data,  but  must not free the
   * handle  or  leave  it  locked.  If the hClipData parameter
   * identifies  a  memory  object,  the  object must have been
   * allocated   using   the   GlobalAlloc  function  with  the
   * GMEM_MOVEABLE and GMEM_DDESHARE flags.
   */

  if (kget().second == 0) {
    return;
  }

  /*
   *      Allocate memory and copy the string to it
   */

  if ((hClipData = GlobalAlloc(AllocFlags, size)) == nullptr) {
    return;
  }

  /*
   *      Copy data
   */

  lpClipData = (LPTSTR)GlobalLock(hClipData);

  if (lpClipData == nullptr) {
    return;
  }

  (void)_tcsncpy(lpClipData, kbuf.first, kbuf.second);
  lpClipData[kbuf.second] = 0;

  GlobalUnlock(hClipData);

  /*
   *      Clear the current contents of the clipboard, and set
   *      the data handle to the new string.
   */

  if (OpenClipboard(XpTerminal::_wnd) == 0) {
    WDGerror(_T("Can't open clipboard."));
  } else {
    EmptyClipboard();
#if     defined(_UNICODE)
    if (SetClipboardData(CF_UNICODETEXT, hClipData) == nullptr) {
      WDGerror(_T("Can't set UNICODE clipboard data."));
    }
#else
    if (SetClipboardData(CF_TEXT, hClipData) == nullptr) {
      WDGerror(_T("Can't set clipboard data."));
    }
#endif
    CloseClipboard();
  }

  if (wParam == IDM_CUT) {
    (void)killregion();
    EnableMenuItem(GetMenu(XpTerminal::_wnd), IDM_CUT,  MF_GRAYED);
    EnableMenuItem(GetMenu(XpTerminal::_wnd), IDM_COPY, MF_GRAYED);
  }

  GlobalUnlock(hClipData);
}

static void
xpclipcopy() {
  xpcutcopy(WM_COPY);
}

static void
xpclippaste() {
#if     defined(_UNICODE)
  UINT fmt[] = { CF_UNICODETEXT, CF_OEMTEXT, CF_TEXT, CF_DSPTEXT };
#else
  UINT fmt[] = { CF_OEMTEXT, CF_TEXT, CF_UNICODETEXT, CF_DSPTEXT };
#endif
  HANDLE  hClipData  = (HANDLE)0;  // handles to clip data
  LPTSTR  lpClipData = nullptr;    // Clip Data

  if (OpenClipboard(XpTerminal::_wnd) == FALSE) {
    WDGerror(_T("Can't find clipboard data"));
    return;
  }

  /*
   * get text from the clipboard
   */

  int type = -1;             /* Data type                    */
  for (int i = 0; i < (sizeof(fmt) / sizeof(fmt[0])); ++i) {
    if ((hClipData = GetClipboardData(fmt[i])) != 0) {
      type = fmt[i];
      break;
    }
  }

  if (type == -1) {
    WDGerror(_T("Can't find a valid clipboard data."));
    CloseClipboard();
    return;
  }

  if (hClipData == (HANDLE)0) {
    WDGerror(_T("Can't find clipboard data"));
    CloseClipboard();
    return;
  }

  if ((lpClipData = (LPTSTR)GlobalLock(hClipData)) == nullptr) {
    WDGerror(_T("No memory left, file may be corrupted."));
    CloseClipboard();
    return;
  }

#if 0
  if (type == CF_OEMTEXT) {
    OemToAnsi((LPSTR)lpClipData, (LPSTR)lpClipData);
  }
#endif

  kdelete();

  for (auto s = lpClipData; *s; ++s) {
    if (*s != '\r') {
      (void)kinsert((int)*s);
    }
  }

  GlobalUnlock(hClipData);
  CloseClipboard();
}

CMD
NTansitooem(EDLINE* lp) {
#if !defined(_UNICODE)
  CharToOemBuff(lp->text(), lp->text(), lp->length());
#else
  (void)lp;
#endif
  return T;
}

CMD
NToemtoansi(EDLINE* lp) {
#if !defined(_UNICODE)
  OemToCharBuff(lp->text(), lp->text(), lp->length());
#else
  (void)lp;
#endif
  return T;
}

/*
 *      Calling external command
 */

int
system(const TCHAR* s) {
  TCHAR   cmd[512];
  TCHAR*  slwr;
  int     internal;
  int    res;
  static constexpr TCHAR* szDosCmd[] = {
    _T("cd"),     _T("cls"),    _T("copy"),   _T("date"),
    _T("del"),    _T("erase"),  _T("dir"),    _T("echo"),
    _T("for"),    _T("if"),     _T("md"),     _T("mkdir"),
    _T("pause"),  _T("ren"),    _T("rd"),     _T("rmdir"),
    _T("set"),    _T("time"),   _T("type"),   _T("ver"),
    _T("vol"),    _T(">"),      _T("<"),      _T("|"),
    nullptr
  };

  internal = FALSE;
  for (slwr = const_cast<TCHAR*>(s);
       *slwr && !std::isspace((int)*slwr);
       ++slwr) {
    if (std::isupper((int)*slwr)) {
      *slwr = (TCHAR)std::tolower((int)*slwr);
    }
  }

  slwr = (TCHAR*)s;

  for (int i = 0; szDosCmd[i]; i++) {
    if (_tcsncmp(s, szDosCmd[i], _tcslen(szDosCmd[i])) == 0) {
      internal = TRUE;
      break;
    }
  }

  if (internal) {
    _tcscpy(cmd, _tgetenv(_T("COMSPEC")));
    _tcscat(cmd, _T(" /c "));
    _tcscat(cmd, slwr);
  } else {
    _tcscpy(cmd, slwr);
  }

  auto hCurOld = SetCursor((HCURSOR)LoadCursor(nullptr, IDC_WAIT));

  ShowCursor(TRUE);

#if     defined(XP_EMACS_OLD_EXEC)
  res = (WinExec((TCHAR *)cmd, SW_SHOWNORMAL) > 31);
#else
  res = XpTerminal::xpsystemspawn((TCHAR *)slwr);
  if (res == TRUE) {
    res = 0; /* unix system returns 0 on success */
  }
#endif

  ShowCursor(FALSE);
  SetCursor(hCurOld);

  return res;
}

void
XpTerminal::xpshowlines(TCHAR* chBuf, int nLen) {
  auto s(chBuf);

  while (*s && nLen-- > 0) {
    const TCHAR c{*s++};
    switch (c) {
    case '\r' :
      display->update();
      break;
    case '\n':
      display->update();
      (void)newline();
      break;
    default:
      (void)EDLINE::linsert(c);
    }
  }
}

int
XpTerminal::xpsystemspawn(const TCHAR* cmd) {
  SECURITY_ATTRIBUTES saAttr;
  PROCESS_INFORMATION piProcInfo;
  STARTUPINFO         siStartInfo;
  HANDLE              hChildStdoutRd = (HANDLE)0;
  HANDLE              hChildStdoutWr = (HANDLE)0;
  HANDLE              hOutputFile;
  DWORD               dwRead;
  char                chBuf[NLINE + 1];
  TCHAR               buf[NFILEN + 1];
  BOOL                bSuccess;
  DWORD               nRetCode = TRUE;
#if     defined(_GET_EXIT_CODE)
  DWORD               lpdwExitCode;
#endif

  saAttr.nLength              = sizeof(SECURITY_ATTRIBUTES);
  saAttr.bInheritHandle       = TRUE;
  saAttr.lpSecurityDescriptor = nullptr;

  /* Set up members of STARTUPINFO structure. */

  if (opt::pipe_process) {
    if (!CreatePipe(&hChildStdoutRd, &hChildStdoutWr, &saAttr, 0)) {
      XpTerminal::xpprinterror(_T("Stdout pipe creation failed\n"), FALSE);
      return FALSE;
    }
    hOutputFile = hChildStdoutWr;
  } else {
    hOutputFile = CreateFile(_T("process.tmp"),
                             GENERIC_WRITE,
                             0,
                             &saAttr,
                             CREATE_ALWAYS,
                             0,
                             nullptr);

    if (hOutputFile == INVALID_HANDLE_VALUE) {
      XpTerminal::xpprinterror(_T("Can't open output file\n"), FALSE);
    }
  }

  siStartInfo.cb          = sizeof(STARTUPINFO);
  siStartInfo.lpReserved  = nullptr;
  siStartInfo.lpReserved2 = nullptr;
  siStartInfo.cbReserved2 = 0;
  siStartInfo.lpDesktop   = nullptr;
  siStartInfo.lpTitle     = (TCHAR *)cmd;
  siStartInfo.wShowWindow = SW_HIDE;
  siStartInfo.dwFlags     = STARTF_USESTDHANDLES | STARTF_USESHOWWINDOW;
  siStartInfo.hStdOutput  = hOutputFile;
  siStartInfo.hStdError   = hOutputFile;
  siStartInfo.hStdInput   = nullptr;

  /*
   * Create the child process.
   */

  _tcscpy(buf, _tgetenv(_T("COMSPEC")));
  _tcscat(buf, _T(" /c "));
  _tcscat(buf, cmd);

  bSuccess = CreateProcess(
    nullptr,
    (TCHAR *)buf,  /* command line                       */
    &saAttr,       /* process security attributes        */
    &saAttr,       /* primary thread security attributes */
    TRUE,          /* handles are inherited              */
    0,             /* creation flags                     */
    nullptr,       /* use parent's environment           */
    nullptr,       /* use parent's current directory     */
    &siStartInfo,  /* STARTUPINFO pointer                */
    &piProcInfo    /* receives PROCESS_INFORMATION       */
    );

  if (bSuccess != TRUE) {
    CloseHandle(hOutputFile);
    return FALSE;
  }

  /*
   * Change the priority from IDLE to NORMAL
   */

  SetPriorityClass(piProcInfo.hProcess, NORMAL_PRIORITY_CLASS);

  /*
   * Close the write end of the pipe before reading from the read end
   */

  if (opt::pipe_process) {
    if (!CloseHandle(hChildStdoutWr)) {
      XpTerminal::xpprinterror(_T("Closing handle failed"), FALSE);
    }

    xpprocess_pending = true;
    for (;;) {
      dwRead = 0;
      if (tt->check() != true) {
        if (!TerminateProcess(piProcInfo.hProcess, 0)) {
          XpTerminal::xpprinterror(_T("TerminateProcess fails"), FALSE);
          nRetCode = FALSE;
        }
        nRetCode = FALSE;
        break;
      }
      bSuccess = ReadFile(hChildStdoutRd,
                          chBuf,
                          NLINE,
                          &dwRead,
                          nullptr);

#if defined(_UNICODE)
      if (bSuccess && dwRead > 0) {
        int     k;
        TCHAR   tchBuf[NLINE + 1];
        char *  s = (char *)chBuf;
        for (k = 0; k < (int)dwRead && k < NLINE; ++k) {
          tchBuf[k] = (TCHAR)s[k];
        }
        tchBuf[k] = (EMCHAR)'\000';

        xpshowlines(tchBuf, k);
      }
#else
      if (bSuccess && dwRead > 0) {
        xpshowlines(chBuf, dwRead);
      }
#endif
      if (bSuccess == FALSE || dwRead == 0) {
        dwRead = GetLastError();
        break;
      }
    }
    xpprocess_pending = false;
    display->update();
  } else {
#if     defined(_GET_EXIT_CODE)
    lpdwExitCode = STILL_ACTIVE;
    while (lpdwExitCode == STILL_ACTIVE && bSuccess == TRUE) {
      bSuccess = GetExitCodeProcess(piProcInfo.hProcess, &lpdwExitCode);
      Sleep(500);
    }
#else
    WaitForSingleObject(piProcInfo.hProcess, INFINITE);
    CloseHandle(hOutputFile);
#endif
  }

  /* Release handles */
  CloseHandle(piProcInfo.hProcess);
  CloseHandle(piProcInfo.hThread);

  return nRetCode;
}

/*
 *      Printing
 */

static HDC
xpgetprinterdc() {
  PRINTDLG pd;
  (void)std::memset(&pd, 0, sizeof(pd));
  pd.lStructSize    = sizeof(pd);
  pd.Flags          = PD_RETURNDC | PD_NOSELECTION | PD_NOPAGENUMS;
  pd.nCopies        = 1;

  if (PrintDlg((LPPRINTDLG)&pd) == FALSE) {
    /*
     * cancelled by by user
     */
    return (HDC)((size_t)-1);
  }

  HDC hDC;
  if (pd.hDC) {
    hDC = pd.hDC;
  } else {
    if (!pd.hDevNames) {
      return (HDC)nullptr;
    }

    auto lpDevNames     = (LPDEVNAMES)GlobalLock(pd.hDevNames);
    auto lpszDriverName = (TCHAR *)lpDevNames + lpDevNames->wDriverOffset;
    auto lpszDeviceName = (TCHAR *)lpDevNames + lpDevNames->wDeviceOffset;
    auto lpszPortName   = (TCHAR *)lpDevNames + lpDevNames->wOutputOffset;
    GlobalUnlock(pd.hDevNames);

    LPDEVMODE lpDevMode = nullptr;

    if (pd.hDevMode) {
      lpDevMode = (LPDEVMODE)GlobalLock(pd.hDevMode);
    }

    hDC = CreateDC(lpszDriverName, lpszDeviceName, lpszPortName, lpDevMode);

    if (pd.hDevMode && lpDevMode) {
      GlobalUnlock(pd.hDevMode);
    }
  }

  if (pd.hDevNames) {
    GlobalFree(pd.hDevNames);
    pd.hDevNames = nullptr;
  }

  if (pd.hDevMode) {
    GlobalFree(pd.hDevMode);
    pd.hDevMode = nullptr;
  }

  return hDC;
}

#define XPLINENBSIZE    8

static  void
xpprint() {
  static auto const hHourGlass(LoadCursor(nullptr, IDC_WAIT));

  auto hSaveCursor = SetCursor(hHourGlass);
  /*
   * handle for printer device context
   */
  auto hPr(xpgetprinterdc());

  if (hPr == (HDC)((size_t)-1)) {
    /* cancelled */
    return;
  }

  if (hPr == nullptr) {
    MessageBox(nullptr, _T("Cannot print."), nullptr, MB_OK|MB_ICONHAND);
    return;
  }

  /*
   * Load the font "Courier New" size 8
   */

  /*
   * LOGFONT structure for the font
   */
  LOGFONT lfPr;
  (void)std::memset(&lfPr, 0, sizeof(lfPr));
  lfPr.lfHeight           = -MulDiv(8, GetDeviceCaps(hPr, LOGPIXELSY), 72);
  lfPr.lfWeight           = FW_LIGHT;
  lfPr.lfOutPrecision     = OUT_STRING_PRECIS;
  lfPr.lfClipPrecision    = CLIP_STROKE_PRECIS;
  lfPr.lfQuality          = DEFAULT_QUALITY;
  lfPr.lfPitchAndFamily   = FF_MODERN | FIXED_PITCH;

  emstrcpy(lfPr.lfFaceName, _T("Courier New"));

  /*
   *  font used for printing
   */
  auto hfPr = CreateFontIndirect(&lfPr);

  auto hOldFont = static_cast<HFONT>(SelectObject(hPr, hfPr));

  SetAbortProc(hPr, (ABORTPROC)nullptr);

  DOCINFO di;          /* Document info                        */
  (void)std::memset(&di, 0, sizeof(di));
  di.cbSize       = sizeof(di);
  di.lpszDocName  = curbp->filename();
  di.lpszOutput   = nullptr;
  di.lpszDatatype = nullptr;
  di.fwType       = 0;

  if (StartDoc(hPr, &di) < 0) {
    MessageBox(nullptr,
               _T("Unable to start print job"),
               nullptr,
               MB_OK | MB_ICONHAND);
    (void)SelectObject(hPr, hOldFont);
    DeleteObject(hfPr);
    DeleteDC(hPr);
  }

  SetCursor(hSaveCursor);      /* Remove the hourglass */

  /*
   * Get time in szTimeBuf and remove '\n'
   */

  std::time_t aclock;      /* current time */
  std::time(&aclock);
  auto szTimeBuf = _tasctime(std::localtime(&aclock));
  for (auto p = szTimeBuf; *p; ++p) {
    if (*p == '\n') {
      *p = '\000';
    }
  }

  /*
   * Since  you  may  have  more  than  one  line,  you need to
   * compute  the  spacing  between  lines.  You can do that by
   * retrieving  the  height of the characters you are printing
   * and  advancing  their height plus the recommended external
   * leading height.
   */

  int LineSpace;   /* spacing between lines                */
  {
    TEXTMETRIC TextMetric;  /* information about char size          */
    GetTextMetrics(hPr, &TextMetric);
    LineSpace = TextMetric.tmHeight + TextMetric.tmExternalLeading;
  }

  /*
   * Since  you  may  have more lines than can fit on one page,
   * you  need to compute the number of lines you can print per
   * page.  You can do that by retrieving the dimensions of the
   * page and dividing the height by the line spacing.
   */

  auto nPageWidth  = GetDeviceCaps(hPr, HORZRES);
  auto nPageLength = GetDeviceCaps(hPr, VERTRES);

  auto LinesPerPage = nPageLength / LineSpace - 1;

  LinesPerPage -= 2; /* 2 lines for header */

  /*
   * You can output only one  line at  a  time,  so you need a
   * count of the number of lines to print.   You can retrieve
   * the count sending the EM_GETLINECOUNT message to the edit
   * control.
   */

  /* Keep track of the current line on the current page */

  auto CurrentLine = 1;
  auto CurrentPage = 1;
  auto LineNumber  = 1;

  /*
   * For each line  you need to advance one line space.  Also,
   * you need to check for the end of the page and start a new
   * page if necessary.
   */

  auto Status = 0; // printing status

  for (auto clp = curbp->firstline();
       clp != curbp->lastline();
       clp = clp->forw()) {
    TCHAR buf[NLINE + XPLINENBSIZE];
    if (CurrentLine == 1) {
      StartPage(hPr);
      (void)emsprintf3(buf,
                       _T("Page #%00d, %s - %s"),
                       CurrentPage,
                       szTimeBuf,
                       curbp->filename());
      TabbedTextOut(hPr,
                    0,
                    CurrentLine * LineSpace,
                    (TCHAR *)buf,
                    (int)_tcslen(buf),
                    0,
                    nullptr,
                    0);
      MoveToEx(hPr, 0, LineSpace * 2, nullptr);
      LineTo(hPr, nPageWidth, LineSpace * 2);
    }

    (void)emsprintf1(buf, _T("%05d : "), LineNumber++);

    int len; // length of the current line.
    if (clp->length() >= NLINE) {
      len = NLINE - 1;
    } else {
      len = clp->length();
    }

    (void)_tcsncpy(buf + XPLINENBSIZE, clp->text(), len);

    buf[len + XPLINENBSIZE] = '\000';

    TabbedTextOut(hPr,
                  0,
                  (CurrentLine + 2) * LineSpace,
                  (TCHAR *)buf,
                  len + XPLINENBSIZE,
                  0,
                  nullptr,
                  0);

    if ((++CurrentLine % LinesPerPage) == 0) {
      CurrentLine = 1;
      CurrentPage++;
      Status = EndPage(hPr);
      if (Status < 0) {
        break;
      } else {
        SelectObject(hPr, hfPr);
      }
    }
  }

  if (Status >= 0) {
    EndPage(hPr);
    EndDoc(hPr);
  }

  (void)SelectObject(hPr, hOldFont);
  DeleteObject(hfPr);
  DeleteDC(hPr);

}

#if defined(_UNICODE)
/*
 * UNICODE generator for tests.
 * Every call returns the next Japanese character that represents "fibonacci".
 * Bound to F12. When called with Shift-F12, reset to first character sequence.
 */
static EMCHAR
xpunicodegen() {
  static constexpr EMCHAR fib[] = {
    0x30D5,
    0x30A3,
    0x30DC,
    0x30CA,
    0x30C3,
    0x30C1,
    0x0020,
    0
  };
  static int pos = 0;

  if (fib[pos] == 0 || GetAsyncKeyState(VK_SHIFT)) {
    pos = 0; // rotate
  }

  return fib[pos++];
}
#endif

/*
 * Main entry point
 */

int WINAPI
_tWinMain(HINSTANCE hInstance,
          HINSTANCE hPInst,
          LPTSTR lpCmdLine,
          int nCmdShow) {
  auto share(true);

  if (hPInst) {
    MessageBox(GetFocus(),
               _T("Application is alredy loaded."),
               _T(""),
               MB_ICONHAND /* | MB_SYSTEMMODAL */);
    return 0;
  }

  XpTerminal::_inst     = hInstance;
  XpTerminal::_acctable = LoadAccelerators(hInstance, _T("EmACTAccelerator"));
  XpTerminal::_prev     = hPInst;
  XpTerminal::_cmdshow  = (int)nCmdShow;
  XpTerminal::_loadmsg  = RegisterWindowMessage(XP_EMACS_LOAD_MESSAGE);
  XpTerminal::_findmsg  = RegisterWindowMessage(FINDMSGSTRING);

  const TCHAR* argv[64];
  int          argc = 0;

#if     defined(_UNICODE)
  argv[argc++] = XP_EMACS_APPNAME;
  if (lpCmdLine && *lpCmdLine) {
    int     n;
    int     i;
    LPWSTR* s = CommandLineToArgvW((LPWSTR)lpCmdLine, &n);

    for (i = 0; i < n; ++i) {
      argv[argc] = (TCHAR *)s[i];
      if (_tcscmp(argv[argc], _T("-noshare")) == 0 ||
          _tcscmp(argv[argc], _T("-ns")     ) == 0) {
        share = false;
        continue;
      }
      ++argc;
    }
  }
  argv[argc] = (TCHAR *)0;
#else
  argv[argc++] = XP_EMACS_APPNAME;
  while (*lpCmdLine) {
    TCHAR   endstr;

    while (*lpCmdLine && *lpCmdLine == ' ') {
      lpCmdLine++;
    }

    if (*lpCmdLine == '\000') {
      break;
    }

    if (*lpCmdLine == '"') {
      ++lpCmdLine;
      endstr = '"';
    } else {
      endstr = ' ';
    }

    argv[argc] = (TCHAR *)lpCmdLine;

    while (*lpCmdLine && *lpCmdLine != endstr) {
      lpCmdLine++;
    }

    if (*lpCmdLine == endstr) {
      if (*lpCmdLine == '"') {
        *lpCmdLine = '\000';
      }
      *lpCmdLine++ = '\000';
    }

    if (_tcscmp(argv[argc], _T("-noshare")) == 0 ||
        _tcscmp(argv[argc], _T("-ns")     ) == 0) {
      share = false;
      argv[argc] = (TCHAR *)nullptr;
    } else {
      ++argc;
    }
  }

  argv[argc] = (TCHAR *)0;
#endif

  /*
   * Create a Mutex that check for multiple instances.
   */

  auto hMutex(CreateMutex(nullptr, FALSE, XP_EMACS_CLASS));

  if (GetLastError() == ERROR_ALREADY_EXISTS) {
    /*
     * Another EmACT is running. Find its Window.
     */
    XpTerminal::_wnd = FindWindow(XP_EMACS_CLASS, nullptr);
  }

  if (XpTerminal::_wnd != nullptr && share) {
    DWORD_PTR res;

    for (int i = 1; i < argc; ++i) {
      (void)_tfullpath(xpshared, argv[i], XP_EMACS_MAXPATH);

      SendMessageTimeout(XpTerminal::_wnd,
                         XpTerminal::_loadmsg,
                         IDM_LOAD,
                         (LPARAM)xpshared,
                         SMTO_BLOCK | SMTO_ABORTIFHUNG,
                         200, /* in ms */
                         &res);
    }

    /*
     * Connect to the running EmACT and exit.
     */

    if (IsIconic(XpTerminal::_wnd)) {
      ShowWindow(XpTerminal::_wnd, SW_RESTORE);
    }

    SetForegroundWindow(XpTerminal::_wnd);

    if (hMutex != 0) {
      ReleaseMutex(hMutex);
    }

    return 0;
  }

  /*
   * Enter emacs Wonderland
   */

  Editor emacs(argc, argv);
  emacs.engine();

  if (hMutex != 0) {
    ReleaseMutex(hMutex);
  }

  return 0;
}
