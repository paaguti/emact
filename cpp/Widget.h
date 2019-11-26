/*
 * static auto rcsid("$Id: ./emacs.h,v 1.66 2018/09/09 07:25:14 jullien Exp $");
 */

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

#if !defined(__WIDGET_H)
#define __WIDGET_H
#include "./emacs.h"
/**
 * Widget class.
 */
class Widget {
 public:
  /*
   * Y/N Widget
   */
  CMD (*w_yn)(const EMCHAR* s);
  /*
   * YES/NO Widget
   */
  CMD (*w_yesno)(const EMCHAR* s);
  /*
   * CONFIRM Widge
   */
  CMD (*w_confirm)(const EMCHAR* s);
  /*
   * ERROR Widget
   */
  void (*w_error)(const EMCHAR* s);
  /*
   * TITLE Widget
   */
  EMCHAR* (*w_title)(EMCHAR* b, EMCHAR* f);
  /*
   * ASKER Widget
   */
  CMD (*w_asker)(const EMCHAR* s, EMCHAR* b, int n);
  /*
   * EDITOR Widget
   */
  CMD (*w_edit)(const EMCHAR* s, EMCHAR* b, int n);
  /*
   * CHANGE Widget
   */
  CMD (*w_change)(const EMCHAR* msg, EMCHAR* op, EMCHAR* np, int n);
  /*
   * PLAY Widget
   */
  void (*w_play)(int flag);
  /*
   * WAIT Widget
   */
  void (*w_wait)();
  /*
   * MSG Widget
   */
  void (*w_message)(const EMCHAR* str);
  /*
   * WRITE Widget
   */
  void (*w_write)(const EMCHAR *fmt, ...);
  /*
   * ADJUST Widget
   */
  void (*w_adjust)();
  /*
   * UPDATE Widget
   */
  void (*w_update)(const EMCHAR* p, EMCHAR* b);
  /*
   * CBCOPY Widget
   */
  void (*w_clipcopy)();
  /*
   * CBPAST Widget
   */
  void (*w_clippaste)();
  /*
   * print buffer
   */
  void (*w_print)();
};
#endif /* __WIDGET_H */
