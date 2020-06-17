/*
 * $Id: ./emacs.h,v 1.66 2018/09/09 07:25:14 jullien Exp $
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

#if !defined(__CHAR_TYPE_H)
#define __CHAR_TYPE_H

#include <cstddef>
#include <cstring>

#if defined(UNICODE)
#include <cwchar>
#include <cwctype>

using EMCHAR = wchar_t;

#define ECSTR(x)        (EMCHAR *)(L ## x)
#define EMEOF           WEOF

static constexpr size_t EMMB_LEN_MAX{4};   // as required by RFC-3629

#define emstrcat(s1, s2)          std::wcscat(s1, s2)
#define emstrcpy(s1, s2)          std::wcscpy(s1, s2)
#define emstrcmp(s1, s2)          std::wcscmp(s1, s2)
#define emstrncat(s1, s2, n)      std::wcsncat(s1, s2, n)
#define emstrncpy(s1, s2, n)      std::wcsncpy(s1, s2, n)
#define emstrncmp(s1, s2, n)      std::wcsncmp(s1, s2, n)
#define emstrpbrk(s1, s2)         std::wcspbrk(s1, s2)
#define emstrrchr(s1, c)          std::wcsrchr(s1, c)
#define emstrlwr(s)               wcslwr(s)
#define emfwide(fd, mode)         std::fwide(fd, mode)
#define emsprintf(buf, fmt, ...)  std::swprintf(buf,sizeof(buf),fmt,__VA_ARGS__)
#define emstrlen(s)               (int)std::wcslen(s)
#define emstrtoi(s)               (int)std::wcstol(s, nullptr, 0)
#else   /* _WIDECHARS */
using EMCHAR = char;

#define ECSTR(x)        (EMCHAR*)x
#define EMEOF           EOF

static constexpr size_t EMMB_LEN_MAX{4};   // as required by RFC-3629

#define emstrcat(s1, s2)          std::strcat((char*)s1, (char*)s2)
#define emstrcpy(s1, s2)          std::strcpy((char*)s1, (char*)s2)
#define emstrcmp(s1, s2)          std::strcmp((char*)s1, (char*)s2)
#define emstrncat(s1, s2, n)      std::strncat((char*)s1, (char*)s2, n)
#define emstrncpy(s1, s2, n)      std::strncpy((char*)s1, (char*)s2, n)
#define emstrncmp(s1, s2, n)      std::strncmp((char*)s1, (char*)s2, n)
#define emstrpbrk(s1, s2)         std::strpbrk((char*)s1, (char*)s2)
#define emstrrchr(s1, c)          std::strrchr((char*)s1, c)
#define emstrlwr(s)               strlwr((char*)s)
#define emsprintf(buf, fmt, ...)  std::snprintf(buf,sizeof(buf),fmt,__VA_ARGS__)
#define emstrlen(s)               (int)std::strlen((char*)s)
#define emstrtoi(s)               (int)std::strtol((char*)s, nullptr, 0)
#endif  /* _WIDECHARS */
#endif  /* __CHAR_TYPE_H */
