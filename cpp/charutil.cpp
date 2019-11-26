#if     !defined(lint)
static auto rcsid("$Id: charutil.cpp,v 1.7 2018/09/07 17:57:09 jullien Exp $");
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
 * charutil.cpp :
 *
 * It implements low level character functions.
 */

#include "./CharType.h"

/*
 * Convert a wide character to the corresponding multibyte character.
 *
 * Parameters
 *  mbchar 
 *         The address of a multibyte character.
 *  wchar 
 *         A wide character.
 *
 * Return Value 
 *  If wctomb converts the wide character to a multibyte character, it
 *  returns the number of bytes (which is never greater than
 *  MB_LEN_MAX) in the wide character.  If wchar is the wide-character
 *  null character (L'\0'), wctomb returns 1.  If the target pointer
 *  mbchar is nullptr, wctomb returns 0.  If the conversion is not
 *  possible wctomb returns -1.
 */

int
emwctomb(char* mbchar, EMCHAR wchar) {
  int ulen = 0;        /* return value for UTF8 size */
  size_t uChar = static_cast<size_t>(wchar);

  if (mbchar == nullptr) {
    return ulen;
  }

  if (uChar <= 0x7F) {
    /* ASCII chars no conversion needed */
    *mbchar = (char)uChar;
    ulen    = 1;
  } else if (uChar <= 0x07FF) {
    /* In the 2 byte utf-8 range */
    *mbchar++ = (char)(0xC0 + (uChar / 0x40));
    *mbchar   = (char)(0x80 + (uChar % 0x40));
    ulen      = 2;
  } else if (uChar <= 0xFFFF) {
    /*
     * In  the  3  byte  utf-8 range.  The values
     * 0x0000FFFE,  0x0000FFFF  and  0x0000D800 -
     * 0x0000DFFF do not occur in UCS-4
     */
    *mbchar++ = (char)(0xE0 + (uChar / 0x1000));
    *mbchar++ = (char)(0x80 + ((uChar / 0x40) % 0x40));
    *mbchar   = (char)(0x80 + (uChar % 0x40));
    ulen      = 3;
  } else if (uChar <= 0x1FFFFF) {
    /* In the 4 byte utf-8 range */
    *mbchar++ = (char)(0xF0 + (uChar / 0x040000));
    *mbchar++ = (char)(0x80 + ((uChar / 0x10000) % 0x40));
    *mbchar++ = (char)(0x80 + ((uChar / 0x40) % 0x40));
    *mbchar   = (char)(0x80 + (uChar % 0x40));
    ulen      = 4;
#if (EMMB_LEN_MAX > 4)
  } else if (uChar <= 0x03FFFFFF) {
    /* In the 5 byte utf-8 range */
    *mbchar++ = (char)(0xF8 + (uChar / 0x01000000));
    *mbchar++ = (char)(0x80 + ((uChar / 0x040000) % 0x40));
    *mbchar++ = (char)(0x80 + ((uChar / 0x1000) % 0x40));
    *mbchar++ = (char)(0x80 + ((uChar / 0x40) % 0x40));
    *mbchar   = (char)(0x80 + (uChar % 0x40));
    ulen      = 5;
#endif
#if (EMMB_LEN_MAX > 5)
  } else if (uChar <= 0x7FFFFFFF) {
    /* In the 6 byte utf-8 range */
    *mbchar++ = (char)(0xF8 + (uChar / 0x40000000));
    *mbchar++ = (char)(0x80 + ((uChar / 0x01000000) % 0x40));
    *mbchar++ = (char)(0x80 + ((uChar / 0x040000) % 0x40));
    *mbchar++ = (char)(0x80 + ((uChar / 0x1000) % 0x40));
    *mbchar++ = (char)(0x80 + ((uChar / 0x40) % 0x40));
    *mbchar   = (char)(0x80 + (uChar % 0x40));
    ulen      = 6;
#endif
  } else {
    return -1;
  }

  return ulen;
}

/*
 * Convert a multibyte character to a corresponding wide character.
 *
 * Parameters
 *  wchar 
 *         Address of a wide character (type EMCHAR).
 *  mbchar
 *         Address of a sequence of bytes (a multibyte character).
 *  count 
 *         Number of bytes to check.
 *
 * Return Value 
 *  If mbchar is not nullptr and if the object that mbchar points to
 *  forms a valid multibyte character, mbtowc returns the length in
 *  bytes of the multibyte character.  If mbchar is nullptr or the
 *  object that it points to is a wide-character null character
 *  (L'\0'), the function returns 0.  If the object that mbchar points
 *  to does not form a valid multibyte character within the first
 *  count characters, it returns -1.
 */

int
emmbtowc(EMCHAR* wchar, const char* mbchar, size_t count) {
  int ulen = 0;
  unsigned int code;
  unsigned char* mbc = (unsigned char*)mbchar;

  if (mbc == nullptr || *mbc == '\000') {
    return ulen;
  }

  if ((*mbc & 0x80) == 0x00) {
    /* ASCII chars no conversion needed */
    if (count < 1) {
      return 0;
    }
    code = (unsigned int)*mbc;
    ulen = 1;
  } else if ((*mbc & 0xE0) == 0xC0) {
    /* In the 2 byte utf-8 range */
    if (count < 2) {
      return 0;
    }
    code = (unsigned int)(((*mbc - 0xC0) * 0x40)
                          + (*(mbc + 1) - 0x80));
    ulen = 2;
  } else if ((*mbc & 0xF0) == 0xE0) {
    /* In the 3 byte utf-8 range */
    if (count < 3) {
      return 0;
    }
    code = (unsigned int)(((*mbc - 0xE0) * 0x1000)
                          + ((*(mbc + 1) - 0x80) * 0x40)
                          + (*(mbc + 2) - 0x80));
    ulen = 3;
  } else if ((*mbc & 0xF8) == 0xF0) {
    /* In the 4 byte utf-8 range */
    if (count < 4) {
      return 0;
    }
    code = (unsigned int)(((*mbc - 0xF0) * 0x40000)
                          + ((*(mbc + 1) - 0x80) * 0x1000)
                          + ((*(mbc + 2) - 0x80) * 0x40)
                          + (*(mbc + 3) - 0x80));
    ulen = 4;
#if (EMMB_LEN_MAX > 4)
  } else if ((*mbc & 0xFC) == 0xF8) {
    /* In the 5 byte utf-8 range */
    if (count < 5) {
      return 0;
    }
    code = (unsigned int)(((*mbc - 0xF8) * 0x1000000)
                          + ((*(mbc + 1) - 0x80) * 0x40000)
                          + ((*(mbc + 2) - 0x80) * 0x1000)
                          + ((*(mbc + 3) - 0x80) * 0x40)
                          + (*(mbc + 4) - 0x80));
    ulen = 5;
#endif
#if (EMMB_LEN_MAX > 5)
  } else if ((*mbc & 0xFE) == 0xFC) {
    /* In the 6 byte utf-8 range */
    if (count < 6) {
      return 0;
    }
    code = (unsigned int)(((*mbc - 0xFC) * 0x40000000)
                          + ((*(mbc + 1) - 0x80) * 0x1000000)
                          + ((*(mbc + 2) - 0x80) * 0x40000)
                          + ((*(mbc + 3) - 0x80) * 0x1000)
                          + ((*(mbc + 4) - 0x80) * 0x40)
                          + (*(mbc + 5) - 0x80));
    ulen = 6;
#endif
  } else {
    code = 0;
    ulen = -1;
  }

  *wchar = (EMCHAR)code;

  return ulen;
}

/*
 * Converts a sequence of multibyte characters to a corresponding sequence of
 * wide characters.
 *
 * Parameters
 *  wcstr
 *         The address of a sequence of wide characters.
 *  mbstr
 *         The address of a sequence of null terminated multibyte characters. 
 *  count 
 *         The maximum number of multibyte characters to convert.
 *
 * Return Value
 *  If emmbstowcs successfully converts the source string, it returns
 *  the number of converted multibyte characters.  If the wcstr
 *  argument is nullptr, the function returns the required size of the
 *  destination string.  If emmbstowcs encounters an invalid multibyte
 *  character, it returns -1.  If the return value is count, the
 *  wide-character string is not null-terminated.
 */

int
emmbstowcs(EMCHAR* wcstr, const char* mbstr, size_t count) {
  size_t i;

  for (i = 0; i < count; ++i) {
    EMCHAR c = (EMCHAR)'?';

    if (*mbstr == '\000') {
      wcstr[i] = (EMCHAR)'\000';
      break;
    }

    auto len = emmbtowc(&c, mbstr, 6);

    if (len == -1) {
      return -1;
    }

    if (wcstr != nullptr) {
      wcstr[i] = c;
    }

    mbstr += len;
  }

  return static_cast<int>(i);
}

/*
 * Converts a sequence of wide characters to a corresponding sequence
 * of multibyte characters.
 *
 * Parameters
 *  mbstr 
 *         The address of a sequence of multibyte characters. 
 *  wcstr 
 *         The address of a sequence of wide characters. 
 *  count 
 *         The maximum number of bytes that can be stored in the
 *         multibyte output string. 
 *
 * Return Value
 *  If  emwcstombs  successfully  converts  the multibyte string,  it
 *  returns  the  number  of  bytes written into the multibyte output
 *  string,  excluding  the  terminating NUL (if any).  If the  mbstr
 *  argument  is  nullptr,  emwcstombs  returns the required size of the
 *  destination string.  If emwcstombs encounters a wide character it
 *  cannot  be  convert to a multibyte character,  it returns -1 cast
 *  to type size_t.
 * 
 */

size_t
emwcstombs(char* mbstr, EMCHAR* wcstr, size_t count) {
  size_t len = 0;

  for (auto i(0); wcstr[i]; ++i) {
    char wbuf[EMMB_LEN_MAX + 1];
    int  conv;

    if ((conv = emwctomb(wbuf, wcstr[i])) > 0) {
      if (mbstr == nullptr) {
        /*
         * Compute required size, do not store result.
         */
        len += conv;
      } else if ((len + conv) >= count) {
        /*
         * Can't fit in buffer.
         */
        return len;
      } else {
        for (auto j(0); j < conv; ++j) {
          mbstr[len++] = wbuf[j];
        }
      }
    } else {
      /*
       * Cannot convert wcstr[i]
       */
      return (size_t)-1;
    }
  }

  if (mbstr != nullptr) {
    if (len < count) {
      /*
       * Array is null-terminated only when len < count.
       */
      mbstr[len] = '\000';
    }
  }

  return len;
}

#if     0
#define SURROGATE_HIGH_START    0xD800
#define SURROGATE_HIGH_END      0xDBFF
#define SURROGATE_LOW_START     0xDC00
#define SURROGATE_LOW_END       0xDFFF
#endif

/*
 * Retuns the number of multiple bytes needed to store MB character c.
 */

static const unsigned char embytesForUTF8[256] = {
  /* ASCII 7bit char         -> 0xxxxxxx */
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, /* 00 */
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, /* 10 */
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, /* 20 */
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, /* 30 */
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, /* 40 */
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, /* 50 */
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, /* 60 */
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, /* 70 */
  /* invalid UTF-8 char      -> 10xxxxxx */
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* 80 */
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* 90 */
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* A0 */
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* B0 */
  /* (c & 0xE0) == 0xC0      -> 110xxxxx */
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, /* C0 */
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, /* D0 */
  /* (c & 0xF0) == 0xE0      -> 1110xxxx */
  3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, /* E0 */
  /* (c & 0xF8) == 0xF0      -> 11110xxx */
#if (EMMB_LEN_MAX == 4)
  4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0  /* F0 */
#else
  4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 0, 0  /* F0 */
#endif
};

int
emmbclen(int c) {
  return (int)embytesForUTF8[c & 0xFF];
}
