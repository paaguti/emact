#!/bin/sh
# $Id: tag.sh,v 1.3 2010-06-26 04:51:52 jullien Exp $

EMACS_VERSION=`grep EMACS_VERSION src/version.h \
               | sed -e "s/.*\"\(.*\)\".*/\1/g"`

TAGNAME=`echo $EMACS_VERSION | sed -e "s/\./-/g"`

# echo $TAGNAME && exit 0

echo -n "Tag with \"cvs tag -c v$TAGNAME\" command [YES/no] ?"; read n

case "$n" in
  "yes"|"")
    cvs tag -c v$TAGNAME
    ;;
  *)
    echo trunc not tagged.
    ;;
esac
