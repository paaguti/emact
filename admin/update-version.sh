#!/bin/sh
# $Id: update-version.sh,v 1.2 2008/04/08 11:32:01 jullien Exp $

EMACS_MAJOR=`grep EMACS_MAJOR src/version.h \
               | sed -e "s/.*\"\(.*\)\".*/\1/g"`
EMACS_MINOR=`grep EMACS_MINOR src/version.h \
               | sed -e "s/.*\"\(.*\)\".*/\1/g"`
EMACS_RELEASE=`grep EMACS_RELEASE src/version.h \
               | sed -e "s/.*\"\(.*\)\".*/\1/g"`
EMACS_VERSION=`grep EMACS_VERSION src/version.h \
               | sed -e "s/.*\"\(.*\)\".*/\1/g"`
EMACS_COPYRIGHT=`grep EMACS_COPYRIGHT src/version.h \
               | sed -e "s/.*\"\(.*\)\".*/\1/g"`

echo $EMACS_MAJOR $EMACS_MINOR $EMACS_RELEASE

update_makedisk()
{
  sed -e "s/set EMACS_MAJOR=.*/set EMACS_MAJOR=$EMACS_MAJOR/g" \
      -e "s/set EMACS_MINOR=.*/set EMACS_MINOR=$EMACS_MINOR/g" \
      -e "s/set EMACS_RELEASE=.*/set EMACS_RELEASE=$EMACS_RELEASE/g" \
      < makedisk.bat > makedisk.tmp \
      && mv makedisk.tmp makedisk.bat \
      && unix2dos makedisk.bat
}

update_diz()
{
  echo "Install EmACT v" $EMACS_VERSION "for Pocket PC" > src/pocket/emacs.diz
  echo $EMACS_COPYRIGHT >> src/pocket/emacs.diz
}

update_to()
{
  sed -e "s/set EMACS_VERSION=.*/set EMACS_VERSION=$EMACS_VERSION/g" \
      < to.bat > to.tmp \
      && mv to.tmp to.bat \
      && unix2dos to.bat
}

update_html()
{
  CURRENT=`grep " stable version is " ftp/index.html \
         | sed -e "s/.*stable version is \([0-9\.]*\).*/\1/g"`

  sed -e "s/$CURRENT/$EMACS_VERSION/g" \
      -e "s/\(.* Release date:\).*/\1 `date -u`)/g" \
      < ftp/index.html > index.tmp \
      && mv index.tmp ftp/index.html \
      && chmod a+x ftp/index.html
}

update_makedisk
update_diz
update_to
update_html
