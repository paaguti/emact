#!/bin/sh
#
# Copyright (c) 1992-2018, Eligis
#

fail() {
    echo "Error:" $@ 1>&2; exit 1
}

aclocal -I m4 --force || fail "aclocal"
autoconf -f || fail "autoconf"
autoheader || fail "autoheader"
# sed -i -e 's/.*AX_REQUIRE_DEFINED.*//g' configure
