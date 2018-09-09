#!/usr/local/ibin/perl -w -i.BAK

# Usage: nospace [files]
# Suppress spaces, tabs ... at the end of the lines.

# This is a sample Perl script to be used inside EmACT

while (<>) {
    s/\s+$/\n/;
    print;
}
