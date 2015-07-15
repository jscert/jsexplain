#!/usr/bin/perl -p
# Converts ml char-array constructions into strings
# eg: ('a'::('b'::('c'::[]))) => "abc"
#
# Makes use of all the horrific Perl regular expression features I could find:
# (?R) is the recursive subexpression pattern -- it recursively reuses the entire pattern as a sub-pattern.
# However, this doesn't support retrieving the nested pattern captures, so we use
# (?{ }) to execute the Perl expression $^R.($^N =~ s{^"}{\\"}r) which concatenates the last captured group (with "
# substituted for \") onto the result of the previous execution of this expression.
# $^R isn't cleared between regular expression matches, so we do this manually.
# The -p operator to Perl wraps this script in a foreach loop that operates over a given file.

$^R = "";
s/'([^']|\\(?:\\|"|'|n|r|t|b| |\d{3}|x[0-9a-fA-F]{2}))(?{ $^R.($^N =~ s{^"}{\\"}r) })'::(?:\((?R)\)|\[\])/"$^R"/;
